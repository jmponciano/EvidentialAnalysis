#===========================================================
#  R script to calculate various quantities in evidential analysis
#  for the citrus tree data in Table 1.  The data are entered in
#  primitive fashion here to illustrate how the matrix calculations
#  are set up.
#
# Mean vector is parameterized as
# X%*%B = X1%*%B1 + X2%*%B2,
# where X is full rank design matrix.
# Null model has B2 = 0.
#===========================================================

# Desired value of delta (generalized per-observation
# departure of model 2 from model 1)
delta=.5

# Model characteristics
n=24
q=6
r=12

# Response variable onservations
y=c(49,39,50,55,43,38,53,48,55,41,67,58,
    53,42,85,73,66,68,85,92,69,62,85,99)

# Columns of design matrix X = [X1 | X2]
x0=rep(1,n)
x1=c(rep(1,8),rep(0,n-8))
x2=c(rep(0,8),rep(1,8),rep(0,8))
x3=rep(c(rep(1,2),rep(0,6)),3)
x4=rep(c(rep(0,2),rep(1,2),rep(0,4)),3)
x5=rep(c(rep(0,4),rep(1,2),rep(0,2)),3)
X1=cbind(x0,x1,x2,x3,x4,x5)
X1
x6=x1*x3
x7=x1*x4
x8=x1*x5
x9=x2*x3
x10=x2*x4
x11=x2*x5
X=cbind(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
X

X2=X[,(r-q+1):r]
X2

# ML estimates for model 1 (null)
B1=solve(t(X1)%*%X1,t(X1)%*%y)
sshat1=t(y-X1%*%B1)%*%(y-X1%*%B1)/n

#ML estimates for model 2 (alternative)
B=solve(t(X)%*%X,t(X)%*%y)
sshat=t(y-X%*%B)%*%(y-X%*%B)/n
sshat.unb=n*sshat/(n-r)   # unbiased

B1
B

sshat1
sshat
sshat.unb

# F statistic and p value
F=(n-r)*(sshat1-sshat)/(q*sshat)
F
p=1-pf(q,n-r,0)
p

# Additional parameters in model 2
B2=B[(r-q+1):r,]
B2

DSIC=n*log(1+q*F/(n-r))-q*log(n)
DSIC

# Calculate lambda (noncentrality parameter)
lambda=n*delta^2
lambda

# Largest post-data probability of evidence for model 1 as
# extreme as observed if data came from model 2
P2 = pf(F,q,n-r,lambda)
P2

# Largest post-data probability of evidence for model 2 as
# extreme as observed if data came from model 1
P1 = 1-pf(F,q,n-r,lambda)
P1

# Noncentral F distribution quantiles
psi1=qf(.05,q,n-r,lambda)
psi1
psi2=qf(.95,q,n-r,lambda)
psi2

# Evidence thresholds (k values) for delta SIC
k1=n*log(1+q*psi1/(n-r))-q*log(n)
k2=n*log(1+q*psi2/(n-r))-q*log(n)
k1
k2
