#  Plots of noncentral F pdfs for a variety of sample sizes, for a linear model
#  hypothesis test with sample size n, r parameters in the alternative hypothesis
#  mean, r-q parameters in the null hypothesis mean.
#  Numerator df: q = 6.  Denominator df:  n - r = 24 - 12 = 12.
#  Noncentrality parameter:  lambda = n*delta^2, where delta = .25.
#  Dashed curve is central F with lambda = 0.  

par(lwd=2)
r=12
q=6
u=seq(.01,10,.01)

n=24
lambda=0
pdf=df(u,q,n-r,lambda)
plot(u,pdf,type="l",xlab="u",
   ylab="probability density",lty=2,cex.lab=1.5,bty="l")

n=24
lambda=n*.25
pdf=df(u,q,n-r,lambda)
points(u,pdf,type="l",lty=1)

n=36
lambda=n*.25
pdf=df(u,q,n-r,lambda)
points(u,pdf,type="l",lty=1)

n=48
lambda=n*.25
pdf=df(u,q,n-r,lambda)
points(u,pdf,type="l",lty=1)

n=60
lambda=n*.25
pdf=df(u,q,n-r,lambda)
points(u,pdf,type="l",lty=1)

abline(v=1.5,lty=3)

text(.7,.7,labels=expression(lambda==0),pos=4,cex=1.5)
text(1.5,.37,labels=expression(lambda==6),pos=4,cex=1.5)
text(2,.33,labels=expression(lambda==9),pos=4,cex=1.5)
text(2.6,.3,labels=expression(lambda==12),pos=4,cex=1.5)
text(4.2,.2,labels=expression(lambda==15),pos=4,cex=1.5)
