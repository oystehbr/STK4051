#Extra_36.R
m = 10
N = 4000
X = matrix(nrow=N,ncol=m)
X[1,] = rnorm(m,0,5)
a = 5
acc = 0
for(i in 2:N)
{
  eps = runif(m,-a,a)
  R = dnorm(X[i-1,]+eps)/dnorm(X[i-1,])
  I = as.numeric(runif(m)<R)
  X[i,] = X[i-1,]+I*eps
  acc = acc + sum(I)
}
acc = acc/((N-1)*m)
cat("Acceptance rate=",acc,"\n")
par(mfrow=c(1,1),mar=rep(2,4))
plot.ts(X,plot.type="single",lty=1,col=1:m)

D = 1000    #Play around with different values on D
par(mfrow=c(2,1),mar=rep(2,4))
means = diag(1/c(1:(N-D)))%*%apply(X[-c(1:D),],2,cumsum)
plot.ts(means,plot.type="single",lty=1,col=1:m,ylim=c(-1,1))
abline(h=0)
means2 = diag(1/c(1:(N-D)))%*%apply(X[-c(1:D),]^2,2,cumsum)
vars = means2-means^2
plot.ts(vars,plot.type="single",lty=1,col=1:m,ylim=c(0,5))
abline(h=1)
