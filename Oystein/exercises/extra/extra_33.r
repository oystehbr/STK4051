#Extra 33
Mvec=c(1000,10000,30000,100000)
m=100
a=2
par(mfrow=c(2,2),mar=c(2,2,2,2))

# we want to sample from the standard normal distribution to find good samples from switched
x = seq(-5, 5, length=1000)
plot(x, dnorm(x), type = "l", col = 1)
lines(x, dnorm(x, a), col = 2)
dev.off()

par(mfrow=c(2,2),mar=c(2,2,2,2))


for(i in 1:length(Mvec))
{
  M = Mvec[i]
  x = rnorm(M)
  w = dnorm(x,a)/dnorm(x)
  q = w/sum(w)
  y = sample(x,m,prob=q,replace=T)
  d = density(y)
  qqnorm(y,main=paste("M=",M));qqline(y,col=2)
#  matplot(cbind(d$x,d$x),cbind(dnorm(d$x,a),d$y),type="l",lty=1,col=1:2)
  cat("M=",M,"\n")
  show("Mean (actual - computed)")
  show(c(a,mean(y)))
  show("SD (actual - computed)")
  show(c(1,sd(y)))
  #Effective sample size
  cat("N_eff=",1/sum(q^2),"\n")
  #Measure of (in)dependence: Number of unique y's
  cat("Number of unique y's=",length(unique(y)),"\n")
}

