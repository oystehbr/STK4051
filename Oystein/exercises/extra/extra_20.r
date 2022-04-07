#Extra 20
n = 100

a=0.5
sig2=0.1

sig=sqrt(sig2)
z = seq(0,2,by=0.5)
mu.sim = matrix(nrow=length(z),ncol=100)
sig2.sim = matrix(nrow=length(z),ncol=100)
N.sim = matrix(nrow=length(z),ncol=100)
x.sim=rep(NA,n)
for(i in 1:length(z))
{
  show(z[i])
for(j in 1:100)
{
  N = 0
  for(k in 1:n)
  {
    more = TRUE
    while(more)
    {
      N = N+1
      x = rnorm(1)
      p = exp(-0.5*(z[i]-a*x)^2/sig2)
      if(runif(1)<p)
        more = FALSE
    }
    x.sim[k] = x
  }
  mu.sim[i,j] = mean(x.sim)
  sig2.sim[i,j] = var(x.sim)
  N.sim[i,j] = N
}
}

#hva er de greiene under herforno a?
mu.hat.z = colMeans(t(mu.sim))
mu.z = a*z/(a^2+sig2)
plot(mu.z,mu.hat.z);abline(c(0,1))
sig2.hat.z = colMeans(t(sig2.sim))
sig2.z = sig2/(a^2+sig2)
show(sig2.z)
show(sig2.hat.z)
show(round(colMeans(t(N.sim))))
show(round(n/(exp(-0.5*z^2/(a^2+sig2))/sqrt((a^2+sig2)/sig2))))

