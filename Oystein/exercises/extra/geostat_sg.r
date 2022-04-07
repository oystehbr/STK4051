#Geostatistical model
set.seed(345)
mu=1
sigma = 1
tau = 1
phi=0.25
n = 1000
s = cbind(runif(n),runif(n))
d = as.matrix(dist(s,diag=TRUE,upper=TRUE))
Sigma = sigma^2*exp(-d/phi)+tau^2*diag(1,n)
Sigma.chol = chol(Sigma)
y = mu + rnorm(n)%*%Sigma.chol



logl = function(y,d,mu,tau2,sig2,phi)
{
  n = length(y)
  Sigma = sig2*exp(-d/phi)+tau2*diag(1,n)
#  res = matrix(y-mu,ncol=1)
#  show(c(mu,sig2,tau2,phi))
#  show(class(Sigma))
#  l = -0.5*n*log(2*pi)-0.5*determinant(Sigma,log=TRUE)$mod-0.5*sum(res*solve(Sigma,res))
  d = dmvnorm(y,rep(mu,n),Sigma,log=TRUE)
#  show(c(d,l))
  d
}
tr = function(M)
{
  sum(diag(M))
}

mu.hat=0
sig2.hat = 0.5
tau2.hat = 2
phi.hat=1
#mu.hat = fit0.geoR$beta
#sig2.hat = fit0.geoR$cov.pars[1]
#tau2.hat = fit0.geoR$nugget
#phi.hat = fit0.geoR$cov.pars[2]
l = logl(y,d,mu.hat,tau2.hat,sig2.hat,phi.hat)
show(c(l,mu.hat,tau2.hat,sig2.hat,phi.hat))
m = 10
lvec = l
itvec = 0
for(it in 1:50000)
{
  ind = sample(1:n,m,replace=FALSE)
  y.k = y[ind]
  s.k = s[ind,]
  d.k = as.matrix(dist(s.k,diag=TRUE,upper=TRUE))
  R.k = exp(-d.k/phi.hat)
  Sigma.k = sig2.hat*R.k+tau2.hat*diag(1,m)
  dR.k = d.k*R.k/phi.hat^2
  res = y.k-mu.hat
  res2 = solve(Sigma.k,res)
  res3 = R.k%*%res2
  res4 = dR.k%*%res2
  H.mu = sum(res2)
  H.sig2 = -0.5*tr(solve(Sigma.k,R.k))+0.5*sum(res2*res3)
  H.tau2 = -0.5*tr(solve(Sigma.k))+0.5*sum(res2*res2)
  H.phi = -0.5*tr(solve(Sigma.k,sig2.hat*dR.k))+0.5*sig2.hat*sum(res2*res4)
  alpha = 10/(m*(1+it))
  mu.hat = mu.hat + alpha*H.mu
  a = log(sig2.hat) + alpha*H.sig2/sig2.hat
  sig2.hat = exp(a)
  a = log(tau2.hat) + alpha*H.tau2/tau2.hat
  tau2.hat = exp(a)
  a = log(phi.hat) + alpha*H.phi/phi.hat
  phi.hat = exp(a)
  if(it%%100==0)
  {
   #show(c(H.mu,H.sig2,H.tau2,H.phi))
   l = logl(y,d,mu.hat,tau2.hat,sig2.hat,phi.hat)
   lvec = c(lvec,l)
   itvec = c(itvec,it)
   show(c(it,l,mu.hat,tau2.hat,sig2.hat,phi.hat))
  }
}
plot(itvec,lvec,type="l")

#Estimates from geoR, a package that do the fitting directly
#Note: This will be extremely slow if n is large!!!
if(0)        #change to 1 if you want to run it
{
 library(geoR)
 d.geoR <- as.geodata(cbind(matrix(y,ncol=1),s),coords.col=2:3,data.col=1)
 fit0.geoR <- likfit(d.geoR,cov.model="exponential",ini.cov.pars=c(0.5,0.5))
 show(fit0.geoR)
}
