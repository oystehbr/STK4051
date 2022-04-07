#Extra 27: Linear state + poisson data
#Illustration of Gaussian approximation
mu=2;sigma=0.5;a=0.9
sig1 = sigma/sqrt(1-a^2)  # sigma from exercise 26

# testing the correct vs. approximation distribution
y=5
x = seq(-1,3,length=1000)
delta = x[2]-x[1]

# finding p(x|y)
p = dnorm(x,mu,sig1)*dpois(y,exp(x))
konst = 1 / sum(p*delta)  # TODO: what kind of constant is this
p = konst*p

# Creating the (approximation) distribution from 27c
tilde.mu = ((1/sig1^2+exp(mu))*mu+y-exp(mu))/(1/sig1^2+exp(mu))
tilde.sigma = sig1/sqrt(1+sig1^2*exp(mu)) 

# plotting the actual, and approximation
matplot(cbind(x,x),cbind(p,dnorm(x,tilde.mu,tilde.sigma)),
        type="l",lty=1,col=1:2,xlab="x",ylab="density")

# Density from exercise 26
lines(x,dnorm(x,mu,sig1),col=4)


#Numerical optimization, checking the negative likelihood of the p(x | y) - function
minuslogp = function(x,mu,sigma,y)
{
  l = -dnorm(x,mu,sigma,log=TRUE)-dpois(y,exp(x),log=TRUE)
  l
}

mu1 = mu
res = nlm(minuslogp,2,mu=mu1,sigma=sig1,y=5,hessian=TRUE)
mu.opt=res$estimate
sig.opt = 1/sqrt(res$hessian)
lines(x,dnorm(x,mu.opt,sig.opt),lty=1,col=3)
legend("topleft",c("True","Approximation1","Approximation2"),
       lty=1,col=1:3)

y = scan("data/sim_animal_trap.txt")
nT = length(y)
N=100000
mu = 2;a=0.9;sigma=0.5;tau=1/sigma^2


x = matrix(nrow=nT,ncol=N)
x.hat = matrix(nrow=nT,ncol=3)
N.eff2 = rep(NA,nT)

#Initialization
#Proposal distribution
sig1 = sigma/sqrt(1-a^2)
tau1 = 1/sig1^2

tilde.mu = ((tau1+exp(mu))*mu+y[1]-exp(mu))/(tau1+exp(mu))
tilde.sigma = sig1/sqrt(1+sig1^2*exp(mu)) 
x[1,] = rnorm(N,tilde.mu,tilde.sigma)

# Find the logarithm of the weights
logw = dnorm(x[1,],mu,sigma,log=TRUE)+
       dpois(y[1],exp(x[1,]),log=TRUE)-
       dnorm(x[1,],tilde.mu,tilde.sigma,log=TRUE)

# Normalizing the weights (in log-form - substract the maximum)
w = exp(logw-max(logw)) # why normalize here and afterwards?

# WHY NOT THIS??
w = (dnorm(x[1,],mu,sigma) * dpois(y[1],exp(x[1,])))/ 
  dnorm(x[1,],tilde.mu,tilde.sigma)
w = w/sum(w)

N.eff2[1] = 1/sum(w^2)

#Resample
ind = sample(1:N,N,replace=T,prob=w)
x[1,] = x[1,ind]
w = rep(1/N,N)
x.hat[1,1] = mean(x[1,])
x.hat[1,2:3] = quantile(x[1,],c(0.025,0.975))

for(i in 2:nT)
{
  #Proposal distribution
  mu.i = mu+a*(x[i-1,]-mu)
  tilde.mu = ((tau+exp(mu.i))*mu.i+y[i]-exp(mu.i))/(tau+exp(mu.i))
  tilde.sigma = sigma/sqrt(1+sigma^2*exp(mu.i)) 
  print(c(i,y[i]))
  print(range(tilde.mu))
  print(range(tilde.sigma))
  x[i,] = rnorm(N,tilde.mu,tilde.sigma)
  
  # TODO: the same here
  logw = log(w)+
    dnorm(x[i,],mu.i,sigma,log=TRUE)+
    dpois(y[i],exp(x[i,]),log=TRUE)-
    dnorm(x[i,],tilde.mu,tilde.sigma,log=TRUE)
  w = exp(logw-max(logw))
  # ----
  
  w = w/sum(w)
  N.eff2[i] = 1/sum(w^2)
  
  #Resample
  ind = sample(1:N,N,replace=T,prob=w)
  x[1:i,] = x[1:i,ind]
  w= rep(1/N,N)
  x.hat[i,1] = mean(x[i,])
  x.hat[i,2:3] = quantile(x[i,],c(0.025,0.975))  

}

matplot(cbind(1:nT,1:nT,1:nT),x.hat,type="l",lty=c(1,2,2),col=1,xlab="time")

x.smo = matrix(nrow=nT,ncol=3)
N.unique2 = rep(NA,nT)
for(i in 1:nT)
{
  x.smo[i,1] = mean(x[i,])
  x.smo[i,2:3] = quantile(x[i,],c(0.025,0.975))  
  N.unique2[i] = length(unique(x[i,]))
}
matlines(cbind(1:nT,1:nT,1:nT),x.smo,type="l",lty=c(1,2,2),col=3)

par(mfrow=c(2,1),mar=rep(2,4))
matplot(cbind(N.eff,N.eff2),type="l",xlab="time",ylab="N.eff")
matplot(cbind(N.unique,N.unique2),type="l",xlab="Time",ylab="N.unique")

