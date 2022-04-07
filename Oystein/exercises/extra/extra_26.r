#Extra 26: Linear state + poisson data
t = scan("data/sim_animal_trap.txt")
nT = length(t)
N=100000

# initializing some values
mu = 2;a=0.9;sigma=0.5

# matrix for storing values
x = matrix(nrow=nT,ncol=N)
x.hat = matrix(nrow=nT,ncol=3)
N.eff = rep(NA,nT)

#Initialization
x[1,] = rnorm(N,mu,sigma/sqrt(1-a^2))
y = dpois(t[1],exp(x[1,]))
y = y/sum(y)
N.eff[1] = 1/sum(y^2)

#Resample
ind = sample(1:N,N,replace=T,prob=y)
x[1,] = x[1,ind]
y = rep(1/N,N)
x.hat[1,1] = mean(x[1,])
x.hat[1,2:3] = quantile(x[1,],c(0.025,0.975))

for(i in 2:nT)
{
  x[i,] = rnorm(N,mu+a*(x[i-1,]-mu),sigma)
  y = y*dpois(t[i],exp(x[i,]))
  y = y/sum(y)
  N.eff[i] = 1/sum(y^2)
  
  #Resample
  ind = sample(1:N,N,replace=T,prob=y)
  x[1:i,] = x[1:i,ind]
  y= rep(1/N,N)
  x.hat[i,1] = mean(x[i,])
  x.hat[i,2:3] = quantile(x[i,],c(0.025,0.975))  
}

matplot(1:length(N.eff),N.eff,type="l",lty=c(1),col=1,xlab="time")
matplot(cbind(1:nT,1:nT,1:nT),x.hat,type="l",lty=c(1,2,2),col=1,xlab="time")

x.smo = matrix(nrow=nT,ncol=3)
N.unique = rep(NA,nT)
for(i in 1:nT)
{
  x.smo[i,1] = mean(x[i,])
  x.smo[i,2:3] = quantile(x[i,],c(0.025,0.975))  
  cat("Unique",i,length(unique(x[i,])),"\n")
  N.unique[i] = length(unique(x[i,]))
}
matlines(cbind(1:nT,1:nT,1:nT),x.smo,type="l",lty=c(1,2,2),col=3)
#Calculate number of unique values at each time point
Nuniq = rep(NA,nT)
for(i in 1:nT)
{
  Nuniq[i] = length(unique(x[i,]))
}
plot(1:nT,Nuniq)

