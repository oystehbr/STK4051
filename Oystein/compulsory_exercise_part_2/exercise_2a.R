data = read.table("data/tgsim.ascii", header=F)

colnames(data) = c('y')

# Collecting the data from the dataframe
y = data$y
n = length(y)

# setting the parameters 
a = 0.85; lambda = 0; sigma = 0.5
N = 10000  # number of starting poins

# matrix for storing values
x = matrix(nrow=n,ncol=N)
x.hat = matrix(nrow=n, ncol=2)  # storing expectation and variance value
N.eff = rep(NA,n)

prob_of_observation = function(y, x, N) {
  w = rep(0, N)
  if (y == 1) {
    w[x < -0.5] = 1
  } else if (y == 2) {
    w[x >= -0.5 & x < 0.5] = 1
  } else {
    w[x >= 0.5] = 1
  }
  return(w)
}

#Initialization
x[1,] = rnorm(N,0,1)
w = prob_of_observation(y[1], x[1,], N)
w = w/sum(w)
N.eff[1] = 1/sum(w^2)

# Resampling according to calculated weights
ind = sample(1:N,N,replace=T,prob=w)
x[1,] = x[1,ind]
w = rep(1/N,N)  # set the weights equal before next iteration
x.hat[1,1] = mean(x[1,])
x.hat[1,2] = var(x[1,])

for(t in 2:n)
{
  eps = rnorm(N, 0, sigma^2)
  x[t,] = a * x[t-1] + lambda + eps
  w = w * prob_of_observation(y[t], x[t,], N)
  w = w/sum(w)
  N.eff[t] = 1/sum(w^2)
  
  # Resample
  ind = sample(1:N,N,replace=T,prob=w)
  x[t,] = x[t,ind]
  w = rep(1/N,N)  # set the weights equal before next iteration
  x.hat[t,1] = mean(x[t,])
  x.hat[t,2] = var(x[t,])
}

matplot(1:length(N.eff),N.eff,type="l",lty=c(1),col=1,xlab="time")
matplot(cbind(1:n,1:n),x.hat,type="l",lty=c(1,2),col=1,xlab="time")

#Calculate number of unique values at each time poin

