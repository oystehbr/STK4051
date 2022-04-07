#Exercise 6.4
coal = read.table("data/coal.dat",header=T)
coal$num = 1:112
par(mfrow=c(3,4))
set.seed(43534)
N = 100000

#a
# sample of the different priors
theta = sample(1:111,N,replace=T) 
a1 = rgamma(N,shape=10,rate=10) # sample from the distribution of a1
lambda1 = rgamma(N,shape=3,rate=a1)
a2 = rgamma(N,shape=10,rate=10) # sample from the distribution of a2
lambda2 = rgamma(N,shape=3,rate=a2) 
w = rep(NA,N)

# calculating all the weights
for(i in 1:N)
{
  # depending on theta
  x1 = coal$disasters[coal$num<=theta[i]]
  x2 = coal$disasters[coal$num>theta[i]]
  w[i] = sum(dpois(x1,lambda1[i],log=TRUE))+        #Working on log-scale is more stable
             sum(dpois(x2,lambda2[i],log=TRUE))
}

#normalizing the weights
w = exp(w-max(w))
w = w/sum(w)

# sequential importance resampling
ind.sir = sample(1:N,N,replace=T,prob=w)
theta.sir = theta[ind.sir]
lambda1.sir = lambda1[ind.sir]
lambda2.sir = lambda2[ind.sir]
print("Prior from (a)")
cat("Estimate of theta",mean(theta.sir),"\n")
hist(theta.sir,100)
cat("Credibility interval for theta:",
    quantile(theta.sir,0.025),quantile(theta.sir,0.975),"\n")
cat("Estimate of lambda1",mean(lambda1.sir),"\n")
hist(lambda1.sir,100)
cat("Credibility interval for lambda1:",
    quantile(lambda1.sir,0.025),quantile(lambda1.sir,0.975),"\n")
cat("Estimate of lambda2",mean(lambda2.sir),"\n")
hist(lambda2.sir,100)
cat("Credibility interval for lambda2:",
    quantile(lambda2.sir,0.025),quantile(lambda2.sir,0.975),"\n")

plot(lambda1,lambda2) # plotting all points from the priors
points(lambda1.sir,lambda2.sir,col=2) # plotting all points from the resampling 

#b
theta = sample(1:111,N,replace=T)
a1 = rgamma(N,shape=10,rate=10)
lambda1 = rgamma(N,shape=3,rate=a1)
logalpha = runif(N,log(1/8),log(2))
alpha = exp(logalpha)
lambda2 = lambda1*alpha
w = rep(NA,N)
for(i in 1:N)
{
  x1 = coal$disasters[coal$num<=theta[i]]
  x2 = coal$disasters[coal$num>theta[i]]
  w[i] = sum(dpois(x1,lambda1[i],log=TRUE))+
    sum(dpois(x2,lambda2[i],log=TRUE))
}
w = exp(w-max(w))
w = w/sum(w)
ind.sir = sample(1:N,N,replace=T,prob=w)
theta.sir = theta[ind.sir]
lambda1.sir = lambda1[ind.sir]
lambda2.sir = lambda2[ind.sir]
print("Prior from (b)")
cat("Estimate of theta",mean(theta.sir),"\n")
hist(theta.sir,100)
cat("Credibility interval for theta:",
    quantile(theta.sir,0.025),quantile(theta.sir,0.975),"\n")
cat("Estimate of lambda1",mean(lambda1.sir),"\n")
hist(lambda1.sir,100)
cat("Credibility interval for lambda1:",
    quantile(lambda1.sir,0.025),quantile(lambda1.sir,0.975),"\n")
cat("Estimate of lambda2",mean(lambda2.sir),"\n")
hist(lambda2.sir,100)
cat("Credibility interval for lambda2:",
    quantile(lambda2.sir,0.025),quantile(lambda2.sir,0.975),"\n")
plot(lambda1,lambda2)
points(lambda1.sir,lambda2.sir,col=2)

#c
theta = sample(1:111,N,replace=T)
a1 = runif(N,0,100)
lambda1 = rgamma(N,shape=3,rate=a1)
a2 = runif(N,0,100)
lambda2 = rgamma(N,shape=3,rate=a2)

w = rep(NA,N)
for(i in 1:N)
{
  x1 = coal$disasters[coal$num<=theta[i]]
  x2 = coal$disasters[coal$num>theta[i]]
  w[i] = sum(dpois(x1,lambda1[i],log=TRUE))+
    sum(dpois(x2,lambda2[i],log=TRUE))
}
w = exp(w-max(w))
w = w/sum(w)
ind.sir = sample(1:N,N,replace=T,prob=w)
theta.sir = theta[ind.sir]
lambda1.sir = lambda1[ind.sir]
lambda2.sir = lambda2[ind.sir]
print("Prior from (c)")
cat("Estimate of theta",mean(theta.sir),"\n")
hist(theta.sir,100)
cat("Credibility interval for theta:",
    quantile(theta.sir,0.025),quantile(theta.sir,0.975),"\n")
cat("Estimate of lambda1",mean(lambda1.sir),"\n")
hist(lambda1.sir,100)
cat("Credibility interval for lambda1:",
    quantile(lambda1.sir,0.025),quantile(lambda1.sir,0.975),"\n")
cat("Estimate of lambda2",mean(lambda2.sir),"\n")
hist(lambda2.sir,100)
cat("Credibility interval for lambda2:",
    quantile(lambda2.sir,0.025),quantile(lambda2.sir,0.975),"\n")
plot(lambda1,lambda2)
points(lambda1.sir,lambda2.sir,col=2)

