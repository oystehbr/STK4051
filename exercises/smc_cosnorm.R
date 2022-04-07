#SMC - cos*normal


nT=100
n=100000
x = matrix(nrow=nT,ncol=n)
sig2.hat = rep(NA,nT)
mu.hat = rep(NA,nT)
var.weight = rep(NA,nT)
N.eff = rep(NA,nT)


a=1
frac = 0.001    #No resampling if frac=0

# initial values of all columns
x[1,] = rnorm(n,0,sqrt(2))


# Assuming that the starting value was 0 (x0 = 0)
w = abs(cos(a*x[1,]))*exp(-0.25*x[1,]^2)/dnorm(x[1,],0,sqrt(2))

#normalizing the weights
w = w/sum(w)

# Find the expected value and variance for the first time step
mu.hat[1] = mean(x[1,]*w)
sig2.hat[1] = mean(w*(x[1,]-mu.hat[1])^2)/mean(w)
 

var.weight[1] = var(w/sum(w))


N.eff[1] = 1/sum(w^2)

# resampling the first observations
if(N.eff[1]<(frac*n))    # Resampling
{
  x[1,] = sample(x[1,],n,replace=TRUE,prob=w)
  w = rep(1,n)
  cat("Resampling at time ",1,"\n")
}



# Looping over all rows (corresponding to the states X_row)
for(i in 2:nT)
{
  x[i,] = rnorm(n,x[i-1,],sqrt(2))
  w = w*abs(cos(a*x[i,]) - x[i-1,])*exp(-0.25*(x[i,]-x[i-1,])^2)/dnorm(x[i,],x[i-1,],sqrt(2))
  w = w/sum(w)
  
  mu.hat[i] = sum(x[i,]*w)
  sig2.hat[i] = sum(w*(x[i,]-mu.hat[i])^2)
  var.weight[i] = var(w)
  N.eff[i] = 1/sum(w^2)
  if(N.eff[i]<(frac*n))    #Resampling
  {
    x[i,] = sample(x[i,],n,replace=TRUE,prob=w)
    w = rep(1,n)
    cat("Resampling at time ",i,"\n")
  }
}


par(mfrow=c(2,2))
plot.ts(mu.hat,ylab=expression(hat(mu)))
plot.ts(sig2.hat/c(1:nT),ylab=expression(hat(sigma)[2]))
plot.ts(sqrt(var.weight),ylab="Std(w)")
plot.ts(N.eff,ylab="N.eff")

w.norm.sort = sort(w/sum(w),decreasing=T)
show(w.norm.sort[1:10])
x
1/sum(w^2)

plot(x[1:100,1:10][,1])
lines(x[1:100,1:10])


x[1,]=rnorm(1000,0,2)
