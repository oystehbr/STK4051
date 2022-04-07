#Extra 35
library(mvtnorm)
#intializing values
mu = c(1,2)
a = 0
Sigma = matrix(c(1,a,a,1),ncol=2)


sig.prop=4
N = 1000  # algorithm to limit 
M = 1000  # amount of simulation variables
X = matrix(nrow=N,ncol=2)
Y = matrix(nrow=M,ncol=2)
for(j in 1:M) {
  acc = 0
  # Start at a place
  X[1,] = rnorm(2)
  for(i in 2:N){
    index = sample(1:2,1)
    y = X[i-1,]
    # changes one of the coordinates
    y[index] = rnorm(1,X[i-1,index],sig.prop)
    
    # working with symmetric distribution
    R = dmvnorm(y,mu,Sigma)/dmvnorm(X[i-1,],mu,Sigma)
    if(runif(1)<R)
    {
      X[i,] = y
      acc = acc + 1
    }
    else
      X[i,] = X[i-1,]
  }
  # storing the last value
  Y[j, ] = X[N,]
  if (j %% 100 == 0) {
    cat("Iteration ", j, "/ ", M, "\n")
  }
}

show(colMeans(X))
show(var(X))
par(mfrow=c(1,1))
plot(X,type="l")
cat("Acceptance rate=",acc/(N-1),"\n")

cat("Actual result, by running the algorithm with M different threads (get M samples) \n")
show(colMeans(Y))
show(var(Y))
plot(Y)
cat("Acceptance rate=",acc/(N-1),"\n")
