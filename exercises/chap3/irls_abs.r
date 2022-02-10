#######################################################
rm(list = ls())
## set upmodel
beta=c(1,1)
set.seed(231171)
n = 20

eps=rnorm(n,0,1)
x=rnorm(n,0,2)
Xdata=cbind(1,x)

y=beta[1]+beta[2]*x+eps

###############################################################
## code

## Least squares
leastSquares = function(X,y)
{
  betaHat = solve(t(X)%*%X) %*%(t(X)%*%y )
}


# IRLS

IRLS_L1 = function(X,y, max_it)
{  
  
  betaHat = solve(t(X)%*%X) %*%(t(X)%*%y )
  pred0 = X%*%betaHat
  res0  =(y-pred0)
  
  betaPrev=c(0,0)
  betaWHat=betaHat
  it=0
  
  while(sum(abs(betaPrev-betaWHat))>0.0001 & it<max_it)
  {  maxW=10
  it=it+1
  betaPrev=betaWHat
  pred= X%*%betaPrev
  res=(y-pred)
  
  # Need to weight the closest once more than outliers
  w = 1/abs(res)
  w[w>maxW]=maxW  # adjustment to avoid super large numbers [ size relative to problem]
  W = diag(as.vector(w))
  
  betaWHat = solve(t(X)%*%W%*%X) %*%(t(X)%*%W%*%y )
  #show(betaWHat)
  }
  betaWHat
}

## Standard case 
betaHat_L2=leastSquares(Xdata,y)
betaHat_L1=IRLS_L1(Xdata,y, 1)


Xplot=cbind(1,c(-5,0,5))

plot(x,y,xlim=c(-6,6),ylim=c(-6,6))
lines(c(-5,5),c(-4,6),lty=3)
lines(Xplot[,2], Xplot%*%betaHat_L2,lty = 2,col=3,lwd=2)
lines(Xplot[,2], Xplot%*%betaHat_L1,lty = 2,col=4,lwd=2)

maxW=10
pred= Xdata%*%betaHat_L1
res=(y-pred)
w = 1/abs(res)
plot(w,type="l",log="y",ylim=c(0.1,300))
w[w>maxW]=maxW  
points(w)


### Case with outliers 
ymod=y
ymod[1]=0
ymod[14]=0

# creating a plot with the solution
plot(x,ymod,xlim=c(-6,6),ylim=c(-6,6))
lines(c(-5,5),c(-4,6),lty=3)

betaHat_L2m=leastSquares(Xdata,ymod)
lines(Xplot[,2], Xplot%*%betaHat_L2m,lty = 2,col=3,lwd=2)
  
betaHat_L1m=IRLS_L1(Xdata,ymod, 100)
lines(Xplot[,2], Xplot%*%betaHat_L1m,lty = 2,col=4,lwd=1)



maxW=10
pred= Xdata%*%betaHat_L1m
res=(ymod-pred)
w = 1/abs(res)
plot(w,type="l",log="y",ylim=c(0.1,300))
w[w>maxW]=maxW  
points(w)





