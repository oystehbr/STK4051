# Data and plot them:
x = c(-5.00, -4.00, -3.00, -2.00, -1.00, 0.00, 1.00, 2.00, 3.00,  4.00, 5.00)
y = c(1.05,  3.20,  1.60,  3.05,  2.45, 2.90, 2.00, 2.20, 4.60,  4.00, 3.50)
par(mfrow=c(1,1))
plot(x,y)

# fitting linear models 
fit0 = lm(y~1)
fit1 = lm(y~x)
anova(fit0,fit1)

# initializing some values
t0 = 5
n=5
m=2*n+1
M = 1000
beta0 = 1 # own choice
beta1.vec = seq(-1,1,by=0.05) # vector of beta1 values
alpha = 0.05

# Save predictions of the models 
y.pred = matrix(nrow=length(beta1.vec),ncol=M)

#looping over all beta1 - values
for(i in 1:length(beta1.vec))
{
  beta1 = beta1.vec[i]
  
  # for each beta, 
  for(j in 1:M)
  {
    # using model 1, to create y simulations 
    y.sim = beta0 + beta1*x + rnorm(m, 0, 1)
    fit = lm(y.sim~x)
    
    # checking whether beta1 in fit is significant, as the test in the exercise
    if(summary(fit)$coef[2,4]<alpha)
      y.pred[i,j] = summary(fit)$coef[1]+summary(fit)$coef[2]*t0
    else
      y.pred[i,j] = mean(y.sim)
  }
}


yhat = apply(y.pred,1,mean)
ysd = apply(y.pred,1,sd)
par(mfrow=c(2,1),mar=c(2,2,2,2))

# plotting the mean value of the theta predictions (for different beta1 values)
plot(beta1.vec,yhat,xlab="beta",ylab="yhat",type="l")
abline(h=beta0,col=2)
lines(beta1.vec,beta0+beta1.vec*t0,col=3)
legend("topleft",c("Simulated","Constant","Linear"),lty=1,col=1:3)

#plotting the sd values for the different theta values
plot(beta1.vec,ysd,xlab="beta",ylab="sd(yhat)",type="l",ylim=c(0.3,1.3))
abline(h=1/sqrt(m),col=2)
lines(beta1.vec,rep(t0*1/sqrt(sum(x^2)),length(beta1.vec)),col=3)

