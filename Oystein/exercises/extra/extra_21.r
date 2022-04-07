n <- 20
#x <- seq(-1,1,length=n)
x = runif(n,-1,1)
M <- 1000
avec <- c(-0.9,-0.5,0,0.5,0.8,0.9)
alpha = 0
beta = 2
sigma = 1
beta.est <- matrix(ncol=2,nrow=M)
sigma.est <- rep(NA,M)
tstat2 = rep(NA,M)
bias.beta = matrix(nrow=length(avec),ncol=2)
var.beta = matrix(nrow=length(avec),ncol=2)
bias.sig = rep(NA,length(avec))
var.sig = rep(NA,length(avec))
rej.t = rep(NA,length(avec))
rej = rep(NA,M)
for(k in 1:length(avec))
{
  a = avec[k]
  sigma.eta = sigma*sqrt(1-a^2)
  set.seed(345)
  for(m in 1:M)
   {
    eps <- arima.sim(n,model=list(ar=a),sd=sigma.eta)
    y <- alpha+beta*x+eps
    fit <- lm(y~x)
    beta.est[m,] <- fit$coef
    sigma.est[m] <- summary(fit)$sigma
    tstat2[m] = (coef(summary(fit))[2,1]-beta)/coef(summary(fit))[2,2]
    rej[m] = abs(tstat2[m])>qt(0.975,n-2)
   }
  
  cat("Estimated significance level",mean(rej),"\n")
  bias.beta[k,] = apply(beta.est,2,mean)-c(alpha,beta)
  var.beta[k,] =  apply(beta.est,2,var)
  bias.sig[k] = mean(sigma.est)-sigma
  var.sig[k] = var(sigma.est)
  rej.t[k] = mean(rej)
}
matplot(cbind(avec,avec,avec),cbind(bias.beta,bias.sig),xlab="a",ylab="bias",type="l",lty=1)
matplot(cbind(avec,avec,avec),cbind(var.beta,var.sig),xlab="a",ylab="var",type="l",lty=1)
legend("topleft",c("beta0","beta1","sig"),lty=1,col=1:3)
plot(avec,rej.t,type="l")