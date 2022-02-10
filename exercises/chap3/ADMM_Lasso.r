##############################################################
## Load and shape data
##############################################################

rm(list=ls())
setwd("~/Documents/GitHub/STK4051/exercises/chap3")
baseball <- read.table("baseball.dat",header=T)
#baseball$freeagent = factor(baseball$freeagent)
#baseball$arbitration = factor(baseball$arbitration)

y=baseball$salary
yn=scale(y)

X=as.numeric(as.matrix(baseball[,2:28]))
dim(X)=dim(baseball[,2:28])
Xn=scale(X)
np=dim(X)

##################################################################
## define functions 
###############################################################

# The first problem in the lectures
firstProblem = function(XTXrhoIinv,X,y,z,rho,lambda){
  beta=XTXrhoIinv%*%(t(X)%*%y+rho*(z-lambda/rho))
}

# The second problem in the lectures
secondProblem =function(beta,gamma,rho,lambda){
  u=beta+lambda/rho
  multiplier= ( abs(u)-gamma/rho)>0;
  z=sign(u)*(abs(u)-gamma/rho)*multiplier
}


lassoADMM  =  function(X,y,gamma){
  tol=1e-3
  rho=100 
  lambdai=0*X[1,] 
  zi     =0*X[1,]
  betai  =0*X[1,]
  np=dim(X)
  XTXrhoIinv = solve(t(X)%*%X +rho*diag(1,np[2],np[2]) )
  
  for(i in 1:10000){
    betaim1=betai
    betai = firstProblem(XTXrhoIinv,X,y,zi,rho,lambdai)
    zi=secondProblem(betai,gamma,rho,lambdai)
    lambdai = lambdai+rho*(betai-zi)
    
    relativeChange= sqrt(mean((betai-betaim1)^2))/sqrt(mean(betai^2))
    relativeSlack = sqrt(mean((betai-zi)^2))/sqrt(mean(betai^2))
    if (relativeChange <tol & relativeSlack<tol ){
      show(i)
      break
    }
  }
  zi
}

#########################################################
### run case

library(glmnet)
gamma=10;
lasso.mod=glmnet(Xn,yn,alpha=1,lambda=gamma/np[1])
lasso.coeff=predict(lasso.mod,type='coefficients')
beta=lassoADMM(Xn,yn,gamma)

show(beta)
show(lasso.coeff)

deviation = sqrt(mean((beta-lasso.coeff[2:28])^2))  
deviation/ 0.00135304


plot(lasso.coeff[2:28],beta)
