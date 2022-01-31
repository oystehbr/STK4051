#Baseball
rm(list=ls())
baseball <- read.table("baseball.dat",header=T)
baseball$freeagent = factor(baseball$freeagent)
baseball$arbitration = factor(baseball$arbitration)
p = ncol(baseball)-1  #number of parameters

#Local search, 1-neigh
mod = sample(c(0,1),p,replace=T)      #make list of 0 or 1 randomly 
ind2 = c(1:p)[mod==1]                 #random indexes
base2 = baseball[,c(1,1+ind2)]        #choose right columns
fit = lm(log(salary)~.,data=base2)    #fit with all data
numcal = 1                            #number of caculations
AICopt = AIC(fit)                     #function output given model
more = TRUE
AICseq = AICopt                       #temp variable

while(more)
{
  more = FALSE
  for(j in 1:p)
  {
    mod2 = mod
    mod2[j] = 1-mod2[j]                  #switch sign of entry j
    ind2 = c(1:p)[mod2==1]               #update indexes
    base2 = baseball[,c(1,1+ind2)]       #update columns
    fit2 = lm(log(salary)~.,data=base2)  #new fit
    numcal = numcal+1
    if(AIC(fit2)<AICopt)                 #check if new fit is better
    {
      more = TRUE                        #if better, update and restart loop
      mod[j] = 1-mod[j]
      AICopt = AIC(fit2)
    }
  }
  AICseq = c(AICseq,AICopt)              #add AIC value, order should decrease
}

#Random descent
mod = sample(c(0,1),p,replace=T)
ind2 = c(1:p)[mod==1]               
base2 = baseball[,c(1,1+ind2)]        
fit = lm(log(salary)~.,data=base2)
AICopt = AIC(fit)
AICseq = AICopt   
more=TRUE

while(more)
{
  more=FALSE
  nei_list=sample(1:27, 27, replace=F)
  for (i in nei_list)
  {
    mod2 = mod
    mod2[i]=1-mod2[i]
    ind2 = c(1:p)[mod2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    if(AIC(fit2)<AICopt)
    {
      more = TRUE                        #if better, update and restart loop
      mod[j] = 1-mod[j]
      AICopt = AIC(fit2)
      AICseq = c(AICseq, AICopt)
      break
    }
  }
}

