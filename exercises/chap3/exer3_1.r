#Baseball
rm(list=ls())
# setwd("~/Documents/GitHub/STK4051/exercises/chap3")

baseball <- read.table("baseball.dat",header=T)
baseball$freeagent = factor(baseball$freeagent)

baseball$arbitration = factor(baseball$arbitration)

# How many features (removing the salary, predicting variable)
p = ncol(baseball)-1

#Local search, 1-neigh, starting with a random features
model = sample(c(0,1),p,replace=T)

# Finding the indexes of the random features
ind2 = c(1:p)[model==1]
base2 = baseball[,c(1,1+ind2)]

# Fitting a linear model to the log-salary parameter, with the given features
fit = lm(log(salary)~.,data=base2)
numcal = 0

# Number we want to minimize
AICopt = AIC(fit)
more = TRUE
AICseq = AICopt

# This loop will make another model by a change in the j-th feature, compare the AIC
# with the newer model, if no improvement => go to next feature. And keep going
while(more)
{
  more = FALSE
  for(j in 1:p)
  {
    model2 = model
    model2[j] = 1-model2[j]
    ind2 = c(1:p)[model2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal = numcal+1
    if(AIC(fit2)<AICopt)
    {
      more = TRUE
      model[j] = 1-model[j]
      AICopt = AIC(fit2)
    }
  }
  AICseq = c(AICseq,AICopt)
}
plot.ts(AICseq)
show(AICopt)
show(numcal)

#a)
#Local search, randomly until better
model = sample(c(0,1),p,replace=T)
ind2 = c(1:p)[model==1]
base2 = baseball[,c(1,1+ind2)]
fit = lm(log(salary)~.,data=base2)
numcal=0
AICopt = AIC(fit)
more = TRUE
AICseq = AICopt

# Will change one by one feature (at random), until improvement. If some improvement
# the algorithm will start over, but if no improvement -> termination. 
while(more)
{
  ind = sample(1:p,p)   #Random order of changes
  i=0
  more2 = TRUE
  
  # running until there is some improvement, not ordering feature changes
  # but will be random choice of the feature. When one feature is improving then
  # this loop will terminate (or ran through all the features)
  while(more2)
  {
    i = i+1
    j = ind[i]
    model2 = model
    model2[j] = 1-model2[j]
    ind2 = c(1:p)[model2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal=numcal+1
    if((AIC(fit2) < AICopt) | (i==p))
      more2 = FALSE
  }
  more = FALSE
  if(AIC(fit2)<AICopt)
  {
    more = TRUE
    model[j] = 1-model[j]
    AICopt = AIC(fit2)
    AICseq = c(AICseq,AICopt)
  }
}
plot.ts(AICseq)
show(AICopt)
show(numcal)

#b)
#Local search, 2-neigh
mod = sample(c(0,1),p,replace=T)
ind2 = c(1:p)[mod==1]
base2 = baseball[,c(1,1+ind2)]
fit = lm(log(salary)~.,data=base2)
numcal=0
AICopt = AIC(fit)
more = TRUE
AICseq = AICopt
while(more)
{
  #First 1-neigh
  more = FALSE
  for(j in 1:p)
  {
    mod2 = mod
    mod2[j] = 1-mod2[j]
    ind2 = c(1:p)[mod2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal=numcal+1
    if(AIC(fit2)<AICopt)
    {
      more = TRUE
      mod[j] = 1-mod[j]
      AICopt = AIC(fit2)
    }
  }
  AICseq = c(AICseq,AICopt)
  #Then 2-neigh
  for(j in 1:(p-1))
  #for(k in (j+1):p)
   for(k in 1:(p-1))
  {
    i=i+1
    mod2 = mod
    mod2[j] = 1-mod2[j]
    mod2[k] = 1-mod2[k]
    ind2 = c(1:p)[mod2==1]
    base2 = baseball[,c(1,1+ind2)]
    fit2 = lm(log(salary)~.,data=base2)
    numcal=numcal+1
    if(AIC(fit2)<AICopt)
    {
      more = TRUE
      mod[j] = 1-mod[j]
      mod[k] = 1-mod[k]
      AICopt = AIC(fit2)
    }
  }
 AICseq = c(AICseq,AICopt)
}
plot.ts(AICseq)
show(AICopt)
show(numcal)

