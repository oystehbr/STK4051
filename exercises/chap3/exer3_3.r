#Baseball - simulated annealing


rm(list=ls())
baseball <- read.table("data/baseball.dat",header=T)
baseball$freeagent = factor(baseball$freeagent)
baseball$arbitration = factor(baseball$arbitration)
p = ncol(baseball)-1
mod = sample(0:1,p,replace=T)
ind = c(1:p)[mod==1]
base2 = baseball[,c(1,1+ind)]
fit = lm(log(salary)~.,data=base2)
AICcur = AIC(fit)
more = TRUE
AICseq = AICcur
Numit=1000

# An algorithm for annealing. One iteration will create a new model with 3 changed 
# features (which is containing in the model). Set a probablility to wheather the 
# the new model shall be kept or not (will be kept if it's better). The probability of 
# changing the model will be less w.r.t. time and how worse model it is. 
for(i in 1:Numit)
{
#  tau = 100/log(i+1)
  tau = 100/(i+1)
  j = sample(1:p,1)
  k = sample(1:p,1)
  l = sample(1:p,1)
  mod2 = mod
  mod2[j] = 1-mod2[j]
  mod2[k] = 1-mod2[k]
  mod2[l] = 1-mod2[l]
  ind2 = c(1:p)[mod2==1]
  base2 = baseball[,c(1,1+ind2)]
  fit2 = lm(log(salary)~.,data=base2)
  AIC2 = AIC(fit2)
  
  # The probability of keeping the new model
  prob = exp((AICcur-AIC2)/tau)
  u = runif(1)
  if(u<prob)
  {
    mod[j] = 1-mod[j]
    mod[k] = 1-mod[k]
    mod[l] = 1-mod[l]
    AICcur = AIC2
  }
  AICseq = c(AICseq,AICcur)
}
plot.ts(AICseq)
show(min(AICseq))
