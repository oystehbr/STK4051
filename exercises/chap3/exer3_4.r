#Baseball - Genetic algorithm
rm(list=ls())
# setwd("~/Documents/GitHub/STK4051/exercises/chap3")
baseball <- read.table("baseball.dat",header=T)
baseball$freeagent = factor(baseball$freeagent)
baseball$arbitration = factor(baseball$arbitration)
p = ncol(baseball)-1
P = 100   # Population size
pop = matrix(NA,nrow=P,ncol=p)
AICfit = rep(NA,P)

#Generate initial population
for(k in 1:P)
{
 pop[k,] = sample(0:1,p,replace=T)
 ind = c(1:p)[pop[k,]==1]
 base2 = baseball[,c(1,1+ind)]
 AICfit[k] = AIC(lm(log(salary)~.,data=base2))
}
AICseq = AICfit
more = TRUE
Numit=30
pop.new = pop
AICfit.new = AICfit
mu = 0.01   #Probability for mutation
#Start iteration on updating populations
for(i in 1:Numit)
{
  #Tournament selection
  numgroup = 5
  sizegroup = P/numgroup
  parents = NULL
  
  # Looping to we get 200 parents, the same value can be chosen more than once
  # taking a random batch, witch size "sizegroup", find the one with lowerst AIC
  # then adding it to the list of parents
  while(length(parents)<(2*P))
  {
    #Construct subsets through permutation
    ind = sample(1:P,P,replace=FALSE)
    for(j in 1:numgroup)
    {
      # Choosing the sizegroup, first indexes (batches), find the 
      # minimum in the batch
      l = which.min(AICfit[ind[(j-1)*sizegroup+1:sizegroup]])
      
      # Finding the correct index for the parent
      parents = c(parents,ind[(j-1)*sizegroup+l])
    }
  }
 for(k in 1:P)
  {
    #Selecting parents with probability proportional to exp(-AIC)
 
    #phi1 = rank(-AICfit)/(P*(P+1))
    #phi2 = rank(-AICfit)/(P*(P+1))
    # One parent will be chosen by a probability proportional to exp(-AIC)
    # so dividing the number of the total exp(-AIC), and get the probabilities
    phi1 = exp(-AICfit)
    phi1 = phi1/sum(phi1)
    #phi2 = exp(-AICfit)
    
    # THe other parent will be choosen uniformly
    phi2 = rep(1/P,P)
    parent1 = sample(1:P,1,prob=phi1)
    parent2 = sample(1:P,1,prob=phi1)
    #parent2 = sample(1:P,1,prob=phi2)
    
    #Selection from tournament
    #parent1=parents[k]
    #parent2=parents[P+k]
     
    
    #Alternative ways of sampling parents, TRY IT OUT!
#    par.rank = rank(-AICfit)
#    parent1 = sample(1:P,1,prob=par.rank)
#    parent2 = sample(1:P,1)

    # Sampling independently which parent to inherit from
    # collect values from the parents randomly
    bred = sample(1:2,p,replace=T)
    pop.new[k,bred==1] = pop[parent1,bred==1]
    pop.new[k,bred==2] = pop[parent2,bred==2]
    
    #Mutation, small probablility of changing one of the feature values of the children
    ind2 = sample(0:1,p,replace=T,prob=c(1-mu,mu))
    if(sum(ind2)>0)
      pop.new[k,ind2==1] = 1-pop.new[k,ind2==1]
    
    # Recollect the new features in the model 
    ind = c(1:p)[pop.new[k,]==1]
    base2 = baseball[,c(1,1+ind)]
    AICfit.new[k] = AIC(lm(log(salary)~.,data=base2))
 }
  
  # updating the population and AIC-values
  pop = pop.new
  AICfit = AICfit.new
  AICseq = rbind(AICseq,AICfit)
}
matplot(AICseq,pch=1)
show(min(AICseq))
#plot.ts(apply(AICseq,1,min))

