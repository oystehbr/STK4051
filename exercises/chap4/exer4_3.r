#Exer 4.3
x = as.matrix(read.table("../data/trivariatenormal.dat",header=T))
show(colSums(is.na(x)))
#Remove all rows with all data missing
num.mis = apply(is.na(x),1,sum)
show(table(num.mis))
x = x[num.mis<3,]
num.mis = apply(is.na(x),1,sum)

#Initial values based on complete data
x.compl = x[num.mis==0,]
mu = colMeans(x.compl)
Sigma = var(x.compl)
show(mu)
show(Sigma)
n = nrow(x)
y.hat = x
V = array(0,c(n,3,3))
more = TRUE;eps=0.001
while(more)
{
  mu.new = rep(0,3)
  Sigma.new = matrix(0,nrow=3,ncol=3)
  for(i in 1:n)
  {
    #First find y.hat and V
    if(num.mis[i]>0)
    {
      ind = c(1:3)[!is.na(x[i,])]
      y.hat[i,] = mu[1:3]+Sigma[1:3,ind]%*%solve(Sigma[ind,ind])%*%matrix(x[i,ind]-mu[ind],ncol=1)
      V[i,,] = Sigma-Sigma[1:3,ind]%*%solve(Sigma[ind,ind])%*%Sigma[ind,1:3]
    }
  }
  #Then estimate
  mu.new = colMeans(y.hat)
  Sigma.new = matrix(0,3,3)
  for(i in 1:n)
    Sigma.new = Sigma.new + V[i,,]+(y.hat[i,]-mu)%o%(y.hat[i,]-mu)
  Sigma.new = Sigma.new/n
  more = (max(abs(mu-mu.new))>eps) | (max(abs(Sigma-Sigma.new))>eps)
  mu = mu.new
  Sigma = Sigma.new
}
show(mu)
show(Sigma)
