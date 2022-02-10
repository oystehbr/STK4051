#Genetic linkage
x = c(34,18,20,125)
#x = c(5,0,1,14)

# likelihooden
lfunc = function(theta,x)
{
  x[1]*log(theta)+(x[2]+x[3])*log(1-theta)+x[4]*log(2+theta)
}

# derivative of the likelihood (score function)
sfunc = function(theta,x)
{
  x[1]/theta - (x[2]+x[3])/(1-theta)+x[4]/(2+theta)
}

# second derivative of the likelihood (append a minus), 
Jfunc = function(theta,x)
{
  x[1]/theta^2 + (x[2]+x[3])/(1-theta)^2+x[4]/(2+theta)^2
}

theta = seq(0,1,length=1000)
plot(theta,lfunc(theta,x),type="l")
#dev.copy2pdf(file="../doc/genetic_linkage_2.pdf")

# genetic linkage model, newton rapshon method
gen.link.NR = function(x,theta=0.5,eps=0.0001,print.it=TRUE)
{
  
  # find the calues of the current theta
  l = lfunc(theta,x)
  s = sfunc(theta,x)
  J = Jfunc(theta,x)
  
  show(c(theta,l,s,J))
  
  # When the score function is small, then we are close to a extrema 
  while(abs(s)>eps)
  {
    # updating theta with newton rapshon
    alpha = 1
    theta.new = theta +alpha*s/J
    l.new = lfunc(theta.new,x)
    #show(c(theta.new,l.new))
    
    # if we went to far, we will make the steplength less
    while(theta.new < 0 | theta.new > 1 | l.new<l)
    {
      alpha = alpha/2
      theta.new = theta +alpha*s/J
      l.new = lfunc(theta.new,x)
    }
    
    # changing the different parameters, for new iteration
    theta = theta.new
    l = l.new
    s = sfunc(theta,x)
    J = Jfunc(theta,x)
    show(c(theta,l,s,J,alpha))
  }
  theta
}

# finding MLE of theta
theta.hat = gen.link.NR(x,0.5,print.it=TRUE)
sd.theta.hat = 1/sqrt(Jfunc(theta.hat,x))

print("Theta.hat")
show(c(theta.hat,sd.theta.hat))
points(theta.hat,lfunc(theta.hat,x),col=2)

Delta = theta[2]-theta[1]
L = exp(lfunc(theta,x))   #Unnormalized likelihood
L = L/(sum(L)*Delta)
plot(theta,L,type="l")
lines(theta,dnorm(theta,theta.hat,sd.theta.hat),col=2)

gen.link.EM = function(x,theta=0.5,eps=0.0001,print.it=TRUE)
{
 more = TRUE
 if(print.it)
  show(c(theta,lfunc(theta,x)))
 while(more)
  {
   # The update according to EM-algorithm
   theta.new = (x[1]+x[4]*theta/(2+theta))/(x[1]+x[2]+x[3]+x[4]*theta/(2+theta))
   more = abs(theta.new-theta)>eps
   theta = theta.new
   if(print.it)
     show(c(theta,lfunc(theta,x)))
 }
 theta
}
theta.hat = gen.link.EM(x,0.5,print.it=TRUE)
print("Theta.hat")
show(theta.hat)
points(theta.hat,lfunc(theta.hat,x),col=3)

#Bootstrapping
#Note: Should here bootstrap indivuduals, 
#first convert to a vector giving categories for each individual
y = rep(1:4,x)
n = sum(x)

B = 10000
theta.sim = rep(NA,B)
for(b in 1:B)
{
  y.sim = sample(y,n,replace=T)
  x.sim = table(c(y.sim,1:4))-1   #Trick to guarantee that all 4 categories are counted
  theta.sim[b] = gen.link.EM(x.sim,theta=0.5,print.it=FALSE)
}
hist(theta.sim)
print("Bias:")
show(mean(theta.sim)-theta.hat)
print("Standard error:")
show(sd(theta.sim))

