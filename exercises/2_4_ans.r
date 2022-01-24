#Exercise 2.4

# The function we want to satisfy s(x) = 0
s = function(x)
{
  matrix(c(dgamma(x[2],2,1)-dgamma(x[1],2,1),
    pgamma(x[2],2,1)-pgamma(x[1],2,1)-0.95),ncol=1)
}

# The derivative of s
d = function(x)
{
  matrix(c(-exp(-x[1])*(1-x[1]),exp(-x[2])*(1-x[2]),
           -dgamma(x[1],2,1),dgamma(x[2],2,1)),ncol=2,byrow=T)
}

x = seq(0,6,length=100)
plot(x,dgamma(x,2,1),type="l")
# Start the iteration of x, with the confidence interval quantiles. 
x = c(qgamma(0.025,2,1),qgamma(0.975,2,1))
show(c(x,dgamma(x[2],2,1)-dgamma(x[1],2,1),pgamma(x[2],2,1)-pgamma(x[1],2,1),x[2]-x[1]))

text(x,dgamma(x,2,1),"0")
# Iterating with the newton method
for(i in 1:8)
{
  x = x - solve(d(x),s(x))
  show(c(x,dgamma(x[2],2,1)-dgamma(x[1],2,1),pgamma(x[2],2,1)-pgamma(x[1],2,1),x[2]-x[1]))
  text(x,dgamma(x,2,1),i)
}

# Show that the required conditions are satisfied. 
show(dgamma(x,2,1))
show(diff(pgamma(x,2,1)))
