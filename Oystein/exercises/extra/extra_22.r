#Extra - Importance sampling standard normal
N =1000
a = 0:4
par(mfrow=c(3,2))
#a
x = rnorm(N)
J.hat1 = rep(NA,5)
for(i in 1:5)
 J.hat1[i] = mean((x+a[i])^2)
matplot(cbind(a,a),cbind(1+a^2,J.hat1),xlab=a,ylab="True and estimated",main="MC estimates")

#b
emp.var = rep(NA,5)
for(i in 1:5)
  emp.var[i] = var((x+a[i])^2)
matplot(cbind(a,a),cbind(2+4*a^2,emp.var),xlab=a,ylab="True and estimated",main="MC variance")

#c
x = rnorm(N)
J.hat2 = rep(NA,5)
emp.var2 = rep(NA,5)
for(i in 1:5)
{
  y = x+a[i]
  w = dnorm(y)/dnorm(y,a[i])
  J.hat2[i] = mean(w*(y+a[i])^2)
  emp.var2[i] = var(w*(y+a[i])^2)
}
matplot(cbind(a,a),cbind(1+a^2,J.hat2),xlab=a,ylab="True and estimated",main="IS estimates")
matplot(cbind(a),cbind(emp.var2),xlab=a,ylab="Estimated",main="IS variance")


x = rt(N,2)
J.hat3 = rep(NA,5)
emp.var3 = rep(NA,5)
for(i in 1:5)
{
  y = x+a[i]
  w = dnorm(y)/dt(y-a[i],2)
  J.hat3[i] = mean(w*(y+a[i])^2)
  emp.var3[i] = var(w*(y+a[i])^2)
}
matplot(cbind(a,a),cbind(1+a^2,J.hat3),xlab=a,ylab="True and estimated",main="IS estimates")
matplot(cbind(a),cbind(emp.var3),xlab=a,ylab="Estimated",main="IS variance")

#c
#c
x = rnorm(N)
J.hat3 = rep(NA,5)
emp.var3 = rep(NA,5)
for(i in 1:5)
{
  y = x+a[i]
  J.hat3[i] = mean(y^2)
  emp.var3[i] = var(y^2)
}
matplot(cbind(a,a),cbind(1+a^2,J.hat3),xlab=a,ylab="True and estimated",main="IS2 estimates")
matplot(cbind(a),cbind(emp.var3),xlab=a,ylab="Estimated",main="IS2 variance")