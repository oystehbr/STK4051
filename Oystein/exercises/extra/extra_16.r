#Exercise 16
n = 10000
par(mfrow=c(2,2))

#a
print("Normal")
z = rnorm(n,2,1)

#b
d = density(z)
matplot(cbind(d$x,d$x),cbind(dnorm(d$x,2,1),d$y),type="l",lty=1,main="Normal")

#c
print("mean and sd")
show(c(mean(z),sd(z)))

#d
v = z[z>3]
print("True and estimated conditional mean")
show(c(2+exp(-0.5)/(sqrt(2*pi)*(1-pnorm(1))),mean(v)))

show(length(v))
#e1
print("Exponential")
z = rexp(n,2)
d = density(z,from=0)
matplot(cbind(d$x,d$x),cbind(dexp(d$x,2),d$y),type="l",lty=1,main="Exponential", xlim = c(0, 6))
print("mean and sd")
show(c(mean(z),sd(z)))
v = z[z>3]
print("True and estimated conditional mean")
show(c(3.5,mean(v)))
show(length(v))
#e2
print("T-distribution")
z = rt(n,4)
d = density(z)
matplot(cbind(d$x,d$x),cbind(dt(d$x,4),d$y),type="l",lty=1,main="T-distribution")
print("mean and sd")
show(c(mean(z),sd(z)))
v = z[z>3]
print("Estimated conditional mean")
show(mean(v))
show(length(v))
#e4
print("Gamma")
z = rgamma(n,2,1)
d = density(z,from=0)
matplot(cbind(d$x,d$x),cbind(dgamma(d$x,2,1),d$y),type="l",lty=1,main="Gamma")
show(c(mean(z),sd(z)))
print("Estimated conditional mean")
v = z[z>3]
show(mean(v))
show(length(v))

