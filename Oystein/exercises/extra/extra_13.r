#Extra 9

u = runif(10000)
x0 = 1
gam = 0.01
x = x0 + gam*tan(pi*(u-0.5))
hist(x,100,freq=F, xlim = c(-2, 3), breaks = 10)
x = sort(x)
y = seq(-20,20,length=100)
lines(y,1/(pi*gam*(1+(y-x0)^2/gam^2)),col=2)
lines(y,dcauchy(y,x0,gam),col=3)
d = density(x)
y = seq(min(x),max(x),length=length(d$x))
matplot(cbind(d$x,y),cbind(d$y,dcauchy(y,x0,gam)),type="l",lty=1,col=1:2,xlim=c(-3,3))

