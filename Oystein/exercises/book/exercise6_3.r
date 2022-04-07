#Exercise 6.3

q = function(x)
  {
  exp(-abs(x)^3/3)
}

M = 100
N = 10000
sig2.hat1 = rep(NA,M)
sig2.hat2 = rep(NA,M)
sig2.hat3 = rep(NA,M)
for(i in 1:M)
{

#a
x = rnorm(N)
w = q(x)/dnorm(x) # weights
w = w/sum(w) # standarized weights
sig2.hat1[i] = sum(w*x^2)

#b 
#The ratio q(x)/phi(x) has max point at x=-1,1 
alpha = q(1)/dnorm(1)
w = q(x)/(alpha*dnorm(x))
x_official = x[runif(N)<w]
sig2.hat2[i] = mean(x_official^2)

#c
x.sort = sort(x)
x1 = x.sort[-length(x)]
xn = x.sort[-1]
sig2.hat3[i] = mean((xn-x1)*x1^2*q(x1))/mean((xn-x1)*q(x1))
}

res = cbind(c(mean(sig2.hat1),mean(sig2.hat2),mean(sig2.hat3)),
                   c(sd(sig2.hat1),sd(sig2.hat2),sd(sig2.hat3)))
colnames(res) = c("Estimate","Std.Error")
rownames(res)= c("Imp.samp","Rej.samp","P-R")
show(res)
