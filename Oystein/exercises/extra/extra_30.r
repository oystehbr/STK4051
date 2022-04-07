#Exercise 30
#Phi(theta)=E[(X-theta)^2], log(X)\sim N(theta,1)
theta1=1.1
theta2=1
N = 100000
#Independent draws
Z1 = rnorm(N,theta1,1);X1=exp(Z1)
Z2 = rnorm(N,theta2,1);X2=exp(Z2)
Phi1.hat = mean((X1-theta1)^2)
sig2.1.hat = sd((X1-theta1)^2)
Phi2.hat = mean((X2-theta2)^2)
sig2.2.hat = sd((X2-theta2)^2)
sig2.hat = sd(((X1-theta1)^2+(X2-theta2)^2))
print("Difference - independent draws")
show(c(Phi1.hat-Phi2.hat,sqrt(sig2.1.hat^2+sig2.2.hat^2)/sqrt(N),sig2.hat/sqrt(N)))
print("Derivative - independent draws")
d=theta1-theta2
show(c((Phi1.hat-Phi2.hat)/d,sqrt(sig2.1.hat^2+sig2.2.hat^2)/(sqrt(N)*d),sig2.hat/(d*sqrt(N))))

#Common random numbers
Eps = rnorm(N)
Z1 = theta1+Eps;X1=exp(Z1)
Z2 = theta2+Eps;X2=exp(Z2)
Phi1.hat = mean((X1-theta1)^2)
Phi2.hat = mean((X2-theta2)^2)
sig2.hat = sd((X1-theta1)^2-(X2-theta2)^2)
print("Difference - common random numbers")
show(c(Phi1.hat-Phi2.hat,sig2.hat/sqrt(N)))
print("Derivative - common random numbers")
show(c((Phi1.hat-Phi2.hat)/d,sig2.hat/(d*sqrt(N))))
cat("Difference obtained by 100 000 000 simulations: 10.36\n")
cat("Derivative obtained by 100 000 000 simulations: 103.57\n")

