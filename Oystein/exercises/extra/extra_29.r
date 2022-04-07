#Exercise 29
#a)
M <- 10000
x <- runif(M)
phi <- (exp(x)-1)/(exp(1)-1)
cat(c("Ordinary            ",round(c(mean(phi),sd(phi)), 7),"\n"))
#b) (importance sampling)
u <- runif(M)
x <- sqrt(u)
phi <- (exp(x)-1)/((exp(1)-1)*2*x)  # why
cat(c("Importance sampling ",c(round(c(mean(phi),sd(phi)),7),"\n")))







#c) (importance sampling2)
u <- runif(M)
x <- u^(1/3)
phi <- (exp(x)-1)/((exp(1)-1)*3*x*x)
cat(c("Importance sampling2",round(c(mean(phi),sd(phi)),7),"\n"))
#d) (control variates)
#Using C=u with expectation 0.5
x <- runif(M)
phi <- (exp(x)-1)/(exp(1)-1)-(x-0.5)
cat(c("Control variates    ",round(c(mean(phi),var(phi)),7),"\n"))

#e) (antithetic variables)
M.half <- M/2
u <- runif(M.half)
x <- c(u,1-u)
phi <- (exp(x)-1)/(exp(1)-1)
sig2 <- 0.25*var(phi[1:M.half])+0.25*var(phi[M.half+1:M.half])+
       0.5*cov(phi[1:M.half],phi[M.half+1:M.half])
cat(c("Antitetic var       ",round(c(mean(phi),sqrt(sig2)),7),"\n"))
#f) (antithetic variables and importance sampling)
M.half <- M/2
u1 <- runif(M.half)
u <- c(u1,1-u1)
x <- sqrt(u)
phi <- (exp(x)-1)/((exp(1)-1)*2*x)-(u-0.5)
sig2 <- 0.25*var(phi[1:M.half])+0.25*var(phi[M.half+1:M.half])+
       0.5*cov(phi[1:M.half],phi[M.half+1:M.half])
cat(c("Antitet+imp.samp    ",round(c(mean(phi),sqrt(sig2)),7),"\n"))
#g) (importance sampling+control variates)
u <- runif(M)
x <- sqrt(u)
phi <- (exp(x)-1)/((exp(1)-1)*2*x)-0.18*(u-0.5)
cat(c("Importance sampling+CV",c(round(c(mean(phi),sd(phi)),7),"\n")))
