#Checking random generators
u = runif(100000)
plot(u[-1],u[-10000])
cor(u[-1],u[-10000])
