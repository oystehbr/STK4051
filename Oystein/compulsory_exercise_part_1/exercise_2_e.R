source('exercise_2_functions.R')
sparseData = read.table("data/sparseDataWithErrors.ascii", header=F)
colnames(sparseData) = c('betaGT', 'y')

# Collecting the data from the dataframe
y = sparseData$y
betaGT = sparseData$betaGT

# need to take an initialization away from class 0
squared_tau_init = mean(y)
p_init = 0.5

values = EM_algorithm(y, p_init, squared_tau_init)
p_opt = values[1]
squared_tau_opt = values[2]

# running bootstrap method for calculating the uncertainty of the parameters 
B = 1000
n = length(y)
p_simulated = rep(NA, B)
squared_tau_simulated = rep(NA, B)
for(b in 1:B) {
  y_sample = sample(y, n, replace = T)
  values = EM_algorithm(y_sample, p_init, squared_tau_init)
  p_simulated[b] = values[1]
  squared_tau_simulated[b] = values[2]
  print(B)
  
}

plot(p_simulated)
plot(squared_tau_simulated)
hist(p_simulated, breaks = 10)
hist(squared_tau_simulated, breaks = 20)

print('Bias:')
show(mean(p_simulated) - p_opt)
show(mean(squared_tau_simulated) - squared_tau_opt)

print('Standard error:')
show(sd(p_simulated))
show(sd(squared_tau_simulated))

