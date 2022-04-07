source('exercise_2_functions.R')
source('exercise3_functions.R')

p = 0.9
squared_tau = 80

y = seq(-5, 5, length = 101)

# plotting the expression of the task
plot(y, beta_estimator(y, squared_tau, p), type = 'l', lwd = 2, ylab = 'estimator of beta_i')
  
