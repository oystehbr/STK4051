source('exercise_1_functions.R')

beta = seq(-5, 5, length = 101)
gamma = 1
ps = c(1.1, 2, 5, 100)

# Plotting all the results for gamma = 1
first = TRUE
count = 1
for(p in ps){
  if(first){
    plot(beta, f(beta, gamma, p), type='l', col = count, ylim = c(-8, 8), xlim = c(-5, 5), 
         lwd = 2, main = 'The function with gamma = 1', ylab = expression("f"['p, y'] ~ (beta)))
    first = FALSE
  } else {
    lines(beta, f(beta, gamma, p), col = count, lwd = 2)
  }
  
  count = count + 1
}

legend(-5, 6,legend = c('p = 1.1', 'p = 2', 'p = 5', 'p = 100'), fill = 1:4)
points(beta, beta, col = 'yellow')

gamma = 0.2

# Plotting all the results for gamma = 0.2
first = TRUE
count = 1
for(p in ps){
  if(first){
    plot(beta, f(beta, gamma, p), type='l', col = count, ylim = c(-8, 8), xlim = c(-5.1, 5.1), 
         lwd = 2, main = 'The function with gamma = 0.2', ylab = expression("f"['p, y'] ~ (beta)))
    first = FALSE
  } else {
    lines(beta, f(beta, gamma, p), col = count, lwd = 2)
  }
  
  count = count + 1
}


legend(-5, 6,legend = c('p = 1.1', 'p = 2', 'p = 5', 'p = 100'), fill = 1:4)
points(beta, beta, col = 'yellow')

