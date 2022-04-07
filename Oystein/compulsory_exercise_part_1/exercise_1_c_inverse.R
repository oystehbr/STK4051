source('exercise_1_functions.R')

beta = seq(-5, 5, length = 101)
gamma = 1
ps = c(1.1, 2, 5, 100)

# Plotting all the results for gamma = 1
first = TRUE
count = 1
for(p in ps){
  if(first){
    plot(f(beta, gamma, p), beta, type='l', col = count, ylim = c(-5, 5), xlim = c(-8, 8), 
         lwd = 2, main = 'The inverse function with gamma = 1', xlab = expression("f"['p, y'] ~ (beta)))
    first = FALSE
  } else {
    lines(f(beta, gamma, p), beta, col = count, lwd = 2)
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
    plot(f(beta, gamma, p), beta, type='l', col = count, ylim = c(-5, 5), xlim = c(-8, 8), 
         lwd = 2, main = 'The inverse function with gamma = 0.2', xlab = expression("f"['p, y'] ~ (beta)))
    first = FALSE
  } else {
    lines(f(beta, gamma, p), beta, col = count, lwd = 2)
  }
  
  count = count + 1
}


legend(-5, 6,legend = c('p = 1.1', 'p = 2', 'p = 5', 'p = 100'), fill = 1:4)
points(beta, beta, col = 'yellow')



