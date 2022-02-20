source('exercise_1_functions.R')

# check the f_bi - function
# plot(beta, f_bi(beta, y, gamma, p), type = 'l')

# test bisection method
gamma = 1
ps = c(1.1, 2, 100)
y = seq(-5, 5, length = 101)

beta_init1 = -5
beta_init2 = 4.9

first = TRUE
count = 1
for(p in ps){
  val = c() 
  for (y_i in y){
    val = c(val, bisection_method(y_i, gamma, p, beta_init1, beta_init2))
  }
  print(val)
  
  if(first){
    plot(y, val, type='l', col = count, ylim = c(-5, 5), xlim = c(-5.1, 5.1), 
         lwd = 2, main = 'beta_opt w.r.t. y', xlab = 'y', ylab = 'beta_opt')
    first = FALSE
  } else {
    lines(y, val, col = count, lwd = 2)
  }
  
  count = count + 1
}

legend(-5, 5 ,legend = c('p = 1.1', 'p = 2', 'p = 100'), fill = 1:3)

