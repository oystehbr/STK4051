
 
 # plot the function f(B)
 
f = function(beta, gamma, p) {
    beta + gamma * sign(beta) * abs(beta) ^ (p-1)
}


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


# Method for finding root of an expression
bisection_method= function(y, gamma, p, beta1, beta2, eps = 10^-10, max_iter = 1000) {
  
  # We want to find beta, s.t. f(beta) = y
  # f_bi - the function we want to use bisection method on
  f_bi = function(beta, y.=y, gamma.=gamma, p.=p){
    return(f(beta, gamma, p) - y)
  }
  
  # If one of the beta-values give 0, then we have the solution
  if (f_bi(beta1) * f_bi(beta2) == 0) {
    if (f_bi(beta1) == 0){
      beta1
    } else {
      beta2
    }
  }

  
  # If they have same sign, we are not guaranteed an 
  if (f_bi(beta1) * f_bi(beta2) > 0){
    stop('The initial guesses have the same sign of the function (not acceptable)')
  }

  converged = FALSE
  iter = 0
  while (!converged && iter < max_iter) {
    beta_mid = (beta1 + beta2) /2 
    
    # Checking if beta1, or beta2 should be updated, if opposite sign => keep them
    if (beta_mid * beta1 < 0){
      beta2 = beta_mid
    } else if (beta_mid * beta2 < 0) {
      beta1 = beta_mid
    } else {
      return(beta_mid)
    }
    
    
    # updating criterion of convergence 
    iter = iter + 1
    converged = abs(f_bi(beta_mid)) < eps
    
  }
  
  return(beta_mid)
}

# check the f_bi - function
# plot(beta, f_bi(beta, y, gamma, p), type = 'l')

# test bisection method
gamma = 1
ps = c(1.1, 2, 100)
y = seq(-5, 5, length = 101)

beta_init1 = -5
beta_init2 = 4.9

first = TRUE
for(p in ps){
  val = c()
  for (y_i in y){
    val = c(val, bisection_method(y_i, gamma, p, beta_init1, beta_init2))
  }
  
  if(first){
    plot(y, val, type='l', col = count, ylim = c(-8, 8), xlim = c(-5.1, 5.1), 
         lwd = 2, main = 'beta_opt w.r.t. y', xlab = 'y', ylab = 'beta_opt')
    first = FALSE
  } else {
    lines(y, val, col = count, lwd = 2)
  }
  
  count = count + 1
}


legend(-5, 1 ,legend = c('p = 1.1', 'p = 2', 'p = 100'), fill = 1:3)




# sparseData = read.table("data/sparseDataWithErrors.dat", header=T)

