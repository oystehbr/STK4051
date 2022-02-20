
f = function(beta, gamma, p) {
  beta + gamma * sign(beta) * abs(beta) ^ (p-1)
}

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
    if (f_bi(beta_mid) * f_bi(beta1) < 0){
      beta2 = beta_mid
    } else if (f_bi(beta_mid) * f_bi(beta2) < 0) {
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