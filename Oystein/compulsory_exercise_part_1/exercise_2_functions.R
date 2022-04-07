p_i = function(y_i, squared_tau, p) {
  numerator = dnorm(y_i, mean = 0, sd = 1) * p
  denominator = numerator + dnorm(y_i, mean = 0, sd = sqrt(squared_tau + 1)) * (1 - p)
  
  numerator/denominator
}

p_update = function(y, squared_tau_cur, p_cur) {
  sum = 0
  for (y_i in y){
    sum = sum + p_i(y_i, squared_tau_cur, p_cur)
  }
  
  sum/length(y)
}

squared_tau_update = function(y, squared_tau_cur, p_cur) {
  numerator = 0
  denominator = 0
  for(y_i in y){
    numerator = numerator + (1 - p_i(y_i, squared_tau_cur, p_cur))*y_i^2
    denominator = denominator + (1 - p_i(y_i, squared_tau_cur, p_cur))
  }
  
  numerator/denominator - 1
}

EM_algorithm = function(y, p_1, squared_tau_1, printing = FALSE, eps = 10^-10, max_iter = 1000){
  
  iter = 1
  converged = FALSE
  while(!(converged) && iter < max_iter){
    squared_tau_2 = squared_tau_update(y, squared_tau_1, p_1)
    p_2 = p_update(y, squared_tau_1, p_1)
    
    # check if both of the parameters have converged
    if(abs(p_2 - p_1) + abs(squared_tau_2 - squared_tau_1) < eps) {
      converged = TRUE
    }
    
    if(printing){
      save = sprintf('iter: %2.0f, %f %f', iter, p_1, squared_tau_1)
      print(save)
    }
    
    # updating the parameters
    squared_tau_1 = squared_tau_2
    p_1 = p_2
    
    iter = iter + 1
  }
  
  c(p_2, squared_tau_2)
}


