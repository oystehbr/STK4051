# The expectation of the expression (using the hint)
E = function(y, squared_tau){
  return(y * squared_tau/(squared_tau + 1))
}

# The probability part is (look up def of p_i in overleaf):
prob_C_1 = function(y, squared_tau, p) {
  return(1 - p_i(y, squared_tau, p))
}

# estimator
beta_estimator = function(y, squared_tau, p){
  return(prob_C_1(y, squared_tau, p) * E(y, squared_tau))
}



