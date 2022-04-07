sigmoid = function(x) {
  x[x < 0] = 0
  return(x)
}

derivative_sigmoid = function(x) {
  x[x > 0] = 1
  x[x < 0] = 0
  
  return(x)
}

f = function (x, alpha_weights, beta_weights, alpha_bias, beta_bias) {
  sum_ = rep(0, 50)
  for (i in 1:length(beta_weights)) {
    sum_ = sum_ + beta_weights[i] * sigmoid(alpha_weights[i]*x + alpha_bias[i])
  }
  
  return(sum_ + beta_bias)
}

sse = function(architecture, x, y, alpha_weights, beta_weights, 
               alpha_bias, beta_bias) {
  predicted_values = architecture(x, alpha_weights, beta_weights, alpha_bias, beta_bias)
  
  return(sum((y - predicted_values)^2))
}

sgd_update_all_parameters = function(x, y, alpha_weights, beta_weights, 
                                     alpha_bias, beta_bias, lambda) {
  # calculating the gradients
  alpha_weights.grad = rep(0, 50)
  beta_weights.grad = rep(0, 50)
  alpha_bias.grad = rep(0, 50)
  f_value = f(x, alpha_weights, beta_weights, alpha_bias, beta_bias)
  for (j in 1:50) {
    alpha_weights.grad[j] = 2 * sum((y - f_value)
                                      * (-1) * (beta_weights[j] * 
                                   derivative_sigmoid(alpha_weights[j]*x + alpha_bias[j])*x))
    beta_weights.grad[j] = 2 * sum((y - f_value)
                                     * (-1) * sigmoid(alpha_weights[j]*x + alpha_bias[j]))
    alpha_bias.grad[j] = 2 * sum((y - f_value)
                                   * (-1) * (beta_weights[j] * 
                                derivative_sigmoid(alpha_weights[j]*x + alpha_bias[j])))
  }
  beta_bias.grad = 2 * sum(y - f_value) * (-1)
  
  # updating the values
  alpha_weights = alpha_weights - lambda * alpha_weights.grad
  beta_weights = beta_weights - lambda * beta_weights.grad
  alpha_bias = alpha_bias - lambda * alpha_bias.grad
  beta_bias = beta_bias - lambda * beta_bias.grad
  
  batch_error = sse(f, x, y, alpha_weights, beta_weights, 
                    alpha_bias, beta_bias)
  
  return(list(batch_error, alpha_weights, beta_weights, 
         alpha_bias, beta_bias))
}




