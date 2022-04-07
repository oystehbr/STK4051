source('exercise5_functions.R')
sparseData = read.table("data/functionEstimationNN.ascii", header=F)
colnames(sparseData) = c('x', 'y', 'fGT')

x = sparseData$x
y = sparseData$y
fGT = sparseData$fGT
n = length(x)

learning_rate = 1e-4
num_epochs = 100

alpha_weights.current = rnorm(50)
beta_weights.current = rnorm(50)
alpha_bias.current = rnorm(50)
beta_bias.current = rnorm(1)

batch_size = 50
amount_of_batches = n/ batch_size

# SGD-algorithm
test_error = c()
for (epoch in 1:num_epochs) {
  for (j in 1:amount_of_batches){
    batch = sample(1:n, batch_size)
    
    x_batch = x[batch]
    y_batch = y[batch]
    
    output = sgd_update_all_parameters(x_batch, y_batch, 
                              alpha_weights.current, beta_weights.current,
                              alpha_bias.current, beta_bias.current,
                              learning_rate)
    
    alpha_weights.current = unlist(output[2])
    beta_weights.current = unlist(output[3])
    alpha_bias.current = unlist(output[4])
    beta_bias.current = unlist(output[5])
  }
  
  epoch_error = sse(f, x, y, alpha_weights.current, beta_weights.current, 
                          alpha_bias.current, beta_bias.current)

  print(sprintf('epoch: %d | error : %f', epoch, epoch_error))
  test_error = c(test_error, epoch_error) 
  
  # Learning rate scheduler
  learning_rate = learning_rate * 0.97
  
}
plot(test_error, type = 'l', ylab = 'epoch_error', xlab = 'epoch')

predicted_values = f(x, alpha_weights.current, beta_weights.current, alpha_bias.current, beta_bias.current)

plot(x, y, col = 2)
points(x, predicted_values, col = 3)
legend(3, 1, legend = c('True', 'Predicted'), lty = c(1, 1) ,col = c(2, 3))


