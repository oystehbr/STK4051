source('exercise_1_functions.R')
sparseData = read.table("data/sparseDataWithErrors.ascii", header=F)
colnames(sparseData) = c('betaGT', 'y')

# Collecting the data from the dataframe
y = sparseData$y
betaGT = sparseData$betaGT

# the likelihood in 2a
loglikelihood = function(y, p, squared_tau) {
  fy = p*dnorm(y, mean = 0, sd = 1) + (1-p)*dnorm(y, mean = 0, sd = sqrt(squared_tau + 1))
  log_fy = log(fy)
  
  return(sum(log_fy))
}

p = seq(0.8, 1, length = 101)
squared_tau = seq(50, 130, length = 101)

z = matrix(1:(101*101), length(squared_tau), length(p))
for(i in 1:length(squared_tau)) {
  for(j in 1:length(p)){
    z[i,j] = loglikelihood(y, p[j], squared_tau[i])
  }
}

# need a transformation to [0, 1] - where 1 is the best value - max(z)
z = max(z)/z

levels_ = c(0.01, 0.1, 0.5, 0.75, 0.95, 0.98, 0.99, 0.9999,1)
contour(squared_tau, p, z, lwd = 2, 
        ylab = 'p', xlab = 'squared_tau', levels = levels_)
filled.contour(squared_tau, p, z, ylab = 'p',
               xlab = 'squared_tau', levels = levels_) #nlevels = 50)

# Mark the ML estimator in the plot
idx = which(z == max(z), arr.ind = TRUE)
points(squared_tau[idx[1]], p[idx[2]])




