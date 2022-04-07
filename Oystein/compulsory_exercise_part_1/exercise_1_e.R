# Reading in the data
source('exercise_1_functions.R')
sparseData = read.table("data/sparseDataWithErrors.ascii", header=F)
colnames(sparseData) = c('betaGT', 'y')

# Collecting the data from the dataframe
y = sparseData$y
betaGT = sparseData$betaGT

# We want to estimate beta_optimal for penalized regression
gamma = 1
ps = c(1.1, 2, 100)
init_beta1 = -100
init_beta2 = 101
residuals = c()
beta_penelized = list()

# Finding the residuals for different p-values
for (p in ps){
  estimation = c()
  for (y_i in y){
    estimation = c(estimation, bisection_method(y_i, gamma, p, init_beta1, init_beta2))
  }
  
  # comparing the estimation vs  the ground truth
  beta_penelized = c(beta_penelized, list(estimation))
  residuals = c(residuals, sum((estimation-betaGT)^2))
}

residuals = data.frame(ps, residuals)


# Comparison to the MLE estimator, just equal to y
beta_MLE = y
residual_MLE = sum((beta_MLE - betaGT)^2)

# Estimate beta by using the residuals
y_list3 = rep(list(y), 3)

beta_alternative = list()
residuals_alternative = rep(NA, 3) 
for (i in 1:3) {
  beta_alternative = c(beta_alternative, list(unlist(y_list3[i]) - unlist(beta_penelized[i])))
  residuals_alternative[i] = sum((unlist(beta_alternative[i]) - betaGT)^2)
}

residuals_alternative_table = data.frame(ps, residuals_alternative)

# First case
residuals
# Second case
residual_MLE
# Third case
residuals_alternative







