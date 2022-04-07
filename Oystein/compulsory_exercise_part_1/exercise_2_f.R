source('exercise_1_functions.R')
source('exercise_2_functions.R')

sparseData = read.table("data/sparseDataWithErrors.ascii", header=F)
colnames(sparseData) = c('betaGT', 'y')

# Collecting the data from the dataframe
y = sparseData$y
betaGT = sparseData$betaGT

# the optmial values from the EM-algorithm
p = 0.946004
squared_tau = 75.005977

complete_information_matrix = function(y, squared_tau, p){
  information = matrix(1:4, nrow = 2, ncol = 2)
  information[1, 2] = 0
  information[2, 1] = 0
  
  one_one = sum((1-p_i(y, squared_tau, p)) *
                  ((1/(2*(squared_tau + 1 )^2)) -
                  (y^2/((squared_tau + 1)^3)))
                )
  two_two = sum(-1/(p^2) * p_i(y, squared_tau, p) - 
                  1/((1-p)^2) * (1 - p_i(y, squared_tau, p)))
  
  information[1, 1] = - one_one
  information[2, 2] = - two_two
  return(information)
}

# The inverse of complete_information -> estimate of covariance matrix
# of the estimated parameters

estimated_covariance_matrix = function(y, squared_tau, p){
  return(solve(complete_information_matrix(y, squared_tau, p)))
}

covariance_matrix = estimated_covariance_matrix(y, squared_tau, p)

