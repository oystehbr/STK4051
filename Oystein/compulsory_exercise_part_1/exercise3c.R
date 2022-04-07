source('exercise_2_functions.R')
source('exercise3_functions.R')

# Reading in the data
sparseData = read.table("data/sparseDataWithErrors.ascii", header=F)
colnames(sparseData) = c('betaGT', 'y')

# Collecting the data from the dataframe
y = sparseData$y
betaGT = sparseData$betaGT

p = 0.9
squared_tau = 80

# find the sum of squares residuals
beta_estimates = beta_estimator(y, squared_tau, p)

residuals = sum((beta_estimates-betaGT)^2)



