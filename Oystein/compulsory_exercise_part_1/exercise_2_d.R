
source('exercise_2_functions.R')
sparseData = read.table("data/sparseDataWithErrors.ascii", header=F)
colnames(sparseData) = c('betaGT', 'y')

# Collecting the data from the dataframe
y = sparseData$y
betaGT = sparseData$betaGT

# need to take an initialization away from class 0
squared_tau_init = mean(y)
p_init = 0.5

# running the EM-algorithm, and collecting the results
values = EM_algorithm(y, p_init, squared_tau_init, printing = TRUE)
p_opt = values[1]
squared_tau_opt = values[2]
