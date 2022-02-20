source('exercise_4_functions.R')
optimalTransport = read.table("data/optimalTransport.ascii", header=F)
colnames(optimalTransport) = c('x', 'y')

# x and y are positions (distance is time)
x = optimalTransport$x
y = optimalTransport$y

# distance: 4.269566
# best_to_now = c(17, 12, 9, 15, 18, 14, 13, 3, 16, 11, 6, 2, 10, 7, 4, 8, 21, 20, 5, 19)

# distance 4.169531
best_to_now = c(19, 21, 8, 4, 7, 20, 10, 5, 2, 6, 11, 16, 3, 13, 14, 18, 15, 9, 12, 17)

if (TRUE){
  route.current = best_to_now
} else {
  route.current = sample(2:length(x), replace = FALSE) 
}
n = length(route.current)

# We measure the model in the distance of the route (shorter => better)
dist_current = distance_of_traveling(x, y, route.current)

# Wish to have control of the distance over time
dist_seq = c()

# Initialization for convergence criterion
max_iter = 10000
iter = 1
converged = FALSE
same_value = 0
max_same_value = 200
last_dist = 0

# simulated annealing
while(!(converged) && iter < max_iter)
  {
    # the cooling
    tau = 100/(iter+1)
    
    # collecting two indexes random, and switch those
    index_of_switching = sample(1 : n, 2, replace = FALSE)
    
    value_1 = route.current[index_of_switching[1]]
    value_2 = route.current[index_of_switching[2]]
    route.neighboor = route.current
    route.neighboor[index_of_switching[1]] = value_2
    route.neighboor[index_of_switching[2]] = value_1
    
    dist_neighboor = distance_of_traveling(x, y, route.neighboor)
  
    # The probability of switching the model
    prob = exp((dist_current - dist_neighboor)/tau)
    
    # We will switch to newer model if it is better (sometimes if it is worse)
    u = runif(1)
    if(u<prob)
    {
      route.current = route.neighboor
      dist_current = dist_neighboor
    }
    
    # adding the distance to see the improvement of the model
    dist_seq = c(dist_seq, dist_current)
    
    # check if we have had an change in the parameters
    if (last_dist == dist_current){
      same_value = same_value + 1
    } else {
      same_value = 0
      last_dist = dist_current
    }
    
    # If we have same parameters for "max_same_value" rounds, then stop the iterations
    if (same_value == max_same_value) {
      converged = TRUE
    }
  
    iter = iter + 1
}

plot.ts(dist_seq)
show(min(dist_seq))

draw_route_of_traveling(x, y, route.current, wait = 0.3)
draw_route_of_traveling(x, y, best_to_now, wait = 0.3)


