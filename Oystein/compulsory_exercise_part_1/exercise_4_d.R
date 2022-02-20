source('exercise_4_functions.R')
optimalTransport = read.table("data/optimalTransport.ascii", header=F)
colnames(optimalTransport) = c('x', 'y')

# x and y are positions (distance is time)
x = optimalTransport$x
y = optimalTransport$y


places = sample(2:length(x), replace = FALSE) 
n = length(places)
random_places = sample(c(0, 1), n, replace = TRUE)

# Score: 2.3279
best_route1 = c(19, 14, 11, 16, 3, 13, 18, 9, 17, 15, 12)
best_route2 = c(21, 8, 4, 7, 20, 10, 6, 2, 5)

# We are either using the best one yet, or random (shuffle some 0's inside the array)
if(TRUE){
  route1.current = every_other_zero(n, best_route1)
  route2.current = every_other_zero(n, best_route2)
} else {
  # splitting 50/50 of the places to the routes
  route1.current = sample(c(places[1:10], rep(0, 10)))
  route2.current = sample(c(rep(0, 10), places[11:20]))
}


dist_current = distance_of_traveling_max(x, y, route1.current, route2.current)
#draw_route_of_traveling_2(x, y, route1.current, route2.current)

# want to save the best run so far
best_route1_now = route1.current 
best_route2_now = route2.current
best_dist_now = dist_current

# Wish to have control of the distance over time
dist_seq = c()

# Initialization for convergence criterion
max_iter = 100000
iter = 1
converged = FALSE
same_value = 0
max_same_value = 200
last_dist = 0

# TABU search
while(!(converged) && iter < max_iter)
{
  # the cooling
  tau = 100/(iter+1)
  
  # copying the current route
  route1.neighbor = route1.current
  route2.neighbor = route2.current
  
  # Finding two values to switch, in one case
  index_of_switching = sample(1 : n, 2, replace = FALSE)
  
  prob = runif(1)
  # Half the time, switch with its current cities
  if (prob > 2/3) {
    value_1 = route1.current[index_of_switching[1]]
    value_2 = route1.current[index_of_switching[2]]
    # Creating the neighborhood for route1
    route1.neighbor[index_of_switching[1]] = value_2
    route1.neighbor[index_of_switching[2]] = value_1
    
  } else if (prob > 1/3){
    value_1 = route2.current[index_of_switching[1]]
    value_2 = route2.current[index_of_switching[2]]
    # Creating the neighborhood for route2
    route2.neighbor[index_of_switching[1]] = value_2
    route2.neighbor[index_of_switching[2]] = value_1
    
  } else {
    # If not they above, then switching between the cars
    index_of_switching = sample(1 : n, 2, replace = FALSE)
    
    value_1 = route1.current[index_of_switching[1]]
    value_2 = route2.current[index_of_switching[2]]
    
    # switching the values
    route1.neighbor[index_of_switching[1]] = value_2
    route2.neighbor[index_of_switching[2]] = value_1
  }
  
  
  dist_neighbor = distance_of_traveling_max(x, y, route1.neighbor, route2.neighbor)
  
  # The probability of switching the model
  prob = exp((dist_current - dist_neighbor)/tau)
  
  # We will switch to newer model if it is better (sometimes if it is worse)
  u = runif(1)
  if(u<prob)
  {
    # updating the routes
    route1.current = route1.neighbor
    route2.current = route2.neighbor
    dist_current = dist_neighbor
    
    if (dist_current < best_dist_now) {
      best_route1_now = route1.current
      best_route2_now = route2.current
      best_dist_now = dist_current
    }
    
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

#draw_route_of_traveling_2(x, y, route1.current, route2.current, wait = 0.3)
draw_route_of_traveling_2(x, y, best_route1_now, best_route2_now, wait = 0.3)

