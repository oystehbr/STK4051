source('exercise_4_functions.R')
optimalTransport = read.table("data/optimalTransport.ascii", header=F)
colnames(optimalTransport) = c('x', 'y')

# x and y are positions (distance is time)
x = optimalTransport$x
y = optimalTransport$y

# distance 3.770697
best_to_now = c(19, 14, 13, 3, 16, 11, 6, 2, 5, 10, 20, 7, 4, 21, 8, 12, 18, 15, 9, 17)

# distance 4.169531 fox?
# best_to_now3 = c(19, 21, 8, 4, 7, 20, 10, 5, 2, 6, 11, 16, 3, 13, 14, 18, 15, 9, 12, 17)

# If true start from the best to now solution
if (FALSE){
  route.current = best_to_now
} else {
  route.current = sample(2:length(x), replace = FALSE) 
}
n = length(route.current)

# We measure the model in the distance of the route (shorter => better)
dist_current = distance_of_traveling(x, y, route.current)

# Wish to have control of the distance over time
dist_seq = c()

# want to save the best run so far
best_route_now = route.current 
best_dist_now = dist_current

# Initialization for convergence criterion
max_iter = 2000
iter = 1
converged = FALSE
same_value = 0
max_same_value = 200

possible_changes = c()
for(i in 1: 19){
  for(j in (i+1): 20) {
    possible_changes = c(possible_changes, list(c(i, j)))
  }
}

max_threshold_tabu = 50
tabu_list = c(list(1, 5))

# tabu algorithm
while(!(converged) && iter < max_iter)
{
  # first go through all non-listed (tabu) - check for the best switch
  neighboor_best = 100000
  for (switch_indexes in possible_changes) {
    # making copy of the current solution
    route.test = route.current
    value1 = route.current[switch_indexes[1]]
    value2 = route.current[switch_indexes[2]]
    route.test[switch_indexes[2]] = value1
    route.test[switch_indexes[1]] = value2
    dist.test = distance_of_traveling(x, y, route.test)
    
    # If it is not tabu, we will check if it is the best of the available neighboors so far
    if (!(list_is_contained(switch_indexes, tabu_list))) {
      if (dist.test < neighboor_best) {
        best_switch = switch_indexes
        neighboor_best = dist.test
      }
    }
    
    # want to store the best one yet, although it is tabu
    if (dist.test < best_dist_now) {
      best_route_now = route.test
      best_dist_now = dist.test
    }
  }
  
  # switching to the solution to best neighboor, (although it is worse than this state)
  value1 = route.current[best_switch[1]]
  value2 = route.current[best_switch[2]]
  route.current[best_switch[1]] = value2
  route.current[best_switch[2]] = value1
  dist_current = distance_of_traveling(x, y, route.current)
  dist_seq = c(dist_seq, dist_current)
  
  # updating the tabu_list, oldest tabu out if exceeding the threshold
  tabu_list = c(tabu_list, list(c(best_switch[1], best_switch[2])
                  [order(c(best_switch[1], best_switch[2]))]))
  if (length(tabu_list) > max_threshold_tabu) {
    tabu_list = tabu_list[-1]
  }
  
  iter = iter + 1
}

plot.ts(dist_seq)
show(min(dist_seq))
show(distance_of_traveling(x, y, best_route_now))
show(distance_of_traveling(x, y, best_to_now))

#draw_route_of_traveling(x, y, route.current, wait = 0.3)
draw_route_of_traveling(x, y, best_route_now, wait = 0.3)
draw_route_of_traveling(x, y, best_to_now, wait = 0.3)


