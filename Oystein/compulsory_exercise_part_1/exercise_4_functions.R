
# start and end with index 1
distance_of_traveling = function(x, y, route) {
  
  # previous location is the start
  old_loc = 1
  
  dist = 0
  for(new_loc in route) {
    
    dist = dist + sqrt((x[new_loc] - x[old_loc])^2 + (y[new_loc] - y[old_loc])^2)
    old_loc = new_loc
  }
  
  # going back to start
  dist = dist + sqrt((x[1] - x[old_loc])^2 + (y[1] - y[old_loc])^2)
  
}

distance_of_traveling_max = function(x, y, route1, route2) {
  route1 = route1[route1 > 0]
  route2 = route2[route2 > 0]
  
  dist1 = distance_of_traveling(x, y, route1)
  dist2 = distance_of_traveling(x, y, route2)
  
  max(dist1, dist2)
}

# Function for visualize the optimized route
draw_route_of_traveling = function(x, y, route, wait = 0.3){
  
  plot(x, y, xlim = c(0, 1), ylim = c(0, 1))
  points(x[1], y[1], col = 'green')
  text(x[1], y[1] + 0.1, labels = 'S')
  
  old_loc = 1
  
  dist = 0
  for(new_loc in route) {
    
    lines(c(x[new_loc], x[old_loc]), c(y[new_loc],  y[old_loc]))
    old_loc = new_loc
    Sys.sleep(wait)
  }
  
  lines(c(x[1], x[old_loc]), c(y[1],  y[old_loc]))
}

draw_route_of_traveling_2 = function(x, y, route1, route2, wait = 0.3){
  
  # keeping just the non-zero values. 
  route1 = route1[route1 > 0]
  route2 = route2[route2 > 0]
  
  plot(x, y, xlim = c(0, 1), ylim = c(0, 1))
  points(x[1], y[1], col = 'green')
  text(x[1], y[1] + 0.1, labels = 'S')
  
  old_loc = 1
  
  dist = 0
  for(new_loc in route1) {
    
    lines(c(x[new_loc], x[old_loc]), c(y[new_loc],  y[old_loc]), col = 'orange')
    old_loc = new_loc
    Sys.sleep(wait)
  }
  
  lines(c(x[1], x[old_loc]), c(y[1],  y[old_loc]), col = 'orange')
  
  
  old_loc = 1
  dist = 0
  for(new_loc in route2) {
    
    lines(c(x[new_loc], x[old_loc]), c(y[new_loc],  y[old_loc]), col = 'red')
    old_loc = new_loc
    Sys.sleep(wait)
  }
  
  lines(c(x[1], x[old_loc]), c(y[1],  y[old_loc]), col = 'red')
}

# Every_other_zero - as long as it is possible
every_other_zero = function(n, array){
  new_array = c(rep(0, max(2 * length(array), n)))
  
  for (i in 1:length(array)) {
    new_array[2*i] = array[i]
  }
  
  array_len = length(new_array)
  while(array_len > n) {
    if (new_array[array_len] == 0 && new_array[array_len-1] == 0) {
      new_array = new_array[1:(array_len - 1)]
    } else {
      # removing one index with 0
      remove_index = sample((1:array_len)[new_array == 0], 1)
      new_array = new_array[1:array_len != remove_index]
    }
    array_len = array_len - 1
    
    
  }
  
  new_array
}

  



