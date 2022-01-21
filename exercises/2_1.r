

cauchy = function(alpha, beta, x) {
    return(1/(pi*beta*(1+ (x - alpha)/beta )^2))
}

loglik = function(theta, x_values) {
    sum = 0
    for(i in 1:length(x_values)) {
        sum = sum + log(cauchy(theta, 1, x_values[i]))
    }
    return(sum)
}

score = function(theta, x) {
    sum =  0
    for (i in 1:length(x)) {
        sum = sum + 2*(x[i] - theta) / (1 + (x[i] - theta)^2)
    }
    return(sum)
}


score_j = function(theta, x) {
    sum = 0
    for(i in 1:length(x)) {
        sum = sum + (1 - (x[i] - theta)^2)/((1 + (x[i] - theta)^2)^2)
    }
    return(-sum)
}

newton_raphson = function(s, j, x_values, eps, N, init_theta) {
    previous_theta = init_theta
    diff = 100
    n = 0
    while(diff>eps && n<N) {
        next_theta = previous_theta - s(previous_theta, x_values)/j(previous_theta, x_values)

        if(abs(next_theta) > 100000) {
            return(NaN)
        }

        diff = abs(previous_theta - next_theta)
        previous_theta = next_theta
        n = n+1
     
    }

    cat("Init: ", init_theta, ", answer = ", next_theta, ",  diff = ", diff)
    return(next_theta)
}



x_values = c(1.77, -0.23, 2.76, 3.80, 3.47, 56.75, 
-1.34, 4.24, -2.44, 3.29, 3.71, -2.40, 4.53, 
-0.07, -1.05, -13.87, -2.53, -1.75, 0.27, 43.21)

# print(x_values) 

# theta_list = seq(-10, 10, 0.5)
# plot(theta_list, loglik(theta_list, x_values))

test_init_theta = c(-11, -1, 0, 1.5, 4, 4.7, 7, 8)
test_init_theta = c(4)
eps = 1e-10
N = 100000

for (init_theta in test_init_theta) {
    answer = newton_raphson(score, score_j, x_values, eps, N, init_theta)
    print(answer)
}



