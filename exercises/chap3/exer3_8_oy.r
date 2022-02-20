#Exercise 3.8
wine = read.table("wine.dat",header=T)

#Calculating objective function
rss = function(x,lab)
{
 rss = 0
 for(j in 1:3)
 {
   # check the guess of the labels, collect data and mean
   z = x[lab==j,]
   mu = colMeans(z)
#   show(z)
#   show(mu)
   # t(z) is the transpose of z
#   show(dim(res))
   
   # calculating the error (difference from mean to the data in the "cluster")
   res = t(z)-mu
   rss = rss + sum(colSums(res^2))
 }
 rss
}

#Initialization
n = nrow(wine)

# Starting with random labeling of the data
lab = sample(1:3,n,replace=T)
#lab = wine[,1]

# calculate the error with the initialize labels
rss.cur = rss(wine[,-1],lab)

#Defining neighborhood to be change of one label

#Steepest descent
more = TRUE;r=rep(NA,3)
while(more)
{
  more = FALSE
  # for all i, switch the label to be the best case
  for(i in 1:n)
  {
    lab.cur = lab[i]
    
    # Find the optimal label for i-th entry
    for(j in 1:3)
    {
      lab[i] = j
      r[j] = rss(wine[,-1],lab)
    }
    
    # Update the i-th label to the lowest error
    jopt = which.min(r)
    lab[i] = jopt
    
    # if some changes, run again
    if(jopt!=lab.cur)
      more = TRUE
  }
  show(min(r))
}
# show the confusion matrix
show(table(wine[,1],lab))

#Simulated annealing
lab = sample(1:3,n,replace=T)
rss.cur = rss(wine[,-1],lab)
for(it in c(1:100))
{
  temp = 10/(1+it)
  for(i in 1:n)
  {
    lab.cur = lab[i]
    lab[i] = sample(1:3,1)
    rss.prop = rss(wine[,-1],lab)
    if(runif(1)<exp(-(rss.prop-rss.cur)/temp))
      rss.cur = rss.prop
    else
      lab[i] = lab.cur
  }
  show(rss(wine[,-1],lab))
}
show(table(wine[,1],lab))

