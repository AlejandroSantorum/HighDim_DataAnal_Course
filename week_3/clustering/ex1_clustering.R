set.seed(1)
m = 10000
n = 24
x = matrix(rnorm(m*n),m,n)
colnames(x)=1:n

# Q1: From the dendrogram which pairs of samples are the furthest away from each other?
d <- dist( t(x))
hc = hclust(d)
plot(hc) # Answer: 9 and 17

# Q2: Based on the Monte Carlo simulation, what is the standard error of this random variable?
set.seed(1)
m = 10000
n = 24
nc = replicate(100,{
  x = matrix(rnorm(m*n),m,n)
  hc = hclust( dist( t(x)))
  length(unique(cutree(hc,h=143)))
})
plot(table(nc)) ## look at the distribution

library(rafalib)
popsd(nc)
