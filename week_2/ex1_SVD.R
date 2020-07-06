library(tissuesGeneExpression)
data(tissuesGeneExpression)

# Compute the SVD of e
s = svd(e)
# Compute the mean of each row
m = rowMeans(e)

# Q1: What is the correlation between the first column of U and m?
cor(s$u[,1],m)

diag(s$d)%*%t(s$v)

newmeans = rnorm(nrow(e)) ##random values we will add to create new means
newe = e+newmeans ##we change the means
sqrt(crossprod(e[,3]-e[,45]))
sqrt(crossprod(newe[,3]-newe[,45]))
y = e - rowMeans(e)
s = svd(y)

resid = y - s$u %*% diag(s$d) %*% t(s$v)
max(abs(resid))


z = s$d * t(s$v)
sqrt(crossprod(e[,3]-e[,45]))
sqrt(crossprod(y[,3]-y[,45]))
sqrt(crossprod(z[,3]-z[,45]))
# Q4: What is the difference (in absolute value) between the actual distance sqrt(crossprod(e[,3]-e[,45])) and the approximation using only two dimensions of z?
realdistance = sqrt(crossprod(e[,3]-e[,45]))
approxdistance = sqrt(crossprod(z[1:2,3]-z[1:2,45]))
abs(realdistance - approxdistance)

# Q5: What is the minimum number of dimensions we need to use for the approximation in SVD Exercises #4 to be within 10% or less?
ks = 1:189
realdistance = sqrt(crossprod(e[,3]-e[,45]))
approxdistances = sapply(ks,function(k){
  sqrt(crossprod(z[1:k,3,drop=FALSE]-z[1:k,45,drop=FALSE] )) 
})
percentdiff = 100*abs(approxdistances - realdistance)/realdistance
plot(ks,percentdiff) ##take a look
min(ks[which(percentdiff < 10)])

# Compute distances between sample 3 and all other samples:
distances = sqrt(apply(e[,-3]-e[,3],2,crossprod))
# Recompute this distance using the 2 dimensional approximation.
approxdistances = sqrt(apply(z[1:2,-3]-z[1:2,3],2,crossprod))
plot(distances,approxdistances) ##take a look
# Q6: What is the Spearman correlation between this approximate distance and the actual distance?
cor(distances,approxdistances,method="spearman")