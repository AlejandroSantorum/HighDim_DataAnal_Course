library(devtools) # get from CRAN with install.packages("devtools")
install_github("ririzarr/rafalib")
library(rafalib)
library(MASS)

n <- 100
set.seed(1)
y <- t(mvrnorm(n, c(0,0),
               matrix(c(1, 0.95, 0.95, 1),
                      2,2)))
mypar()
LIM <- c(-3.5, 3.5)
plot(y[1,], y[2,], xlim=LIM, ylim=LIM)

s <- svd(y)
PC1 <- s$d[1] * s$v[,1]
PC2 <- s$d[2] * s$v[,2]
plot(PC1, PC2, xlim=LIM, ylim=LIM)


library(tissuesGeneExpression)
data(tissuesGeneExpression)

set.seed(1)
ind <- sample(nrow(e), 500)
Y <- t(apply(e[ind,], 1, scale))

s <- svd(Y)
U <- s$u
V <- s$v
D <- diag(s$d)

Yhat <- U %*% D %*% t(V)
resid <- Y - Yhat
max(abs(resid))

plot(s$d)

# Reducing 4 dimensions
k <- ncol(U)-4
Yhat <- U[,1:k] %*% D[1:k,1:k] %*% t(V[,1:k])
resid <- Y - Yhat
max(abs(resid))
plot(s$d^2 / sum(s$d^2)*100)

# Reducing half of the dim
k <- ncol(U)-95
Yhat <- U[,1:k] %*% D[1:k,1:k] %*% t(V[,1:k])
resid <- Y - Yhat
max(abs(resid))
plot(s$d^2 / sum(s$d^2)*100)

boxplot(resid, ylim=LIM)

1-var(as.vector(resid))/var(as.vector(Y))

# Checking correlation
m <- 100
n <- 2
x <- rnorm(m)
e <- rnorm(n*m, 0, 0.01)
Y <- cbind(x,x)+e

cor(Y)

d <- svd(Y)$d
d[1]^2/sum(d^2)
