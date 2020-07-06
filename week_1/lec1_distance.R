library(devtools)
# The data represent RNA expression levels for eight tissues, each with several individuals.
library(tissuesGeneExpression)
data(tissuesGeneExpression)

dim(e) ##e contains the expression data
tissue

table(tissue) ##tissue[i] tells us what tissue is represented by e[,i]

# Let’s compute distance between samples 1 and 2, both kidneys, and then to sample 87, a colon.
x <- e[,1]
y <- e[,2]
z <- e[,87]
tissue[c(1,2,87)]

sqrt(sum((x-y)^2))
sqrt(sum((x-z)^2))

sqrt(crossprod(x-y))
sqrt(crossprod(x-z))

# Now to compute all the distances at once, we have the function dist.
# Because it computes the distance between each row, and here we are interested in the distance between samples, we transpose the matrix
?dist
d <- dist(t(e))
class(d)

# Note that this produces an object of class dist and, to access the entries using row and column, indexes we need to coerce it into a matrix:
as.matrix(d)[1,2]
as.matrix(d)[1,87]
image(as.matrix(d))
