library(devtools)
# The data represents RNA expression levels for seven tissues, each with several biological replictes.
# We call samples that we consider to be from the same population, such as liver tissue from different individuals,biological replictes:
library(tissuesGeneExpression)
data(tissuesGeneExpression)
head(e)
head(tissue)

# Q1: How many biological replicates for hippocampus?
sum(tissue=="hippocampus")
## to answer this question for all tissues look at
table(tissue)

# Q2: What is the distance between samples 3 and 45?
d <- dist(t(e))
as.matrix(d)[3,45]
#or
sqrt( crossprod(e[,3]-e[,45]) )
##or
sqrt( sum((e[,3]-e[,45])^2 ))

# Q3: What is the distance between gene 210486_at and 200805_at
x<-e["210486_at",]
y<-e["200805_at",]
sqrt( crossprod(x-y) )
#or
sqrt( crossprod(e["210486_at",]-e["200805_at",]) )
##or
sqrt( sum((e["210486_at",]-e["200805_at",])^2 ))

# Q4: If I run the command (don’t run it!) d = as.matrix(dist( e)).
# How many cells (number of rows times number of columns) would this matrix have?
22215*22215
##every pair of rows has an entry:
nrow(e)^2

# Q5: Compute the distance between all pairs of samples: Q:How many distances are stored in d? (Hint: What is the length of d)?
d = dist(t(e))
length(d)

# Q6: Why is the answer above not ncol(e)^2?
ncol(e)^2 #Incorrect
nrow(e)^2 #Incorrect
ncol(e)*(ncol(e)-1)/2 #Because we take advantage of symmetry: only lower triangular matrix is stored thus only 

