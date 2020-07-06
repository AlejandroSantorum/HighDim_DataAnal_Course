library(tissuesGeneExpression)
data(tissuesGeneExpression)

y = e - rowMeans(e)
s = svd(y)
z = s$d * t(s$v)

library(rafalib)
ftissue = factor(tissue)
mypar(1,1)
plot(z[1,],z[2,],col=as.numeric(ftissue))
legend("topleft",levels(ftissue),col=seq_along(ftissue),pch=1)

d = dist(t(e))
mds = cmdscale(d)
# Q1: What is the correlation between the first row of z and the first column od mds?
cor(z[1,], mds[,1])
# Q2: What is the correlation between the second row of z and the second column od mds?
cor(z[2,],mds[,2])

# Note that the mds plot is not the same:
library(rafalib)
ftissue = factor(tissue)
mypar(1,2)
plot(z[1,],z[2,],col=as.numeric(ftissue))
legend("topleft",levels(ftissue),col=seq_along(ftissue),pch=1)
plot(mds[,1],mds[,2],col=as.numeric(ftissue))

library(GSE5859Subset)
data(GSE5859Subset)
dim(geneExpression)

s = svd(geneExpression-rowMeans(geneExpression))
z = s$d * t(s$v)
# Q4: Which dimension of z most correlates with the outcome sampleInfo$group?
which.max(cor(sampleInfo$g,t(z)))

# Q5: What is this max correlation?
max(cor(sampleInfo$g,t(z)))

# Q6: Which dimension of z has the second highest correlates with the outcome sampleInfo$group?
which.max(cor(sampleInfo$g,t(z))[-1]) + 1 #We add 1 because we took out the first.

#Note these measurements were made during two months:
sampleInfo$date
#We can extract the month this way:
month = format( sampleInfo$date, "%m")
month = factor( month)
month

# Q7.1: Which dimension of z has the highest correlates with the outcome month
which.max(cor( as.numeric(month), t(z)))
# Q7.2: What is this correlation?
max(cor( as.numeric(month), t(z)))

# ADVANCED QUESTION
result = split(s$u[,6],geneAnnotation$CHR)
result = result[ which(names(result)!="chrUn") ]
boxplot(result,range=0)
boxplot(result,range=0,ylim=c(-0.025,0.025))
medians = sapply(result,median)
names(result)[ which.max(abs(medians)) ]
