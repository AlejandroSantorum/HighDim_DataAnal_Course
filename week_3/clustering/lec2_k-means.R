library(tissuesGeneExpression)
data(tissuesGeneExpression)
library(rafalib)

set.seed(1)
km <- kmeans(t(e[1:2,]), centers=7)
names(km)

table(tissue,clusters=km$cluster)

mypar(1,2)
plot(e[1,], e[2,], col=as.fumeric(tissue), pch=16)
plot(e[1,], e[2,], col=km$cluster, pch=16)

table(true=tissue,cluster=km$cluster)

km <- kmeans(t(e), centers=7)
d<-dist(t(e))
mds <- cmdscale(d)

mypar(1,2)
plot(mds[,1], mds[,2]) 
plot(mds[,1], mds[,2], col=km$cluster, pch=16)

table(true=tissue,cluster=km$cluster)
