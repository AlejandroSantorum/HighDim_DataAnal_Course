library(tissuesGeneExpression)
data(tissuesGeneExpression)

d <- dist( t(e) )
head(d)

library(rafalib)
mypar()
hc <- hclust(d)
hc

plot(hc,labels=tissue,cex=0.5)

myplclust(hc, labels=tissue, lab.col=as.fumeric(tissue), cex=0.5)
abline(h=120)

hclusters <- cutree(hc, h=120)
table(true=tissue, cluster=hclusters)

hclusters <- cutree(hc, k=8)
table(true=tissue, cluster=hclusters)
