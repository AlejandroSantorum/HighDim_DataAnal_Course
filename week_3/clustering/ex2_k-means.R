library(GSE5859Subset)
data(GSE5859Subset)
library(rafalib)

set.seed(10)

d <- dist(t(geneExpression))
mds = cmdscale(d)
result = kmeans(t(geneExpression),5)
mypar(1,1)
plot(mds,bg=result$cl,pch=21)

table(sampleInfo$group,result$cluster)
table(sampleInfo$date,result$cluster)
##looks better if we re-order:
table(sampleInfo$date,result$cluster)[,c(4,1,5,3,2)]
