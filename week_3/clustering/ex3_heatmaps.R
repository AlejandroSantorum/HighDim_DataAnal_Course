library(GSE5859Subset)
data(GSE5859Subset)

#install.packages("matrixStats")
library(matrixStats)
library(gplots)

# Q1: Use heatmap.2 to make a heatmap showing the sampleInfo$group with color,
#     the date as labels, the rows labelled with chromosome, and scaling the rows.
#     What do we learn from this heatmap?

##load libraries
library(rafalib)
library(gplots)
library(matrixStats)
library(RColorBrewer)
##make colors
cols = colorRampPalette(rev(brewer.pal(11,"RdBu")))(25)
gcol=brewer.pal(3,"Dark2")
gcol=gcol[sampleInfo$g+1]

##make lables: remove 2005 since it's common to all
labcol= gsub("2005-","",sampleInfo$date)  

##pick highly variable genes:
sds =rowMads(geneExpression)
ind = order(sds,decreasing=TRUE)[1:25]

## make heatmap
heatmap.2(geneExpression[ind,],
          col=cols,
          trace="none",
          scale="row",
          labRow=geneAnnotation$CHR[ind],
          labCol=labcol,
          ColSideColors=gcol,
          key=FALSE)

### Q2
# Create a large data set of random data that is completely independent of sampleInfo$group like this:
set.seed(17)
m = nrow(geneExpression)
n = ncol(geneExpression)
x = matrix(rnorm(m*n),m,n)
g = factor(sampleInfo$g )

# Create two heatmaps with these data. Show the group g either with labels or colors.
# Which of the following statements is true
library(gplots)
library(matrixStats)
library(genefilter)
library(RColorBrewer)
cols = colorRampPalette(rev(brewer.pal(11,"RdBu")))(25)

ttest = rowttests(x,g)
sds = rowSds(x)
Indexes = list(t=order(ttest$p.value)[1:50], s=order(-sds)[1:50])
for(ind in Indexes){
  heatmap.2(x[ind,],
            col=cols,
            trace="none",
            scale="row",
            labCol=g,
            key=FALSE)
}






