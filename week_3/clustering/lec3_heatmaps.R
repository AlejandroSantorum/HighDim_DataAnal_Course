library(tissuesGeneExpression)
data(tissuesGeneExpression)
image(e[1:100,])

# Let’s begin by defining a color palette:
library(RColorBrewer) 
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)

# Now, pick the genes with the top variance over all samples:
library(Biobase)
library("BiocManager")
library(genefilter)
library(parathyroidSE)
rv <- rowVars(e)
idx <- order(-rv)[1:40]

library(gplots) ##Available from CRAN
library(rafalib)
cols <- palette(brewer.pal(8, "Dark2"))[as.fumeric(tissue)]
head(cbind(colnames(e),cols))

heatmap.2(e[idx,], labCol=tissue,
          trace="none", 
          ColSideColors=cols, 
          col=hmcol)
