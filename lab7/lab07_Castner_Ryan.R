source("http://bioconductor.org/biocLite.R")
biocLite("inSilicoDb")
biocLite("limma")
library("inSilicoDb")
library("limma")
library("gplots")
InSilicoLogin("rrc9704@rit.edu", "60173cbbcba96c4f80ba8b5941370799")
results = getDataset("GSE10006", "GPL570", norm="FRMA",features="gene")
experimentData(results)
smoker <- pData(results)[,"Smoker"]
mm <- model.matrix(~smoker)
fit <- lmFit(results, mm)
fit2 <- eBayes(fit)
res <- topTable(fit2, coef="smokersmoker", number=50)
top50 <- results[rownames(res)]
breaks <- 0:4
col <- rainbow((ncol(top50)-1))
matrix <- as.numeric(factor(pData(top50)[,"Smoker"]))-1
matrix <- as.matrix(matrix)
matrix <- cbind(matrix, rownames(top50))
#heatmap.2(matrix, breaks=ncol(), col=col)