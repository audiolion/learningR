source("~/[BIOL230] Bioinformatics Languages (R)/lab5/lab05_Castner_Ryan.R")
helaData <- list.files(path="~/helaData", full.names=T, recursive=FALSE)
results <- lapply(helaData, processFile, window=30)

rbindResults <- do.call(rbind, results)

binnedResultsA <- rbindResults[1:201,]
binnedResultsB <- rbindResults[202:401,]
binnedResultsC <- rbindResults[402:601,]
binnedResultsD <- rbindResults[602:801,]
binnedResultsE <- rbindResults[802:1000,]

avgA <- colMeans(binnedResultsA)
avgB <- colMeans(binnedResultsB)
avgC <- colMeans(binnedResultsC)
avgD <- colMeans(binnedResultsD)
avgE <- colMeans(binnedResultsE)


avgs <- cbind.data.frame(avgA, avgB, avgC, avgD, avgE)

names(avgs) <- LETTERS[1:5]

lineGraph(avgs, "lapply.png")

library(parallel)
parallelResults = mclapply(helaData, processFile, window=61)
rbindParallel <- do.call(rbind, parallelResults)

binnedParallelA <- rbindParallel[1:201,]
binnedParallelB <- rbindParallel[202:401,]
binnedParallelC <- rbindParallel[402:601,]
binnedParallelD <- rbindParallel[602:801,]
binnedParallelE <- rbindParallel[802:1000,]

avgParaA <- colMeans(binnedParallelA)
avgParaB <- colMeans(binnedParallelB)
avgParaC <- colMeans(binnedParallelC)
avgParaD <- colMeans(binnedParallelD)
avgParaE <- colMeans(binnedParallelE)


avgParas <- cbind.data.frame(avgParaA, avgParaB, avgParaC, avgParaD, avgParaE)

names(avgParas) <- LETTERS[1:5]

lineGraph(avgParas, "mclapply.png")