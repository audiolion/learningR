library("ggplot2")
source("~/lab06_Castner_Ryan1.R")

someWrapperFunction <- function(){
  helaData2 <- list.files(path="~/helaData2", full.names=T, recursive=FALSE)
  results <- lapply(helaData2, processFile, window=41)
  
  rbindResults <- do.call(rbind, results)
  
  binnedResultsA <- rbindResults[1:41,]
  binnedResultsB <- rbindResults[42:81,]
  binnedResultsC <- rbindResults[82:121,]
  binnedResultsD <- rbindResults[122:161,]
  binnedResultsE <- rbindResults[162:200,]
  
  avgA <- colMeans(binnedResultsA)
  avgB <- colMeans(binnedResultsB)
  avgC <- colMeans(binnedResultsC)
  avgD <- colMeans(binnedResultsD)
  avgE <- colMeans(binnedResultsE)
  
  
  avgs <- cbind.data.frame(avgA, avgB, avgC, avgD, avgE)
  
  names(avgs) <- LETTERS[1:5]
  
  return(avgs)
}

df <- someWrapperFunction()
ggBox <- function(df){
  g <- ggplot(df) + geom_boxplot(aes(x=1, y=df[,1], fill="A")) +
            geom_boxplot(aes(x=2, y=df[,2], fill="B")) + geom_boxplot(aes(x=3, y=df[,3], fill="C")) + 
            geom_boxplot(aes(x=4, y=df[,4], fill="D")) + geom_boxplot(aes(x=5,y=df[,5], fill="E")) + 
            scale_colour_manual(breaks=c("A","B","C","D","E"),
                                labels=c("A","B","C","D","E"),
                                values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))
  g <- g+ggtitle('Hela Data')+theme(plot.title = element_text(size=20, face="bold",vjust=2))
  g <- g+labs(x="Groups", y="Average Distribution")
  g <- g+scale_x_continuous(label=function(x){return(LETTERS[x])})
  g <- g+theme(legend.title=element_blank(), legend.key=element_rect(fill=NA))
  ggsave(file="boxplot-hela.png")
  
  print(g)
}

ggDense <- function(df){
  g <- ggplot(df) + geom_density(aes(x=df[,1], fill="A"), alpha=.5) +
    geom_density(aes(x=df[,2], fill="B"), alpha=.5) +
    geom_density(aes(x=df[,3], fill="C"), alpha=.5) +
    geom_density(aes(x=df[,4], fill="D"), alpha=.5) +
    geom_density(aes(x=df[,5], fill="E"), alpha=.5) +
    scale_colour_manual(breaks=c("A","B","C","D","E"),
                        labels=c("A","B","C","D","E"),
                        values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
    ggtitle('Hela Data Density Plot')+theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
    labs(x="Mean values",y="Density") + theme(legend.title=element_blank(), legend.key=element_rect(fill=NA))
  ggsave(file="densityplot-hela.png")
  print(g)
}

ggStat <- function(df){
  g <- ggplot(df) + geom_jitter(aes(x=df[,1],y=1.0,colour="A",group=1)) +
    geom_jitter(aes(x=df[,2],y=1.0,colour="B",group=1)) +
  geom_jitter(aes(x=df[,3],y=1.0,colour="C",group=1)) +
  geom_jitter(aes(x=df[,4],y=1.0,colour="D",group=1)) +
  geom_jitter(aes(x=df[,5],y=1.0,colour="E",group=1)) +
  scale_colour_manual(breaks=c("A","B","C","D","E"),
                      labels=c("A","B","C","D","E"),
                      values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
    ggtitle('Hela Data Scatterplot')+theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
    labs(x="Mean values",y="Display") + theme(legend.title=element_blank(), legend.key=element_rect(fill=NA)) +
  geom_smooth(method="gam", aes(x=(group=1),y=1)) + stat_smooth(method="gam", aes(x=(group=1),y=1))
  ggsave(file="scatterplot-hela.png")
  print(g)
}

ggBox(df)
ggDense(df)
ggStat(df)