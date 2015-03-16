best <- function(state, outcomeName){
  wd <- getwd()
  file <- paste(wd, "/outcome-of-care-measures.csv", sep="")
  data <- read.csv(file,header=T,sep=",")
  if(sum(grep(state, data$State))==0){
    return(paste("Error in best(",state,", ",outcomeName,") : invalid state", sep=""))
  }
  outcomeName <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", outcomeName, perl=TRUE)
  outcomeName <- gsub(" ", ".", outcomeName)
  if(sum(grep(outcomeName, colnames(data)))==0){
    return(paste("Error in best(",state,", ",outcomeName,") : invalide outcome", sep=""))
  }
  statedata <- subset(data, data$State==state)
  outcomeName <- paste("^Hospital.30.Day.Death..Mortality..Rates.from.", outcomeName, "$", sep="")
  statedata.sub <- statedata[grepl(paste(outcomeName,"|^Hospital.Name$",sep=""), colnames(statedata))]  
  bestHospital <- suppressWarnings(which.min(as.vector(statedata.sub[,2])))
  return(suppressWarnings(as.vector(statedata.sub$Hospital.Name[bestHospital])))
}