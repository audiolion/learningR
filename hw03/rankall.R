rankall <- function(outcome, num = "best"){
  states <- c(state.abb,"DC")
  states <- sort(states)
  df <- data.frame(matrix(ncol=2, nrow=0))
  colnames(df) <- c("hospital","state")
  counter <- 1
  for(i in states){
    vec <- rankhospital(i, outcome, num)
    df[counter,] <- vec
    counter <- counter+1
  }
  rownames(df) <- states
  return(df)
}

rankhospital <- function(state, outcome, num = "best"){
  wd <- getwd()
  file <- paste(wd, "/outcome-of-care-measures.csv", sep="")
  data <- read.csv(file,header=T,sep=",")
  if(sum(grep(state, data$State))==0){
    stop('invalid state')
  }
  outcome <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", outcome, perl=TRUE)
  outcome <- gsub(" ", ".", outcome)
  if(sum(grep(outcome, colnames(data)))==0){
    stop('invalid outcome')
  }
  statedata <- subset(data, data$State==state)
  outcome <- paste("^Hospital.30.Day.Death..Mortality..Rates.from.", outcome, "$", sep="")
  statedata.sub <- statedata[grepl(paste(outcome,"|^Hospital.Name$", sep=""), colnames(statedata))]
  statedata.sub[,2] = suppressWarnings(as.numeric(as.character(statedata.sub[,2])))
  order <- order(statedata.sub[,2], statedata.sub[,1])
  statedata.order <- statedata.sub[order,]
  if(is.character(num)){
    if(num == "best"){
      return(c(as.character(statedata.order$Hospital.Name[1]),state))
    }else if(num == "worst"){
      index <- which.max(statedata.order[,2])
      return(c(as.character(statedata.order$Hospital.Name[index]),state))
    }else{
      stop('invalid num')
    }
  }else if(num <= nrow(statedata.order)){
    return(c(as.character(statedata.order$Hospital.Name[num]),state))
  }else{
    return(c("<NA>",state))
  }
}