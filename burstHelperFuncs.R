#takes:
    #a df of values where columns are time based and rows are unit to analyze
    #column 1=first time period
    #all rows represent the same type

#returns:
    #a column of z-scores (whose rows correspond to the input data frame)

scoreWeek <- function(dataToScore, x1 = .05)
{

    dataToScore$zFirstWeek <- (dataToScore[,1]-rowMeans(dataToScore))/apply(dataToScore,1,sd)
    dataToScore$zFirstWeek[is.na(dataToScore$zFirstWeek)] <- 0
    
    sumTot <- sum(dataToScore[,1])
    dataToScore$burstScore <- ((dataToScore[,1]/sumTot)^x1)*(dataToScore$zFirstWeek)
    
    return(dataToScore$burstScore)

}

#takes: Dataframe of daily data in the format category, xx, xx, day 1, day 2, ..., day N
#outputs dataframe of DC-adjusted data (DC calculated at the category level)

dailyComponent <- function(dataToDC) 
{
    dayCount <- vector("numeric", length= 7)
    dayDev <- matrix(0,nlevels(dataToDC$category),7)
    
    colTotals <- aggregate(dataToDC[,4:ncol(dataToDC)], by = list(dataToDC$category), FUN = sum)
        
    for(i in 1:(ncol(colTotals)-1))
    {
        dayCount[(i-1)%%7+1] <- dayCount[(i-1)%%7+1] + 1
        dayDev[,(i-1)%%7+1] <- dayDev[,(i-1)%%7+1] + colTotals[,i+1] #1st column is label; 2nd column corresponds to 1st day
    }
    
    dayDev <- dayDev/do.call("rbind", replicate(nrow(dayDev), dayCount, simplify = FALSE))
    dayDev <- dayDev/rowMeans(dayDev)
    
    #add in category label, repeat the DC to cover timeframe
    dayDev <- cbind(colTotals[,1],as.data.frame(rep(as.data.frame(dayDev),ceiling((ncol(dataToDC)-3)/7)))[,1:(ncol(dataToDC)-3)])
    names(dayDev)[1] <-"category"
    
    dayDevExpand <- join(dataToDC[,1:3],dayDev, by = "category", type = "left")
    
    dcedData <- cbind(dataToDC[,1:3],dataToDC[,4:ncol(dataToDC)]/dayDevExpand[,4:ncol(dataToDC)])

    return(dcedData)

}

#
#takes: matrix of daily data with columns as days and rows as units to analyze; parameter of convFilter (window function)
#Provides a score for each day that is a weighted average of the current day and the days preceding it
#Score on a given day is the dot product of the scores over the previous convFilterLength days and convFilter
#outputs score for each day (note first winLen - 1 days should be discarded)

convolveDays <- function(inputData, convFilter)
{
    #convWin <- (start:(start+winLen-1))^(expon)/(sum((start:(start+winLen-1))^(expon)))

    winLen <- length(convFilter)
    convolvedData <- apply(inputData,1,convolve,convFilter,type="open")
    
    return(t(convolvedData[winLen:(nrow(convolvedData)),]))
    
}

read.timeseries <- function(file) {
  ts <- read.delim(file, header=FALSE, quote = "")
  
  anchor <- as.character(subset(ts, V1 == "anchor_date")[1,3])
  if(gregexpr("-",anchor)[[1]][1] == 3)
  {
      anchor <- paste(substr(anchor,7,11),substr(anchor,1,5),sep = "-")
  }
  
  date <- as.Date(anchor)
  colnames <- character(ncol(ts))
  colnames[1:3] <- c("category", "uuid", "label")
  for (i in 4:ncol(ts)) {
    colnames[i] <- paste("X",gsub("-",".",as.character(date)),sep = "")
    date <- date - 1
  }
  colnames(ts) <- colnames
  
  ts[is.na(ts)] <- 0.0
  
  #reverse time function
  #tsRev <-  cbind(ts[,1:3],ts[,63:4])
  
  return(ts)
}
