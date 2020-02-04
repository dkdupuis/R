# Time Series Project - Engineering Handoff
Dave DuPuis  
`r format(Sys.time(), '%B %d, %Y')`  


### Test Script



```r
    file <- "http://***.txt"
    data <- read.timeseries(file)
    
    writeLines("Calling flagDates with the Topic Score and edgeAndSecondDerivFilter\n")
    flagDates(data[3,4:ncol(data)],edgeAndSecondDerivFilter())
    
    writeLines("\nconvolveData test with inputs [5 10 2 4 6 1 8] and [1.125 -1.500  0.375]\n")
    convolveData(c(5,10,2,4,6,1,8),edgeAndSecondDerivFilter())
    
    writeLines("\nedgeAndSecondDerivFilter\n")
    edgeAndSecondDerivFilter()
    
 #   writeLines("\nburstFilter (NOT NEEDED FOR INTIAL STORY)\n")
 #   burstFilter()
```

```
## Calling flagDates with the Topic Score and edgeAndSecondDerivFilter
## 
##        dates  zScores
## 1 2015-08-13 2.499075
## 2 2015-07-13 5.387664
## 3 2015-05-11 2.337042
## 4 2015-05-05 5.147853
## 5 2015-05-04 7.901962
## 6 2015-04-29 2.829880
## 
## convolveData test with inputs [5 10 2 4 6 1 8] and [1.125 -1.500  0.375]
## 
## [1] -8.625  9.750 -1.500 -4.125  8.250
## 
## edgeAndSecondDerivFilter
## 
## [1]  1.125 -1.500  0.375
```




### flagDates

Purpose: to identify dates of interest in a timeseries
Takes: data = a time series of data with most recent days on the left/first; windowFilter = filter/mask/kernal to convolve with data before z-scoring; cutoff = all dates with z-scores about this value are returned
Returns: A data frame with the flagged dates and the scores on those dates
Notes: oldest length(windowFilter)-1 dates are unassessed as convolution creates edge effects; length(data) should be greater than or equal to length(windowFilter), or no dates will be assessed



```r
flagDates <- function(data, windowFilter, cutoff = 2.17)
{
    
    columnNames <- colnames(data)
    numData <- as.numeric(data)
    
    #convolve data with the window filter
    integralData <- convolveData(numData, windowFilter)
    
    #Get standard deviation of the convolved data
    integralSD <- sd(integralData)
    #Get mean of convolved data
    integralMean <- mean(integralData)

    #Calculate Z score for each day on the convolved data
    integralZScore <- (integralData - integralMean)/integralSD
    
    #Return data frame of the dates whose z-scores cross the threshold as well as the z-score on those days
    dfToReturn <- data.frame(dates = as.Date(substr(columnNames[integralZScore>cutoff],2,11),"%Y.%m.%d"), zScores = integralZScore[integralZScore>cutoff])
    
    return(dfToReturn)
}
```


### convolveData

takes a times series and a windowFilter
returns convolution of times series and windowFilter, removes oldest length(windowFilter)-1 days (to remove edge effects)
data should be at least as long as the windowFilter or no data will be returned



```r
convolveData <- function(data, windowFilter)
{
    #get length of windowFilter
    lengthWin <- length(windowFilter)
    #get length of time-series data
    lengthData <- length(data)
    
    #get length of output data (convolution produces edge effects and first length(WindowFilter)-1 days will be removed
    integralDataLength <- (lengthData-lengthWin+1)
    #create vector for output data
    integralData <- vector("numeric",integralDataLength)
    
    #for each day in the output data series...
    for(i in 1:integralDataLength)
    {
        #define result as the dot product of most recent length(windowFilter) days and the windowFilter
        integralData[i] <- data[i:(i+lengthWin-1)]%*%windowFilter
    }
    
    return(integralData)
}
```


### edgeAndSecondDerivFilter


Purpose: A filter used to detect rapid shifts in a time-series
Notes: Sum of filter = 0 making filter robust to trend changes in the underlying time-series data; Filter is 50-50 combination of normalized  edge detection & second derivative filters (1.5,-1.5, 0; .75, -1.5, .75)


```r
edgeAndSecondDerivFilter <- function()
{
    return(c(1.125,-1.5,.375))
}
```


### burstFilter

**Not Needed for intial story**

Purpose: Provide a filter to rank items based on the prevelence of activity over the last week
Notes: Sum of filter = 0 making filter robust to trend changes in the underlying time-series data; Filter is a gravity function (idea re-purposed from hackernews' ranking algorithm http://amix.dk/blog/post/19574)


```r
burstFilter <- function()
{
    start = 6
    gravWinLen = 5
    winLen = 14
    expon = -3.2
    
    burstWin <- c((start:(start+gravWinLen-1))^(expon)/(sum((start:(start+gravWinLen-1))^(expon))), rep(0,winLen-gravWinLen))-1/(winLen)
    
    return(burstWin)
}
```



### Helper function to read in data


```r
read.timeseries <- function(file) 
{
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
```
---

```r
sessionInfo()
```

```
## R version 3.2.2 (2015-08-14)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.5 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] magrittr_1.5    formatR_1.2.1   tools_3.2.2     htmltools_0.2.6
##  [5] yaml_2.1.13     stringi_0.5-5   rmarkdown_0.8   knitr_1.11     
##  [9] stringr_1.0.0   digest_0.6.8    evaluate_0.8
```
