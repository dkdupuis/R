parDirLoc <- paste(getwd(),"/c1/", sep = "")
parDir <- dir(parDirLoc)

for(i in 1:length(parDir))
{
    
    subDirLoc <- paste(parDirLoc, parDir[i], "/", sep = "")
    subDir <- dir(subDirLoc)
    
    for(r in 1:length(subDir))
    {
        
        file <- paste(subDirLoc, subDir[r], sep = "")
        watchData <- read.timeseries(file)
        
        anchorDate <- paste("X",gsub("-",".",as.character(watchData[watchData$category=="anchor_date",]$label)),sep = "")
        
        dates <- detectTopics(file, daysToUse = 115, mainCatagoryLabel = "watchlist_score", useDC = FALSE) #+ 1
        
        print(paste("Topic - ", as.character(watchData[watchData$category=="watchlist_score",]$label),sep = ""))
        topicID <- watchData[watchData$category=="watchlist_topic",]$uuid
        
        burstScores <- burstOutliers(file,filterToUse = edgeFilter(), daysToWeigh = 1, daysToUse = 116, burstParam = .5)
        
        for(t in 1:length(dates))
        {
            colDate <- paste("X", gsub("-", ".", dates[t]), sep = "")
            rowOfMax <- which.max(burstScores[,colnames(burstScores)==colDate])
            
            infID <- as.character(burstScores$uuid[rowOfMax])   
            print(dates[t])
            print(paste("Influencer - ",as.character(burstScores$label[rowOfMax]),sep = ""))
            
            bashCom <- paste("bash explaininf.sh",topicID,dates[t],infID, sep = " ")
            print(system(bashCom, intern = TRUE))
            
        }
    }
}