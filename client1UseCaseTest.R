parDirLoc <- paste(getwd(),"/c2/", sep = "")
parDir <- dir(parDirLoc)

outDataFrame <- data.frame()

for(i in 1:length(parDir))
{
    
    subDirLoc <- paste(parDirLoc, parDir[i], "/", sep = "")
    subDir <- dir(subDirLoc)
    
    report <- parDir[i]
    
    
    for(r in 1:length(subDir))
    {
        
        bashCom <- paste("bash demo.sh ",parDirLoc,parDir[i],"/",subDir[r], sep = "")
         
        #code for printing to excel; system(bashCom) to print to console
        output <- system(bashCom, intern = TRUE)
    
        company <- substr(output[1],28,regexpr("[(]",output[1])[1]-2)
        outLen <- length(output)
        ind <- 7
        print(company, echo = TRUE)
        while(ind < outLen)
        {
            date <- output[ind]
            title <- output[ind+1]
            link <- output[ind+2]
            explination <- output[ind+3]
            
            outDataFrame <- rbind(outDataFrame, data.frame(report,company,date,title,explination,link))
            ind = ind + 5
        }
            
            
        
    }
    
}

write.csv(outDataFrame, file = "c2_SepemberReport.csv")