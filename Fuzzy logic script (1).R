#####################################################################
# Description: This function flags Fuzzy duplicates with a given set of
#              names. 
# Version: 1.0
# Author : Satish (satish181990@gmail.com)
# @Param : Function taks a DataFrame with an ID and the Name(string) on which the
#          fuzzy needs to be performed.
# @Return : Function returns a data frame which has a Group ID for ech fuzzyMatch identifier
# 
#####################################################################

detectfuzzy <- function(vendor,similarityFraction)
{
  
  matchGroup <- 0
  numRecords <- 1
  numRecords <- as.numeric(nrow(vendor))
  result <- data.frame(ID = "", Name = "", Group = "", stringsAsFactors = FALSE)
  matchValue <- 4
  alreadyMatchedFlag <- FALSE
  vendor$MatchedFlag <- FALSE
  
  for(rowCount in c(1:numRecords))
  {
    currentName <- as.character(vendor[rowCount,]$Name)
    currentID <- as.character(vendor[rowCount,]$ID)
    matchedFlag <- vendor[rowCount,]$MatchedFlag
    if(!matchedFlag){
    for(iterate in c(rowCount:numRecords))
    {
      if(iterate != rowCount )
      {
        compareName <- as.character(vendor[iterate,]$Name)
        compareID <- as.character(vendor[iterate,]$ID)
        matchValue <- agrep(currentName,compareName,max.distance = (1-similarityFraction))
        if( length(matchValue)==1)
        {
          vendor[iterate,]$MatchedFlag <- TRUE
          if(!alreadyMatchedFlag)
          {
          matchGroup <- matchGroup+1
          result <- rbind(result,data.frame(ID = as.character(currentID),Name = as.character(currentName), Group = matchGroup))
          }
          result <- rbind(result,data.frame(ID = as.character(compareID),Name = as.character(compareName), Group = matchGroup))
          alreadyMatchedFlag <- TRUE
        }
        else {
          
          alreadyMatchedFlag <- FALSE
        }
      }
    }}
    
    alreadyMatchedFlag <- FALSE
  }
  result[-1,]
  
}