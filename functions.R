# if necessary, install these packages first #
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("lubridate")
# install.packages("ggmap")
# install.packages("DescTools")
# install.packages("Hmisc")
library(dplyr)
library(magrittr)
library(ggmap)
library(grid)
#library(animation)
library(lubridate)
library(Hmisc)
library(DescTools)
#########################
### List of Functions ###
#########################
# 1. totalMinToHours: 
###### input: a positive numeric value in minutes since midnight
###### output: character "hh:mm" for that number of minutes since midnight 00:00  
###### functions used: singleToDoubleDigit

# 2. singleToDoubleDigit 
###### input: numeric value = v. 
###### output: if v < 10, return character "0v"; else return character "v" 
###### Functions used: None

# 3. splitPostActMin
###### input: dataframe with one column labeled SPOSTMIN and one column labeled SACTMIN
###### function: for rows with an entry for both SPOSTMIN and SACTMIN become two rows, 
################ one with SPOSTMIN value and SACTMIN value
###### output: original dataframe except rows with entry for both SPOSTMIN and SACTMIN 
############## becomes two separater rows; one for SPOSTMIN and one for SACtmins
###### Functions used: None


# 4. findStartTimePerDay
###### input: dataframe with one column labeled SSINCEOPEN and dateModified
############# SSINCEOPEN should be numeric 
############# dateModified should be a dateObject as YYYY-MM-DD
###### output: dateframe with one column called date where it is the days 
############## from startDate to endDate inclusive in dateObject
############## second column is called startTime where it is the
##############  numuber of minutes from midnight
###### Functions used: None

# 5. preProcess
###### inputs: park is character upper case ex: "AK"
############## ride is number of specific ride, numeric value 
############## startDate in the format "MM/DD/YYYY" character
############## endDate in the format "MM/DD/YYYY? character
###### output: dataframe for specific ride in [startDate, endDate]
############## and orders first according to Year, Month, 
############## and time (HH:MM) in increasing pattern
###### Functions used: singleToDoubleDigit, splitPostActMin

# 6. windowGenerationPerPeriod
# 7. windowGenerationPerPeriod
# 8. avgRatio
# 9. avgDiff
# 10. avgActualOverWindow
# 11. avgPostOverWindow
# 12. avgActualOverPeriod
# 13. avgPostOverPeriod 
# 14. avgWaitTimeForRides

# function to change total minutes to hour:minutes#
totalMinToHours <- function(totalMin){
  if (is.na(totalMin)){
    return(NA)
  }
  # hours # 
  hour <- floor(totalMin/60)
  # minutes #
  min <- totalMin %% 60
  #convert single digit to double digits
  return(paste0(singleToDoubleDigit(hour), ":", singleToDoubleDigit(min)))
}
#convert an integer to double digit character representation
#if the digit argument already has 2 or 3 digits, only change it to character vector
singleToDoubleDigit <- function(digit){
  if(digit < 10){
    return(paste0(0,digit))
  }
  else{
    return(as.character(digit))
  }
}
#separate out the original entries that have both SPOSTMIN and SACTMIN
splitPostActMin <- function(dataFrame){
  # both not NA which we want to separate
  data1 <- dataFrame %>% filter(!is.na(SPOSTMIN), !is.na(SACTMIN))
  if(nrow(data1) == 0){
    return(dataFrame)
  }
  else{
    # at least one is NA # 
    data2 <- dataFrame %>% filter(is.na(SPOSTMIN) | is.na(SACTMIN))
    # data 1 + data 2 = dataFrame # 
    data1.1 <- rbind(data1, data1)
    # make first half SPOSTMIN NA # 
    data1.1[1:nrow(data1),]$SPOSTMIN <- NA
    # make second half SACTMIN NA #
    data1.1[(nrow(data1)+1):nrow(data1.1),]$SACTMIN <- NA
    return(rbind(data1.1, data2))
  }  
}
preProcess <- function(park, ride, startDate = "11/01/2009", endDate = "12/31/2016"){
  # inputs # 
  # park = initial of the park; all caps, such as "AK"
  # ride = number of specific ride # 
  # windowSize = length of time in minutes # 
  # startDate and endDate in the format "12/01/2009"  MM/DD/YYYY
  
  # To allow for faster read-in time, we created .Rdata files in advance and the read-in csv is stored in variable data
  filename <- paste0("readInRdata/waittimes_",park,".Rdata")
  load(filename)
  # when loaded, dataframe in variable (data) #
  
  # subset ride out #
  ride <- singleToDoubleDigit(ride)
  data <- data[data$sattid == paste0(park,ride),]
  
  # create a date based on combined information of SYEAR, SMONTH, SYEAR
  data$convertedDate <- paste0(sapply(data$SMONTH, singleToDoubleDigit), "/", sapply(data$SDAY, singleToDoubleDigit), "/", sapply((data$SYEAR-2000), singleToDoubleDigit))
  # throw away time inconsistencies entries# 
  data <- data[data$date == data$convertedDate, ]
  
  
  # sep data entries with both SPOST and SACT into two #
  data <- splitPostActMin(data)
  #filter out outliers
  data <- data[(!is.na(data$SPOSTMIN) & data$SPOSTMIN < 360 & data$SPOSTMIN >= 0) | (!is.na(data$SACTMIN) & data$SACTMIN < 360 & data$SACTMIN >= 0),]
  
  
  # we create a column of date objects to allow for filtering
  data$dateModified <- as.Date(data$date, "%m/%d/%y")
  
  # hours + min to minutes # 
  data$totalMin	<- (data$SHOUR * 60) + data$SMIN
  #account for time delay of actual posted time#
  for (i in 1: nrow(data)){
    if (!is.na(data$SACTMIN[i])){
      data$totalMin[i] = data$totalMin[i] - data$SACTMIN[i]
    }
  }
  #subsets by dates and sorts the data in order#
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  data <- data[data$dateModified <= endDateObject & data$dateModified >= startDateObject,]
  data <- data[order(data$SYEAR, data$SMONTH, data$SDAY, data$totalMin, decreasing = FALSE),]
  return(data)
}

findStartTimePerDay <- function(data, startDate, endDate){
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  startTimeList = c()
  for (i in 1: nrow(data)){
    startTime = data$totalMin[i] - data$SSINCEOPEN[i]
    correctDay <- data$dateModified[i] - startDateObject + 1
    if (startTime < 0){
      correctDay <- correctDay - 1
      startTime = 1440 + startTime
    }
    startTimeList[correctDay] = startTime
  }
  if ((length(startTimeList)+1) <= (endDateObject - startDateObject + 1)){
    for (i in (length(startTimeList)+1):(endDateObject - startDateObject + 1)){
      startTimeList[i] = NA
    }
  }
  dateSeq = seq(startDateObject, endDateObject, by = 1)
  return(data.frame(date = dateSeq, startTime = startTimeList))
}

windowGenerationPerDay <- function(indices, data, windowSize){
  numWindowsPerDay <- ceiling(24*60/windowSize)
  windowStartSinceMidnight <- seq(from = 0, to = (numWindowsPerDay -1)) * windowSize
  windowEndSinceMidnight <- c(seq(from = 1, to = (numWindowsPerDay -1)) * windowSize, 1440) 
  if (is.na(indices["beg"]) & is.na(indices["end"])){
    return(data.frame(AvgPost = rep(NA, numWindowsPerDay), AvgAct = rep(NA, numWindowsPerDay), NumPost = rep(NA, numWindowsPerDay), NumAct = rep(NA, numWindowsPerDay), Ratio = rep(NA, numWindowsPerDay), Diff = rep(NA, numWindowsPerDay)))
  }
  # subset of data set for unique day #
  data.wanted <- data[unlist(indices["beg"]):unlist(indices["end"]),]
  results.values <- data.frame(AvgPost = NA, AvgAct = NA, NumPost = NA, NumAct = NA, Ratio = NA)
  currentPosition = 1
  for (i in 1:numWindowsPerDay){
    totalPost = 0
    totalActual = 0
    numPost = 0
    numActual = 0
    #print(data.wanted[currentPosition, "totmin"])
    while (currentPosition <= nrow(data.wanted) & data.wanted[currentPosition, "totalMin"] >= windowStartSinceMidnight[i] & data.wanted[currentPosition, "totalMin"] < windowEndSinceMidnight[i]){
      if (!is.na(data.wanted[currentPosition, "SPOSTMIN"])){
        totalPost = totalPost + data.wanted[currentPosition, "SPOSTMIN"]
        numPost = numPost + 1
      }
      if (!is.na(data.wanted[currentPosition, "SACTMIN"])){
        totalActual = totalActual + data.wanted[currentPosition,"SACTMIN"]
        numActual = numActual + 1
      }
      currentPosition = currentPosition + 1
    }
    
    results.values[i, "NumPost"] = numPost
    results.values[i, "NumAct"] = numActual
    if (numPost != 0){
      avgPost = totalPost/numPost
      results.values[i, "AvgPost"] = avgPost
    }
    if (numActual != 0){
      avgActual = totalActual/numActual
      results.values[i, "AvgAct"] = avgActual
      
    }
    if (numPost!=0 & numActual != 0 & results.values[i, "AvgAct"] != 0){
      results.values[i, "Ratio"] = results.values[i, "AvgPost"]/results.values[i, "AvgAct"]
    }
  }
  results.values$Diff = results.values$AvgPost - results.values$AvgAct
  return(results.values)
}




windowGenerationPerPeriod <- function(park, ride, windowSize, startDate = "11/01/2009", endDate = "12/31/2016"){
  
  data <- preProcess(park, ride, startDate, endDate)
  startTimeDataFrame = findStartTimePerDay(data, startDate, endDate)
  
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  
  # number of unique days
  numUnqDays <- endDateObject - startDateObject + 1
  unqDays <- seq(startDateObject, endDateObject, by = 1)
  # rounded up number of windows in one day
  numWindowsPerDay <- ceiling(24*60/windowSize)
  
  # the string representation of the starting and ending index
  windowEntries <- c()
  for(i in 1:(numWindowsPerDay-1)){
    windowEntries[i] <- paste0(totalMinToHours((i-1)*windowSize), "-", totalMinToHours(i*windowSize))
  }
  windowEntries[numWindowsPerDay] <- paste0(totalMinToHours((numWindowsPerDay-1)*windowSize), "-", totalMinToHours(1440))
  
  
  # new table that we want to produce #
  # results has four columns # 
  # the first one is the date # 
  # the second is the window - time range # 
  # the third column displays the start and end of the time range respectively in minutes from Midnight#
  results <- data.frame(date = rep(unqDays, each = numWindowsPerDay), window = rep(windowEntries, times = numUnqDays))
  results$windowStartSinceMidnight <- seq(from = 0, to = (numWindowsPerDay -1)) * windowSize
  results$windowEndSinceMidnight <- c(seq(from = 1, to = (numWindowsPerDay -1)) * windowSize, 1440) 
  
  # we only care about these variables so select only these columns # 
  dataSelected <- data %>% select(SPOSTMIN, SACTMIN, totalMin, dateModified)
  # the first row index for which a unique day appears # 
  beginIndices <- match(unqDays, dataSelected$dateModified)
  reverseDates <- rev(dataSelected$dateModified)
  # the last row index for which a unique day appears # 
  endIndices <- (length(dataSelected$dateModified) + 1) - match(unqDays, reverseDates)
  indexRangePerDay <- data.frame(beg = beginIndices, end = endIndices)
  
  
  
  #results.values <-lapply(indexRangePerDay, windowGenerationPerDay, dataSelected, windowSize)
  results.values <- apply(indexRangePerDay, 1, windowGenerationPerDay, dataSelected, windowSize)
  results.values <- do.call(rbind, results.values)
  results <- cbind(results[,c("date", "window","windowStartSinceMidnight")], results.values)
  results$windowStartSinceOpen <- (results$windowStartSinceMidnight - rep(unlist(startTimeDataFrame$startTime), each = numWindowsPerDay))
  results$dayOfWeek <- weekdays(results$date)
  return(results)
}
# takes in indices for each unique day#



# tot.days to numUnqDays#
#computers vector which index corresponds to window in day and the value is average of all the ratio in that window
avgRatio <- function(resultsDataFrame){
  windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
  numWindowsPerDay <- ceiling(24*60/windowSize)
  numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
  vals <- c()
  result <- c()
  for(i in 1:numWindowsPerDay){
    vals <- resultsDataFrame$Ratio[i + numWindowsPerDay * (0:(numUnqDays-1))]
    result[i] <- mean(vals, na.rm = TRUE)
  }
  
  return(result)
}
#computers vector which index corresponds to window in day and the value is average of all the difference in that window
avgDiff <- function(resultsDataFrame){
  windowSize = resultsDataFrame$start[2]-resultsDataFrame$start[1]
  numWindowsPerDay <- ceiling(24*60/windowSize)
  numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
  vals <- c()
  result <- c()
  for(i in 1:numWindowsPerDay){
    vals <- resultsDataFrame$Diff[i + numWindowsPerDay * (0:(numUnqDays-1))]
    result[i] <- mean(vals, na.rm = TRUE)
  }
  
  return(result)
}


# takes results over some number of days # 
# for each window in a day # 
# take the mean of Avg Act and Avg Post over those days # 
avgActualOverWindow <- function(resultsDataFrame){
  windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
  numWindowsPerDay <- ceiling(24*60/windowSize)
  numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
  vals <- c()
  result <- c()
  for(i in 1:numWindowsPerDay){
    vals <- resultsDataFrame$AvgAct[i + numWindowsPerDay * (0:(numUnqDays-1))]
    result[i] <- mean(vals, na.rm = TRUE)
  }
  return(result)
}
avgPostOverWindow <- function(resultsDataFrame){
  windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
  numWindowsPerDay <- ceiling(24*60/windowSize)
  numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
  vals <- c()
  result <- c()
  for(i in 1:numWindowsPerDay){
    vals <- resultsDataFrame$AvgPost[i + numWindowsPerDay * (0:(numUnqDays-1))]
    result[i] <- mean(vals, na.rm = TRUE)
  }
  return(result)
}

# from that average of AvgPost and AvgAct over some number of days for each window in a day #
# take the mean of each average # 
avgActualOverPeriod <- function(resultsDataFrame){
  avgOverWindow = avgActualOverWindow(resultsDataFrame)
  result <- mean(avgOverWindow, na.rm = TRUE)
  return(result)
}

avgPostOverPeriod <- function(resultsDataFrame){
  avgOverWindow = avgPostOverWindow(resultsDataFrame)
  result <- mean(avgOverWindow, na.rm = TRUE)
  return(result)
}

# for a set of rides and a windowSize # 
# over a number of days #
avgWaitTimeForRides <- function (rideNames, windowSize, 
                                 startDate = "11/01/2009", 
                                 endDate = "12/31/2016"){
  
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  avgActWaitTimes <- c()
  avgPostWaitTimes <- c()
  for (i in 1: length(rideNames)){
    filename <- paste0(tolower(rideNames[i]), "Results",
                       windowSize, ".Rdata")
    load(filename)
    dataFrameName <- paste0(tolower(rideNames[i]), "Results",
                            windowSize, "Min")
    dataFrame <- get(dataFrameName)
    dataFrame <- dataFrame[dataFrame$date >= startDateObject & 
                             dataFrame$date <= endDateObject,]
    avgActWaitTimes[i] <- avgActualOverPeriod(dataFrame)
    avgPostWaitTimes[i] <- avgPostOverPeriod(dataFrame)
  }
  # return avgActual over Period and avgPost over period for set of rides # 
  return(c(avgActWaitTimes,avgPostWaitTimes))
}

dateObject <- function(date){
  return(as.Date(date, "%m/%d/%Y"))
}

getActualRideName <- function(park, ride){
  data <- read.csv("Touringplan's Data Dictionary.csv", header = TRUE, stringsAsFactors = FALSE)
  index <- match(paste0(tolower(park), singleToDoubleDigit(ride)), sapply(data$code, tolower))
  return(data$name[index])
}

readInPrecomputedDataFrame <- function(park, ride, windowSize, startDate = "11/01/2009", endDate = "12/31/2016", dayOfWeek = c(day.name)){
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  filename <- paste0("generatedResults/",tolower(park), windowSize, "Min/", tolower(park), singleToDoubleDigit(ride), "Results", windowSize, ".Rdata")
  load(filename)
  dataFrameName <- paste0(tolower(park), singleToDoubleDigit(ride), "Results",
                          windowSize, "Min")
  dataFrame <- get(dataFrameName)
  dataFrame <- dataFrame[dataFrame$date >= startDateObject &
                           dataFrame$date <= endDateObject &
                           tolower(weekdays(dataFrame$date)) %in% tolower(dayOfWeek),]
  return(dataFrame)
}

readInOneDayDataFrame <- function(rideName, windowSize, date){
  return (readInPrecomputedDataFrame(rideName, windowSize, startDate = date, endDate = date))
}






# Jerry Graphics Functions ##
#this is for ratio but can make minor changes to work for diff
# parks<-c("MK","HS","AK","EP")
# allrides<-list(c(1,3,141),c(12,20,22),c(7,11,14),c(9,14,16))
# for(parkride in 1:length(parks)){
#   parkname<-parks[parkride]
#   rides<-allrides[[parkride]]
#   date <- list(c("12/1/2013","2/28/2014"),c("3/1/2014","5/31/2014"),c("6/1/2014","8/31/2014"),c("9/1/2014","11/30/2014"))
#   for(i in rides){
#     for(x in date){
#       j=x[1]
#       k=x[2]
#       temp<-windowGenerationPerPeriod(parkname,i,15,j,k)
#       name<-paste0(parkname,singleToDoubleDigit(i),"Uplots",j,"-",k,'.pdf')
#       new<-gsub("/", "_",name , fixed=TRUE)
#       pdf(new,width=9,height=6)
#       plot(avgRatio(temp),main=paste0("Average Ratio between Posted and Actual Wait Times \n","At Expedition Everest"," Between ",j,"-",k),ylab="Posted/Actual Wait Time",xlab="Time in Hour", xaxt="n")
#       #axis.POSIXct(1, at=seq(strptime("0",format="%H"), strptime("24",format="%H"), by="hour"), format="%H")
#       axis(1, at = seq(0, 96, by = 4),labels = sapply(seq(0, 1440, by = 60),totalMinToHours),las=3)
#       dev.off()

#     }

#   }
# }




## Oscar Graphics Functions ## 

## Lei Graphics Functions ##
rangeDayPlot <- function(startDate, endDate, windowSize, rideId, rideName){
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  numWindowsPerDay <-ceiling(1440/windowSize)
  #dataList <- list()
  dataStore <- data.frame(matrix(NA, nrow = numWindowsPerDay, ncol = length(rideId)))
  # for each ride #
  for(k in 1:length(startDate)){
    for (i in 1:length(rideId)){
      colnames(dataStore) <- rideId
      
      filename <- paste0(rideId[i], "Results",
                         windowSize, ".Rdata")
      load(filename)
      dataFrameName <- paste0(tolower(rideId[i]), "Results",
                              windowSize, "Min")
      dataFrame <- get(dataFrameName)
      dataFrame <- dataFrame[dataFrame$date >= startDateObject[k] & 
                               dataFrame$date <= endDateObject[k],]
      dataStore[,i] <- avgActualOverWindow(dataFrame)
      
    }
    windowEntries <- c()
    windowEntries <- sapply((0:numWindowsPerDay)*windowSize, totalMinToHours)
    windowEntries[numWindowsPerDay] <- totalMinToHours(1440)
    val <- which(rep(c(TRUE,FALSE,FALSE), length.out = length(windowEntries)))
    pdfName <- paste0("plot1_", weekdays(startDateObject[k], abbreviate = TRUE),startDateObject[k],
                      "_to_",endDateObject[k],weekdays(endDateObject[k], abbreviate = TRUE),
                      ".pdf")
    pdf(pdfName)
    if(sum(is.na(dataStore)) != prod(dim(dataStore))){
      matplot(dataStore, type = "l", 
              lty = 1, 
              lwd = 2, 
              col = 1:5, 
              xlab = "Time",
              xaxt = "n",
              ylab = "Minutes", 
              main = paste("Average Actual Waiting Time from \n", startDateObject[k], "to", endDateObject[k]))
      legend('topleft', legend = rideName, col = 1:5, lty = 1, lwd = 2, cex = 0.75)
      axis(1, at = val, labels = windowEntries[val], las = 3)
    }
    
    dev.off()
  }
  return(dataStore)
}

dataLoop <- function (windowSize, 
                      startDate = "01/01/2014", 
                      endDate = "02/01/2014"){
  ## makes base map for Magic Kingdom ##
  lat <- range(rideLatLon$Lat)
  lon <- range(rideLatLon$Long)
  geoBox <- make_bbox(lon, lat, f= 0.2)
  magicKingdom <- get_map(location = geoBox ,
                          maptype = "roadmap", source = "osm", color = "bw")
  magicKingdomMap <- ggmap(magicKingdom)
  ## base map made for Magic Kingdom ##
  
  
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  
  # number of windows per day # 
  windowPerDay <- ceiling(1440/windowSize)
  # total number of days # 
  totalDays <- (endDateObject - startDateObject) + 1
  #totalWindows <- windowPerDay * totalDays
  
  # creates each start to end time according to windowSize throughout a day # 
  windowEntries <- c()
  for(i in 1:(windowPerDay-1)){
    windowEntries[i] <- paste0(totalMinToHours((i-1)*windowSize), "-", totalMinToHours(i*windowSize))
  }
  windowEntries[windowPerDay] <- paste0(totalMinToHours((windowPerDay-1)*windowSize), "-", totalMinToHours(1440))
  
  # all data we want to upload # 
  fileNameList <- sapply(tolower(rideLatLon$ID), paste0, "Results",windowSize, ".Rdata")
  
  # upload all data # 
  for(k in 1:length(fileNameList)){
    load(fileNameList[k])
  }
  
  # all data names # 
  objectNameList <- sapply(tolower(rideLatLon$ID), paste0, "Results", windowSize, "Min")
  
  daysDate <- seq(startDateObject, endDateObject, by = "day")
  #daysDate <- rep(daysDate, each = windowPerDay)
  
  # each list will contain all rides # 
  # each ride will have a data frame # 
  # each row is a window throughout a day # 
  # each column a day #
  # every entry will be the respective AvgAct or AvgPost for that day and that window # 
  dataListPost <- list()
  dataListAct <- list()
  for(k in 1:length(objectNameList)){
    dataPost <- data.frame(matrix(NA, nrow = windowPerDay, ncol = totalDays))
    dataAct <- data.frame(matrix(NA, nrow = windowPerDay, ncol = totalDays))
    dataFrameName <- objectNameList[k]
    dataFrame <- get(dataFrameName)
    dataFrame <- dataFrame[dataFrame$date >= startDateObject & 
                             dataFrame$date <= endDateObject,]
    for(d in 1:windowPerDay){
      dataPost[d,] <- t(subset(dataFrame, window == windowEntries[d], select = AvgPost))
      dataAct[d,] <- t(subset(dataFrame, window == windowEntries[d], select = AvgAct))
    }
    dataListPost[[k]] <- dataPost
    dataListAct[[k]] <- dataAct
    
    dataListPost[[k]] <- rowMeans(dataPost, na.rm = TRUE)
    dataListAct[[k]] <- rowMeans(dataAct, na.rm = TRUE)
  }
  names(dataListPost) <- objectNameList
  names(dataListAct) <- objectNameList
  
  
  min <- min(c(unlist(dataListPost), unlist(dataListAct)), na.rm = TRUE)
  max <- max(c(unlist(dataListPost), unlist(dataListAct)), na.rm = TRUE)
  
  for(i in 1:windowPerDay){
    #for(j in 1:length(objectNameList)){
    # dataPlot$Size[j] <- rowMeans(dataListAct[[j]], na.rm = TRUE)[i]
    #dataPlot$Size[j - 1 + length(objectNameList)] <- rowMeans(dataListPost[[j]], na.rm = TRUE)[i]
    #}
    dataPlot <- data.frame(Lat = rep(rideLatLon$Lat,2), 
                           Lon = rep(rideLatLon$Long,2))
    dataPlot$Fill <- rep(c("Average Actual Wait Time", 
                           "Average Posted Wait Time"), 
                         each = nrow(rideLatLon))
    dataPlot$Size <- NA
    # Act then Post #
    val <- c(sapply(dataListAct,"[", i), sapply(dataListPost,"[", i))
    dataPlot$Size <- val
    dataPlot$Size[is.nan(dataPlot$Size)] <- NA
    dataPlot$Size <- unlist(dataPlot$Size)
    #dataPlot$Size <- pi*(dataPlot$Size^2)
    
    pdfName <- paste0("flowPlot", i, ".pdf")
    
    dataPlot$Fill[is.na(dataPlot$Size)] <- NA
    #dataPlot$Size[is.na(dataPlot$Size)] <- 0
    dataPlot$Outline <- dataPlot$Fill
    dataPlot$Outline[!is.na(dataPlot$Fill)] <- "Data is Available"
    dataPlot$Fill[is.na(dataPlot$Fill)] <- "Time Data Isn't Available"
    dataPlot$Outline[is.na(dataPlot$Outline)] <- "Wait Time Is Not Available"
    
    magicKingdomMap +   
      geom_point(aes(x = Lon,y = Lat,
                     fill = Fill, 
                     size = Size/2, 
                     alpha = 0.5, 
                     color = Outline),
                 data = dataPlot,
                 shape = 21, 
                 na.rm = TRUE) +
      scale_color_manual(values = c("Data is Available" = "black", 
                                    "Wait Time Is Not Availabe" = NA), 
                         labels = c("Data is Available" = "Data is Available", 
                                    "Wait Time Is Not Available" = "Wait Time Is Not Available")) + 
      scale_fill_manual(values = c("Average Actual Wait Time" = 'red', 
                                   "Average Posted Wait Time" = "blue", 
                                   "Wait Time Is Not Available" = NA), 
                        labels =  c("Average Actual Wait Time" = 'Average Actual Wait Time', 
                                    "Average Posted Wait Time" = "Average Posted Wait Time", 
                                    "Time Data Isn't Available" = "Time Data Isn't Available"))+
      
      #scale_size_continuous(range = c(min,max)/10) +
      scale_size_identity() +
      #scale_size_continuous(limits = c(1,100)) +
      #scale_size_continuous(limits = c(min,max)*10) +
      
      ggtitle(paste("Plot of Average Posted + Average Actual Wait Time for \n Magic Kindgom Rides from", 
                    startDateObject, "to", endDateObject,windowEntries[i])) +
      theme(legend.key = element_rect(size = 0.5), 
            plot.title = element_text(size = 10, 
                                      face = "bold", 
                                      vjust = 1, 
                                      lineheight = 0.6), 
            axis.title.x = element_text(size = 6), 
            axis.title.y = element_text(size = 6), 
            axis.text.x = element_text(size = 6), 
            axis.text.y = element_text(size = 6)) + 
      labs(x = "Longitude", y = "Latitude") +
      guides(alpha = FALSE, 
             color = FALSE,
             fill = guide_legend(title = NULL, 
                                 overrides.aes = list(shape = 21)))
    
    ggsave(pdfName, width = 8, height = 5, units = "in")
    print(val)
    print(windowEntries[i])
  }
}


adjustedStdActualOverWindow <- function(resultsDataFrame){
  windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
  numWindowsPerDay <- ceiling(24*60/windowSize)
  numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
  vals <- c()
  result <- c()
  for(i in 1:numWindowsPerDay){
    vals <- resultsDataFrame$AvgAct[i + numWindowsPerDay * (0:(numUnqDays-1))]
    if (sum(!is.na(vals)) > 0) {result[i] <- sd(vals, na.rm = TRUE)/sqrt(sum(!is.na(vals)))}
    else {result[i] <- NA}    
  }
  return(result)
}
#this calculates standard deviation using n-1
adjustedStdPostedOverWindow <- function(resultsDataFrame){
  windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
  numWindowsPerDay <- ceiling(24*60/windowSize)
  numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
  vals <- c()
  result <- c()
  for(i in 1:numWindowsPerDay){
    vals <- resultsDataFrame$AvgPost[i + numWindowsPerDay * (0:(numUnqDays-1))]
    if (sum(!is.na(vals)) > 0) {result[i] <- sd(vals, na.rm = TRUE)/sqrt(sum(!is.na(vals)))}
    else {result[i] <- NA}    
  }
  return(result)
}

pdfPlotAvgOfWindowOverMultipleDays <- function(park, ride,windowSize, 
                                               startDate = "11/01/2009", 
                                               endDate = "12/31/2016", 
                                               confPerc, dayOfWeek = c(day.name)){  
  numWindowsPerDay <- ceiling(24*60/windowSize)
  windowStart <- seq(from = 0, to = (numWindowsPerDay -1)) * windowSize
  
  dataFrame <- readInPrecomputedDataFrame(park, ride, windowSize, 
                                          startDate, endDate, dayOfWeek)
  numUnqDays = nrow(dataFrame)/numWindowsPerDay
  postDF = c()
  actualDF = c()
  for (i in 1: numWindowsPerDay){
    postDF[i] <- sum(!is.na(dataFrame$AvgPost[i + 
                                                numWindowsPerDay * (0:(numUnqDays-1))]))
    actualDF[i] <- sum(!is.na(dataFrame$AvgAct[i + 
                                                 numWindowsPerDay * (0:(numUnqDays-1))]))
  }
  postVal <- qt(confPerc + (1-confPerc)/2, df = postDF - 1)
  actualVal <- qt(confPerc + (1- confPerc)/2, df = actualDF - 1)
  actualVal[is.nan(actualVal)] <- NA
  postVal[is.nan(postVal)] <- NA
  averagePostOverWindow <- avgPostOverWindow(dataFrame)
  averageActOverWindow <- avgActualOverWindow(dataFrame)
  upperConfidenceIntervalofPost <- averagePostOverWindow + 
    adjustedStdPostedOverWindow(dataFrame)*postVal
  lowerConfidenceIntervalofPost <- averagePostOverWindow - 
    adjustedStdPostedOverWindow(dataFrame)*postVal
  upperConfidenceIntervalofActual <- averageActOverWindow + 
    adjustedStdActualOverWindow(dataFrame)*actualVal
  lowerConfidenceIntervalofActual <- averageActOverWindow - 
    adjustedStdActualOverWindow(dataFrame)*actualVal
  
  # baseFrame <- data.frame(windowStart = windowStart, 
  #                         range = seq(floor(min(c(lowerConfidenceIntervalofPost, 
  #                                             lowerConfidenceIntervalofActual), na.rm = TRUE)), 
  #                                     ceiling(max(c(upperConfidenceIntervalofPost, 
  #                                                   upperConfidenceIntervalofActual), na.rm = TRUE)), 
  #                                     length.out = length(windowStart)))
  baseFrame <- data.frame(windowStart = windowStart, 
                          range = seq(0,
                                      125, 
                                      length.out = length(windowStart)))
  filename <- paste0("both", "avgWT ", park ,ride, "win", 
                     windowSize, " between ", as.character(dateObject(startDate)),
                     as.character(dateObject(endDate)), " on",
                     paste(dayOfWeek, collapse = ","), ".pdf")
  ggplot(baseFrame,aes(x = windowStart, y = range))  + 
    scale_y_continuous(breaks=seq(0, 120, 20)) + 
    geom_point(aes(y = averagePostOverWindow), color = "blue") + 
    geom_line(aes(y = averagePostOverWindow, color = "AvgPost")) +
    geom_ribbon(aes(ymin = lowerConfidenceIntervalofPost, 
                    ymax = upperConfidenceIntervalofPost), 
                alpha = 0.2, fill = "blue") + 
    geom_point(aes(y = averageActOverWindow), color = "red") + 
    geom_line(aes(y = averageActOverWindow, color = "AvgAct")) + 
    geom_ribbon(aes(ymin = lowerConfidenceIntervalofActual, 
                    ymax = upperConfidenceIntervalofActual),
                alpha = 0.2, fill = "red") + 
    ggtitle(paste0("Average Wait Times of ",
                   getActualRideName(park, ride),
                   " between ",
                   as.character(dateObject(startDate)),
                   " and ", as.character(dateObject(endDate)),
                   "\nWindow Size = ", windowSize, " min on ", 
                   paste(dayOfWeek, collapse = ",")))+
    labs(x = "Time in the Day (HH:MM)", 
         y = "Minutes") + 
    scale_x_continuous(breaks = seq(0,1440, by= 60), 
                       labels = sapply(seq(0, 1440, by = 60),
                                       totalMinToHours), 
                       limits = c(300,1440)) +
    scale_color_manual(values = c("AvgAct" = "red",
                                  "AvgPost" = "blue"),
                       labels = c("Average Actual",
                                  "AveragePosted")) + 
    guides(color = guide_legend(title = NULL))
  ggsave(filename, width = 9, height = 6)
  
  # pdf(filename, width = 9, height = 6)
  # plot(x = windowStart,
  #      y = averagePostOverWindow,
  #      xlab = "time in the day (hour: minute)",
  #      xlim = c(300, 1440), 
  #      #we limit the starting values for the time axis because there 
  #      #should be no useful values for wait time before 5am
  #      xaxt = "n",
  #      ylab = "wait time (minutes)",
  #      col = "blue",
  #      type = "l"
  #      )
  # lines(x = windowStart, y = upperConfidenceIntervalofPost, 
  #       col = "blue", lty = 2)
  # lines(x = windowStart, y = lowerConfidenceIntervalofPost, 
  #       col = "blue", lty = 2)
  # lines(x = windowStart, y = averageActOverWindow, col = "red")
  # lines(x = windowStart, y = upperConfidenceIntervalofActual, 
  #       col = "red", lty = 2)
  # lines(x = windowStart, y = lowerConfidenceIntervalofActual, 
  #       col = "red", lty = 2)
  #   
  # axis(side = 1, at = seq(0, 1440, by = 60), 
  #      labels = sapply(seq(0, 1440, by = 60),totalMinToHours),
  #      las = 3)
  # title(main = list(paste0("Average Wait Times of ", 
  #                          getActualRideName(park, ride), 
  #                          " between ", 
  #                          as.character(dateObject(startDate)), 
  #                          " and ", as.character(dateObject(endDate)), 
  #                          "\nWindow Size = ", windowSize, " min on", dayOfWeek),
  #                          font = 1))
  # legend(x = "topright", legend = c("Posted", "Actual"), 
  #        col=c("blue", "red"), lty = c(1, 1))
  # dev.off()
}







# dayOfWeek<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday")
# for(i in dayOfWeek){
#   pdfPlotAvgOfWindowOverMultipleDays(park = "mk", ride = 3, windowSize = 20, startDate = "01/01/2014", endDate = "12/31/2014", confPerc = 0.95,i)

# }


piePlotPerRide <- function(park, ride, startDate = "11/01/2009", endDate= "12/31/2016", windowSize, dayOfWeek = c(day.name)){
  dataFrame <- readInPrecomputedDataFrame(park, ride, windowSize, 
                                          startDate, endDate, dayOfWeek)
  dataFrame<-dataFrame %>% filter(!is.na(dataFrame$AvgAct) & !is.na(dataFrame$AvgPost))
  greaterThanDoubleDF <- dataFrame[(2*dataFrame$AvgAct) <= dataFrame$AvgPost,]
  slices <- c(nrow(dataFrame)-nrow(greaterThanDoubleDF), 
              nrow(greaterThanDoubleDF[greaterThanDoubleDF$AvgAct < 5,]), 
              nrow(greaterThanDoubleDF[greaterThanDoubleDF$AvgAct > 10,]), 
              nrow(greaterThanDoubleDF[greaterThanDoubleDF$AvgAct >= 5 & greaterThanDoubleDF$AvgAct <= 10,]))
  lbls <- c("AvgPost < 2*AvgAct", 
            "AvgAct Less than 5 + AvgPost >= 2*AvgAct",
            "AvgAct Greater than 10 + AvgPost >= 2*AvgAct", 
            "AvgAct between 5 and 10 + AvgPost >= 2*AvgAct")
  pct <- slices/sum(slices)*100
  lbls <- paste0(lbls, ", ",slices, ", ", pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,
      main=paste0("Percentage of Windows from ", 
                  startDate, " to ", endDate, 
                  "every ", windowSize, " minutes for ",
                  park, ride), 
      col=rainbow(length(lbls)))
  legend("topright", lbls, cex=0.8, fill=rainbow(length(lbls)))
}

piePlotPerRide("mk", 3, startDate = "11/01/2009", endDate = "12/31/2016", windowSize = 10)

histPlotPerRide <- function(park, ride, startDate = "11/01/2009", endDate= "12/31/2016", windowSize, dayOfWeek = c(day.name)){
  dataFrame <- readInPrecomputedDataFrame(park, ride, windowSize, 
                                          startDate, endDate, dayOfWeek)
  dataFrame<-dataFrame %>% filter(!is.na(dataFrame$AvgAct) & !is.na(dataFrame$AvgPost))
  greaterThanDoubleDF <- dataFrame[(2*dataFrame$AvgAct) <= dataFrame$AvgPost,]
  max<-max(greaterThanDoubleDF$AvgAct)
  max<-ceiling(max/5)
  histarray<-rep(0,max)
  for(i in greaterThanDoubleDF$AvgAct){
    #     histarray[floor(i/5+1)]=histarray[floor(i/5+1)]+1
    if (i == 0)  {histarray[1] = histarray[1] + 1}
    else {
      histarray[ceiling(i/5)] = histarray[ceiling(i/5)] + 1
    }
  }
  endLabels = seq(5,max*5,5)
  startLabels = endLabels - 4
  startLabels[1] = 0
  labels = paste0(startLabels, "-", endLabels)
  barplot(histarray, names.arg = labels, las = 2)
  
  #axis(1,at=seq(0,max,1),labels=seq(0,max*5,5))
}

# vector of rides # 
combineParkRides <- function(park, rides, startDate, endDate, windowSize, dayOfWeek = c(day.name)){ 
  
  dataFrameList <- lapply(rides, readInPrecomputedDataFrame, park = park, windowSize=windowSize, startDate=startDate, endDate=endDate, dayOfWeek=dayOfWeek)
  #dataFrame <- do.call(dataFrameList, merge, by = c("date", "windowStartSinceMidnight", "windowStartSinceOpen"))
  #dataFrame <- Reduce(function(x, y) merge(x, y, by = c("date", "windowStartSinceMidnight", "windowStartSinceOpen")), dataFrameList)
  result = dataFrameList[[1]][,c("date", "window", "windowStartSinceMidnight","windowStartSinceOpen", "dayOfWeek")]
  for(i in 1:length(rides)){
    result[[paste0(park, rides[i],"AvgAct")]] = dataFrameList[[i]]$AvgAct					#mk3 instead of mk03
    result[[paste0(park, rides[i], "AvgPost")]] = dataFrameList[[i]]$AvgPost
    result[[paste0(park, rides[i], "NumPost")]] = dataFrameList[[i]]$NumPost
    result[[paste0(park, rides[i], "NumAct")]] = dataFrameList[[i]]$NumAct
  }
  return(result)
}


produceRegressionDataSet <- function(park, predictRide, otherRides, startDate, 
                                     endDate, windowSize, historical, chunks, 
                                     dayOfWeek = c(day.name), predictLag, 
                                     numDays = c(30, 90, 180, 365), 
                                     startTimeOfChunks = c(9,14,17,21)){
  
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y") 
  numWindowPerDay <- ceiling(1440/windowSize)
  numDays <- numDays[!(numDays >= endDateObject - startDateObject)]
  dataFrame <- combineParkRides(park, c(predictRide, otherRides), endDate = endDate, 
                                startDate = startDate, windowSize, dayOfWeek)
  if(historical == TRUE){
    
    columnNames <- paste0("Lag", numDays, "SameWindow")
    dataFrame[, c(columnNames, "previousExactSameWindow")] <- NA
    moreColumnNames <- paste0("Lag", numDays, "AllWindows")
    dataFrame[,moreColumnNames] <- NA
    currentDate = dateObject("01/01/2000") # hardcoded date which is not possible as a date we have data of
    currentAverage = NA
    for(i in ((max(numDays) + predictLag)*numWindowPerDay +1):nrow(dataFrame)){
      if (dataFrame[i, "date"] == currentDate){
        dataFrame[i ,moreColumnNames] <- currentAverage
      }
      else{
        currentDate <- dataFrame[i, "date"]
        currentAverage <- averageOverDayAverages(park, predictRide, currentDate,
                                                 dataFrame[i, "windowStartSinceMidnight"],
                                                 predictLag,numDays,
                                                 dataFrame, windowSize, startDateObject)
        dataFrame[i ,moreColumnNames] <- currentAverage
      }
      dataFrame[i ,c(columnNames, "previousExactSameWindow")] <- movingAverageOverDays(park, predictRide, dataFrame[i,"date"],
                                                                                       dataFrame[i, "windowStartSinceMidnight"],predictLag,numDays,dataFrame, windowSize, startDateObject)
    }
  }
  if(chunks == TRUE){
    dataFrame[,c("chunk","minIntoChunk")] <- NA
    for(i in 1:nrow(dataFrame)){
      dataFrame[i,c("chunk","minIntoChunk")] <- chunkIndicators(dataFrame[i,"windowStartSinceMidnight"],
                                                                startTimeOfChunks)
      
      #dataFrame <- dataFrame[!is.na(dataFrame$chunk),]
    }
    dataFrame[,"minIntoChunk"] <- as.numeric(dataFrame[,"minIntoChunk"])
  }
  #dataFrame <- na.omit(dataFrame)
  return(dataFrame)
}


movingAverageOverDays <- function(park, predictRide, predictDate, windowStartSinceMidnight, 
                                  predictLag = 7, numDays, dataFrame, windowSize, firstDate){
  endDate <- predictDate - predictLag
  startDates <- endDate - numDays
  listDateSeq <- lapply(startDates, seq, endDate, by = 1)
  numWindowPerDay <- ceiling(1440/windowSize)
  results <- c()
  endIndex <- as.numeric(endDate - firstDate)*numWindowPerDay + windowStartSinceMidnight/windowSize + 1
  for(i in 1:length(numDays)){
    startIndex <- as.numeric(startDates[i]-firstDate)*numWindowPerDay + windowStartSinceMidnight/windowSize + 1
    results[i] <- mean(dataFrame[seq(startIndex, endIndex, 
                                     by = numWindowPerDay),
                                 paste0(park, predictRide, "AvgAct")], 
                       na.rm = TRUE)
    
  }
  exactValue <- dataFrame[endIndex, paste0(park, predictRide, "AvgAct")]
  results = c(results, exactValue)
  results[is.nan(results)] <- NA
  return(results)
}

averageOverDayAverages <- function(park, predictRide, predictDate, windowStartSinceMidnight, 
                                   predictLag = 7, numDays, dataFrame, windowSize, firstDate){
  endDate <- predictDate - predictLag
  startDates <- endDate - numDays
  listDateSeq <- lapply(startDates, seq, endDate, by = 1)
  numWindowPerDay <- ceiling(1440/windowSize)
  results <- c()
  endIndex <- as.numeric(endDate + 1 - firstDate)*numWindowPerDay
  #averagePerDay <- tapply(dataFrame[, paste0(park, predictRide, "AvgAct")],
  #                       rep(seq(1:nrow(dataFrame),by = numWindowPerDay, 
  #                             each = numWindowPerDay),
  #                         mean, na.rm = TRUE)
  # mean(averagePerDay[startDates[i]-firstDate + 1, endDate - firstDate + 1], na.rm = TRUE)
  for(i in 1:length(numDays)){
    startIndex <- as.numeric(startDates[i]-firstDate)*numWindowPerDay + 1
    averagePerDay <- tapply(dataFrame[seq(startIndex, endIndex), paste0(park, predictRide,"AvgAct")], 
                            rep(seq(startIndex, endIndex, by = numWindowPerDay),
                                each = numWindowPerDay), 
                            mean, na.rm = TRUE)
    results[i] <- mean(averagePerDay, na.rm = TRUE)
  }
  results[is.nan(results)] <- NA
  return(results)
}



chunkIndicators <- function(sinceMidnight, startTimeOfChunks = c(9,14,17,21)){
  
  startMinutes <- startTimeOfChunks*60
  if(sinceMidnight >= 0 & sinceMidnight < 120){
    return(c("1", sinceMidnight))
    #return(data.frame(chunk = "1", minIntoChunk = sinceMidnight))
  }
  if(sinceMidnight >= 120 & sinceMidnight < 480){
    return(c(NA, sinceMidnight - 120))
    #return(data.frame(chunk = NA, minIntoChunk = sinceMidnight - 120))
  }
  
  for (i in 1:length(startTimeOfChunks)){
    if (i == 1){
      if (sinceMidnight >= 480 & sinceMidnight < startMinutes[i]){
        return(c(as.character(i+1), sinceMidnight - 480))
        #return(data.frame(chunk = as.character(i + 1), minIntoChunk = sinceMidnight - 480))
      }
    }
    else if (sinceMidnight >= startMinutes[i-1] & sinceMidnight < startMinutes[i]){
      return(c(as.character(i+1), sinceMidnight - startMinutes[i-1]))
      #return(data.frame(chunk = as.character(i + 1), minIntoChunk = sinceMidnight - startMinutes[i-1]))
    }
    if (i == length(startTimeOfChunks)){
      return(c(as.character(i+2), sinceMidnight - startMinutes[i]))
      #return(data.frame(chunk = as.character(i + 2), minIntoChunk = sinceMidnight - startMinutes[i]))
    }
  }  
}



