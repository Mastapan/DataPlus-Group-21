---
  title: "Cleaning Code"
author: "Lei Qian"
date: "May 23, 2016"
output: pdf_document
---
  ```{r}
# if necessary, install these packages first #
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("lubridate")
#install.packages("ggmap")
library(dplyr)
library(magrittr)
library(ggmap)

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
  filename <- paste0("waittimes_",park,".Rdata")
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
  
  data <- data[(!is.na(data$SPOSTMIN) & data$SPOSTMIN < 360 & data$SPOSTMIN >= 0) | (!is.na(data$SACTMIN) & data$SACTMIN < 360 & data$SACTMIN >= 0),]
  
  
  # we create a column of date objects to allow for filtering
  data$dateModified <- as.Date(data$date, "%m/%d/%y")
  
  # hours + min to minutes # 
  data$totalMin	<- (data$SHOUR * 60) + data$SMIN
  for (i in 1: nrow(data)){
    if (!is.na(data$SACTMIN[i])){
      data$totalMin[i] = data$totalMin[i] - data$SACTMIN[i]
    }
  }
  #data$totalMin = data$totalMin - data$SACTMIN[data$SACTMIN]
  
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
  
  # takes in indices for each unique day# 
  windowGenerationPerDay <- function(index){
    if (is.na(indexRangePerDay[index,"beg"]) & is.na(indexRangePerDay[index,"end"])){
      return(data.frame(AvgPost = rep(NA, numWindowsPerDay), AvgAct = rep(NA, numWindowsPerDay), NumPost = rep(NA, numWindowsPerDay), NumAct = rep(NA, numWindowsPerDay), Ratio = rep(NA, numWindowsPerDay), Diff = rep(NA, numWindowsPerDay)))
    }
    # subset of data set for unique day #
    data.wanted <- data[indexRangePerDay[index,"beg"]:indexRangePerDay[index,"end"],]
    results.values <- data.frame(AvgPost = NA, AvgAct = NA, NumPost = NA, NumAct = NA, Ratio = NA)
    currentPosition = 1
    for (i in 1:numWindowsPerDay){
      totalPost = 0
      totalActual = 0
      numPost = 0
      numActual = 0
      #print(data.wanted[currentPosition, "totmin"])
      while (currentPosition <= nrow(data.wanted) & data.wanted[currentPosition, "totalMin"] >= results$windowStartSinceMidnight[i] & data.wanted[currentPosition, "totalMin"] < results$windowEndSinceMidnight[i]){
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
  
  results.values <-lapply(1:length(unique(results$date)), windowGenerationPerDay)
  results.values <- do.call(rbind, results.values)
  results <- cbind(results[,c("date", "window","windowStartSinceMidnight")], results.values)
  results$windowStartSinceOpen <- (results$windowStartSinceMidnight - rep(unlist(startTimeDataFrame$startTime), each = numWindowsPerDay))
  results$dayOfWeek <- weekdays(results$date)
  return(results)
}



# tot.days to numUnqDays#
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
avgDiff <- function(resultsDataFrame){
  windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
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
# made some changes to these functions, commented out old code, new code below # 

# avgActualOverWindow <- function(resultsDataFrame){
#   windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
#   numWindowsPerDay <- ceiling(24*60/windowSize)
#   numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
#   vals <- c()
#   result <- c()
#   for(i in 1:numWindowsPerDay){
#     vals <- resultsDataFrame$AvgAct[i + numWindowsPerDay * (0:(numUnqDays-1))]
#     result[i] <- mean(vals, na.rm = TRUE)
#   }
#   return(result)
# }
# avgPostOverWindow <- function(resultsDataFrame){
#   windowSize = resultsDataFrame$windowStartSinceMidnight[2]-resultsDataFrame$windowStartSinceMidnight[1]
#   numWindowsPerDay <- ceiling(24*60/windowSize)
#   numUnqDays <- nrow(resultsDataFrame)/numWindowsPerDay
#   vals <- c()
#   result <- c()
#   for(i in 1:numWindowsPerDay){
#     vals <- resultsDataFrame$AvgPost[i + numWindowsPerDay * (0:(numUnqDays-1))]
#     result[i] <- mean(vals, na.rm = TRUE)
#   }
#   return(result)
# }

# avgActualOverPeriod <- function(resultsDataFrame){
#   avgOverWindow = avgActualOverWindow(resultsDataFrame)
#   result <- mean(avgOverWindow, na.rm = True)
#   return(result)
# }

# avgPostOverPeriod <- function(resultsDataFrame){
#   avgOverWindow = avgPostOverWindow(resultsDataFrame)
#   result <- mean(avgOverWindow, na.rm = True)
#   return(result)
# }


# avgWaitTimeForRides <- function (rideNames, windowSize, startDate = "11/01/2009", endDate = "12/31/2016"){

#   startDateObject <- as.Date(startDate, "%m/%d/%Y")
#   endDateObject <- as.Date(endDate, "%m/%d/%Y")
#   avgActWaitTimes <- c()
#   avgPostWaitTimes <- c()
#   for (i in 1: length(rideNames)){
#     filename <- paste0(tolower(rideNames[i]), "Results", windowSize, ".Rdata")
#     load(filename)
#     dataFrameName <- paste0(tolower(rideNames[i]), "Results", windowSize, "Min")
#     dataFrame <- get(dataFrameName)
#     dataFrame <- dataFrame[dataFrame$date >= startDateObject & dataFrame$date <= endDateObject]
# 		avgActWaitTimes[i] <- avgActualOverPeriod(dataFrame)
# 		avgPostWaitTimes[i] <- avgPostOverPeriod(dataFrame)
#   }
#   return(c(avgActWaitTimes,avgPostWaitTimes))
# }

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
  return(c(avgActWaitTimes,avgPostWaitTimes))
}




# staticChart  Act then Post#     
dataPlotFunc <- function (windowSize, 
                          startDate = "11/01/2009", 
                          endDate = "12/31/2016"){
  dataPlot <- data.frame(Lat = rep(rideLatLon$Lat,2), 
                         Lon = rep(rideLatLon$Long,2))
  dataPlot$Size <- avgWaitTimeForRides(rideLatLon$ID, windowSize, startDate, endDate)
  dataPlot$colour <- rep(c("Average Actual Wait Times", 
                           "Average Posted Wait Times"), 
                         each = windowSize)
  return(dataPlot[order(dataPlot$Size, decreasing = TRUE),])
}


rideLatLon <- read.csv("ParkRidesLonLat.csv", header = TRUE)

lat <- range(rideLatLon$Lat)
lon <- range(rideLatLon$Long)
geoBox <- make_bbox(lon, lat, f= 0.1)
magicKingdom <- get_map(location = geoBox ,
                        maptype = "roadmap", source = "osm", color = "bw")
magicKingdomMap <- ggmap(magicKingdom)
magicKingdomMap

magicKingdomMap +   
  geom_point(aes(x = Lon,y = Lat,
                 color = colour, 
                 size = Size),
             data = dataPlotFunc(15, 
                                 startDate = "1/1/2012", 
                                 endDate = "12/31/2012")) +
  scale_radius(range = range(dataPlotFunc(15, 
                                          startDate = "1/1/2012", 
                                          endDate = "12/31/2012")$Size, 
                             na.rm = TRUE)) +
  ggtitle("Time Flow Plot of Average Posted and Average Actual Wait Time for Magic Kindgom Rides") +
  theme(legend.key = element_rect(size = 0.5), 
        plot.title = element_text(size = 20, 
                                  face = "bold", 
                                  vjust = 1, 
                                  lineheight = 0.6)) + 
  labs(x = "Longitude", y = "Latitude")+
  guides(size = guide_legend(override.aes = list(size = 1:3))) 




dataPlotFunc <- function (windowSize, 
                          startDate = "11/01/2009", 
                          endDate = "12/31/2016"){
  dataPlot <- data.frame(Lat = rep(rideLatLon$Lat,2), 
                         Lon = rep(rideLatLon$Long,2))
  dataPlot$Size <- avgWaitTimeForRides(rideLatLon$ID, windowSize, startDate, endDate)
  dataPlot$colour <- rep(c("Average Actual Wait Times", 
                           "Average Posted Wait Times"), 
                         each = windowSize)
  dataPlot <- dataPlot[order(dataPlot$Size, decreasing = TRUE),]
  
  lat <- range(rideLatLon$Lat)
  lon <- range(rideLatLon$Long)
  geoBox <- make_bbox(lon, lat, f= 0.1)
  magicKingdom <- get_map(location = geoBox ,
                          maptype = "roadmap", source = "osm", color = "bw")
  magicKingdomMap <- ggmap(magicKingdom)
  
  magicKingdomMap +   
    geom_point(aes(x = Lon,y = Lat,
                   fill = colour, 
                   size = Size),
               data = dataPlot, 
               colour = "black", 
               shape = 21, 
               stroke = 2) +
    scale_radius(range = range(dataPlot$Size, 
                               na.rm = TRUE), 
                 breaks = 1:ceiling(max(dataPlot$Size, na.rm = TRUE)/20)*20) +
    ggtitle(paste("Plot of Average Posted + Average Actual Wait Time for Magic Kindgom Rides", 
                  "from", startDate, "to", endDate)) +
    theme(legend.key = element_rect(size = 0.5), 
          plot.title = element_text(size = 20, 
                                    face = "bold", 
                                    vjust = 1, 
                                    lineheight = 0.6)) + 
    labs(x = "Longitude", y = "Latitude")+
    guides(size = guide_legend(override.aes = list(size = 1:(length(1:ceiling(max(dataPlot$Size, na.rm = TRUE)/20)*20)-1)))) 
  
}


dataPlotFunc(15, "01/01/2012", "01/01/2012")





# plot 1 # 

rideId <- paste0("mk", sapply(c(1,2,23,5,3), singleToDoubleDigit))
rideName <- c("SpaceMT", "BuzzLightyear", 
              "Haunted Mansion", "Peter Pan", 
              "Big Thunder")
winterSingleDays <- c("12/16/2013", "1/6/2014", "2/3/2014", 
                      "12/10/2013", "1/14/2014", "2/11/2014", 
                      "12/18/2013", "1/22/2014", "2/25/2014", 
                      "12/26/2013", "1/16/2014", "2/13/2014", 
                      "12/20/2013", "1/10/2014", "2/28/2014", 
                      "12/7/2013", "1/11/2014", "2/1/2014", 
                      "12/29/2013", "1/26/2014", "2/16/2014")

singleDayPlot <- function(seasonDates, windowSize){
  startDateObject <- as.Date(seasonDates, "%m/%d/%Y")
  endDateObject <- as.Date(seasonDates, "%m/%d/%Y")
  numWindowsPerDay <-ceiling(1440/windowSize)
  dataList <- list()
  dataStore <- data.frame(matrix(NA, nrow = numWindowsPerDay, ncol = length(rideId)))
  
  for(j in 1:length(seasonDates)){
    
    for (i in 1:length(rideId)){
      filename <- paste0(rideId[i], "Results",
                         windowSize, ".Rdata")
      load(filename)
      dataFrameName <- paste0(tolower(rideId[i]), "Results",
                              windowSize, "Min")
      dataFrame <- get(dataFrameName)
      dataFrame <- dataFrame[dataFrame$date >= startDateObject[j] & 
                               dataFrame$date <= endDateObject[j],]
      dataStore[,i] <- dataFrame$AvgAct
      
    }
    colnames(dataStore) <- rideId
    dataList[[j]] <- dataStore
    pdfName <- paste0(as.Date(seasonDates[j], "%m/%d/%Y"), ".pdf")
    pdf(pdfName)
    matplot(dataList[[j]], type = 'l', 
            lty = 1:5, 
            lwd = 1, 
            col = 1:5,
            xlab = "Time", 
            ylab = "AvgActualInMinutes", 
            main = paste("Average Actual Waiting Time on", startDateObject[j]))
    legend('topleft',legend = rideId, col = 1:5, lty = 1:5, lwd = 1)
    dev.off()
  }
  names(dataList) <- seasonDates
  return(dataList)
}

singleDayPlot(winterSingleDays, windowSize = 20)







mk03.process <- preProcess(park = "MK", ride = 3, "01/01/2014", "12/31/2014")
mk04.process <- preProcess(park = "MK", ride = 4, "01/01/2014", "12/31/2014")

#model 1#
timeStart = proc.time()
wS = 15
mk03.window <- windowGenerationPerPeriod(park = "MK", ride = 3, windowSize = wS, "01/01/2014", "12/31/2014")
timeEnd = proc.time()
timeTaken = timeEnd - timeStart
mk04.window <- windowGenerationPerPeriod(park = "MK", ride = 4, windowSize = wS, "01/01/2014", "12/31/2014")
mk141.window <- windowGenerationPerPeriod(park = "MK", ride = 141, windowSize = wS, "01/01/2014", "12/31/2014")

mk0304.linear <- mk03.window
mk0304.linear$AvgPost04 <- mk04.window$AvgPost
mk0304.linear$AvgPost141 <- mk141.window$AvgPost
mk0304.linear <- mk0304.linear %>% filter(!is.na(AvgPost) & !is.na(AvgPost04) & !is.na(AvgPost141) & !is.na(AvgAct))
mk0304.linear <- mk0304.linear %>% filter(AvgPost < 1000, AvgPost04 < 1000, AvgPost141 < 1000)

linearModel <- lm(AvgAct ~ AvgPost + AvgPost04 + AvgPost141 + dayOfWeek + windowStartSinceOpen , data = mk0304.linear)
summary(linearModel)




#model 2#
mk0304.linear$month <- month(mk0304.linear$date)
mk0304.season1 <- mk0304.linear %>% filter(month %in% c(12,1,2))
mk0304.season2 <- mk0304.linear %>% filter(month %in% c(3,4,5))
mk0304.season3 <- mk0304.linear %>% filter(month %in% c(6,7,8))
mk0304.season4 <-  mk0304.linear %>% filter(month %in% c(9,10,11))

summary(lm(AvgAct ~ AvgPost + AvgPost04 + dayOfWeek + windowStartSinceOpen + AvgPost141, data = mk0304.season1))
summary(lm(AvgAct ~ AvgPost + AvgPost04 + dayOfWeek + windowStartSinceOpen + AvgPost141, data = mk0304.season2))
summary(lm(AvgAct ~ AvgPost + AvgPost04 + dayOfWeek + windowStartSinceOpen + AvgPost141, data = mk0304.season3))
summary(lm(AvgAct ~ AvgPost + AvgPost04 + dayOfWeek + windowStartSinceOpen + AvgPost141, data = mk0304.season4))

#model 3#
for(i in 1:nrow(mk0304.linear)){
  if(mk0304.linear$month[i] %in% c(12,1,2)){
    mk0304.linear$season[i] <- "winter"
  } else if(mk0304.linear$month[i] %in% c(3,4,5)){
    mk0304.linear$season[i] <- "spring"
  } else if(mk0304.linear$month[i] %in% c(6,7,8)){
    mk0304.linear$season[i] <- "summer"
  } else {mk0304.linear$season[i] <- "autumn"}
}

summary(lm(AvgAct ~ AvgPost + AvgPost04 + dayOfWeek + windowStartSinceOpen + AvgPost141 + season, data = mk0304.linear))


ex <- preProcess(park = "AK", ride = 07, window = 10, "08/03/2014", "08/07/2014")
windowGenerationPerPeriod(ex)
findStartTimePerDay(ex)


ak11.results <- windowGenerationPerPeriod(preProcess(park = "AK", ride = 11, window = 10, startDate = "08/01/2014", endDate = "08/31/2014"))
plot(1:length(avgRatio(ak11.results)),avgRatio(ak11.results), ylim = c(0, 5))
View(cbind(1:length(avgRatio(ak11.results)), avgRatio(ak11.results)))

####plot two different things on one graph
plot(1:length(avgRatio(ak11.results)),avgRatio(ak11.results), col="red",ylim = c(0, 10))
par(new=TRUE)
plot(1:length(avgDiff(ak11.results)),avgDiff(ak11.results),col="blue",ylab='',xlab='',yaxt="n",ylim = c(0, 10))
axis(side=4)
mtext(side=4,line=-1.5,'avgDiff(ak11.results))')
####

# ex <- ak20.results %>% filter(NumAct != 0, NumPost != 0)
# plot(table(ex$date))

# ex.post <- ak20.results %>% filter(NumPost != 0)
# plot(table(ex.post$date))

# ex.actual <- ak20.results %>% filter(NumAct != 0)
# plot(table(ex.actual$date))

fifteenRides <- c(1,2,23,5,3,6,27,41,16,30,13,40,4,141,28)
for(i in fifteenRides){
  nam <- paste0("mk", singleToDoubleDigit(i), "Results", 10, "Min")
  assign(nam,windowGenerationPerPeriod(park = "MK", ride = i,
                                       windowSize = 10, "01/01/2009", "12/31/2016"))
  save(list = nam, file = paste0("mk",singleToDoubleDigit(i),"Results",10,".Rdata"))
  
}
for(i in fifteenRides){
  nam <- paste0("mk", singleToDoubleDigit(i), "Results", 15, "Min")
  assign(nam,windowGenerationPerPeriod(park = "MK", ride = i,
                                       windowSize = 15, "01/01/2009", "12/31/2016"))
  save(list = nam, file = paste0("mk",singleToDoubleDigit(i),"Results",15,".Rdata"))
  
}
for(i in fifteenRides){
  nam <- paste0("mk", singleToDoubleDigit(i), "Results", 20, "Min")
  assign(nam,windowGenerationPerPeriod(park = "MK", ride = i,
                                       windowSize = 20, "01/01/2009", "12/31/2016"))
  save(list = nam, file = paste0("mk",singleToDoubleDigit(i),"Results",20,".Rdata"))
  
}


windowEntries <- c()
windowEntries <- sapply((0:numWindowsPerDay-1)*windowSize, totalMinToHours)
# windowEntries[numWindowsPerDay] <- totalMinToHours(1440)
val <- which(rep(c(TRUE,FALSE,FALSE), length.out = length(windowEntries)))  
plot(1:length(windowEntries), y values)
axis(1, at = val, labels = windowEntries[val], las = 3)




axis(side = 1, at = seq(0, 1440, by = 60), labels = sapply(seq(0, 1440, by = 60),totalMinToHours), las = 3)





dataLoop <- function (windowSize, 
                      startDate = "01/01/2014", 
                      endDate = "02/01/2014"){
  lat <- range(rideLatLon$Lat)
  lon <- range(rideLatLon$Long)
  geoBox <- make_bbox(lon, lat, f= 0.1)
  magicKingdom <- get_map(location = geoBox ,
                          maptype = "roadmap", source = "osm", color = "bw")
  magicKingdomMap <- ggmap(magicKingdom)
  
  startDateObject <- as.Date(startDate, "%m/%d/%Y")
  endDateObject <- as.Date(endDate, "%m/%d/%Y")
  
  
  windowPerDay <- ceiling(1440/windowSize)
  totalDays <- (endDateObject - startDateObject) + 1
  #totalWindows <- windowPerDay * totalDays
  
  
  windowEntries <- c()
  for(i in 1:(windowPerDay-1)){
    windowEntries[i] <- paste0(totalMinToHours((i-1)*windowSize), "-", totalMinToHours(i*windowSize))
  }
  windowEntries[windowPerDay] <- paste0(totalMinToHours((windowPerDay-1)*windowSize), "-", totalMinToHours(1440))
  
  
  fileNameList <- sapply(tolower(rideLatLon$ID), paste0, "Results",windowSize, ".Rdata")
  
  for(k in 1:length(fileNameList)){
    load(fileNameList[k])
  }
  
  
  objectNameList <- sapply(tolower(rideLatLon$ID), paste0, "Results", windowSize, "Min")
  
  #daysDate <- seq(startDateObject, endDateObject, by = "day")
  #daysDate <- rep(daysDate, each = windowPerDay)
  
  dataPlot <- data.frame(Lat = rep(rideLatLon$Lat,2), 
                         Lon = rep(rideLatLon$Long,2))
  dataPlot$colour <- rep(c("Average Actual Wait Times", 
                           "Average Posted Wait Times"), 
                         each = nrow(rideLatLon))
  
  dataPlot$Size <- NA
  
  dataListPost <- list()
  dataListAct <- list()
  for(k in 1:length(objectNameList)){
    dataPost <- data.frame(matrix(NA, nrow = windowPerDay, ncol = totalDays))
    dataAct <- data.frame(matrix(NA, nrow = windowPerDay, ncol = totalDays))
    for(t in 1:windowPerDay){
      dataFrameName <- objectNameList[k]
      dataFrame <- get(dataFrameName)
      dataFrame <- dataFrame[dataFrame$date >= startDateObject & 
                               dataFrame$date <= endDateObject,]
      dataPost[t,] <- t(subset(dataFrame, window == windowEntries[t], select = AvgPost))
      dataAct[t,] <- t(subset(dataFrame, window == windowEntries[t], select = AvgAct))
    }
    dataListPost[[k]] <- dataPost
    dataListAct[[k]] <- dataAct
  }
  names(dataListPost) <- objectNameList
  names(dataListAct) <- objectNameList
  
  for(i in 1:windowPerDay){
    for(j in 1:length(objectNameList)){
      dataPlot$Size[j] <- rowMeans(dataListAct[[j]], na.rm = TRUE)[i]
      dataPlot$Size[j - 1 + length(objectNameList)] <- rowMeans(dataListPost[[j]], na.rm = TRUE)[i]
    }
    
    dataPlot$Size <- unlist(dataPlot$Size)
    dataPlot$Size <- pi*(dataPlot$Size^2)
    min <- min(c(sapply(dataListPost, rowMeans, na.rm = TRUE), sapply(dataListAct, rowMeans, na.rm = TRUE)), na.rm = TRUE)
    max <- max(c(sapply(dataListPost, rowMeans, na.rm = TRUE), sapply(dataListAct, rowMeans, na.rm = TRUE)), na.rm = TRUE)
    min <- pi*(min^2)
    max <- pi*(max^2)
    pdfName <- paste0("flowPlot", i, ".pdf")
    val <- ceiling(max/20)
    magicKingdomMap +   
      geom_point(aes(x = Lon,y = Lat,
                     fill = colour, 
                     size = Size, 
                     alpha = 0.5),
                 data = dataPlot, 
                 colour = "black", 
                 shape = 21, 
                 na.rm = TRUE) +
      scale_size(limits = c(min,max)) +
      ggtitle(paste("Plot of Average Posted + Average Actual Wait Time for 
                    \n Magic Kindgom Rides", 
                    "from", startDateObject, "to", endDateObject,
                    windowEntries[i])) +
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
      guides(alpha = FALSE, fill = guide_legend(title = NULL)) 
    
    ggsave(pdfName, width = 8, height = 5, units = "in")
  }
}
```
#We cleaned and processed the data provided by Touring Plans to better examine the relationship between Disney's estimated wait times and actual wait times for Disney park rides. 
#We hope to predict actual wait times using posted wait times and discover any causal relationship between these two variables.  
