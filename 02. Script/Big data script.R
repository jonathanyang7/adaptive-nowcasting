# load in libraries
library(forecast)
library(ggplot2)
library(tidyr)

##
## Section 1: Date function -----
##

dates <- function(start, end){
  # Generates a sequence of months for a period specified by parameters start
  # and end
  # 
  # :param start: date at start of sequence in the form %Y%m%d- chr format
  # :param end: date at the end of sequence %Y%m%d - chr format
  # 
  # :return: sequence of dates in date format
  
  startDate <- as.Date(start, "%Y%m%d")
  endDate <- as.Date(end, "%Y%m%d")
  period <- seq(startDate, endDate, by = "month")
  
  return (period)
}

##
## Section 2: Google Trends -----
##

worldwideSearch <- function(filename = worldwideFilename){
  # Reads in the Google Trends Data for worldwide searches
  # 
  # :param filename: directory and filename of Trends file - chr object
  # 
  # :return: dataframe with  
  #   column 1: the month - chr objects
  #   column 2: worldwide searches for Tower of London - int objects
  #   column 3: worldwide searches for Stonehenge - int objects
  #   column 4: worldwide searches for Giant's Causeway - int objects
  #   column 5: worldwide searches for London Eye - int objects
  #   column 6: worldwide searches for Royal Mile - int objects
  
  df <- read.csv(filename, skip = 2, na.strings = "<1") #turn all <1 values to NA
  df[is.na(df)] <- 0 #turn all NA values to zero
  colnames(df) <- c("month", "wwTOL", "wwS", "wwGC",
                    "wwLE", "wwRM")
  
  df$month <- dates(start = "20040101", end = "20191101") #create a column for the date

  return (df)
}

totalWorldwide <- function(dfww = worldwideSearches){
  # Calculates the total search volume for all 5 topics
  # 
  # :param dfww: dataframe with  
  #   column 1: the month - chr objects
  #   column 2: worldwide searches for Tower of London - int objects
  #   column 3: worldwide searches for Stonehenge - int objects
  #   column 4: worldwide searches for Giant's Causeway - int objects
  #   column 5: worldwide searches for London Eye - int objects
  #   column 6: worldwide searches for Royal Mile - int objects
  #
  # :return: dataframe with same columns as dfww but with an additional column
  #   for the total searches
  #   column 1 to 6: same as dfww
  #   column 7: total searches for all 5 topics
  
  df <- dfww
  df$total <- (dfww$wwTOL+ dfww$wwS + dfww$wwGC + dfww$wwLE + dfww$wwRM)
  
  return (df)
}

##
## Section 3: ONS data -----
##

visits <- function(filename = visitsFilename){
  # Read in ONS data for monthly overseas visits to the UK from 2004-01 to 2019-11
  # 
  # :param filename: location of ONS visits file - chr object
  # 
  # :return: dataframe with 
  #   column 1: the month - Date objects
  #   column 2: monthly overseas visits to the UK - int objects
  
  df <- read.csv(filename, skip = 395, header = FALSE, nrows = 191,
                 col.names = c("month", "visits")) #relevant data is from line 396
                                                   # to line 586
  df$month <- dates(start = "20040101", end = "20191101") #create a column for the date
  
  return (df)
}

earnings <- function(filename = earningsFilename){
  # Reads in ONS data for monthly UK tourism earnings from 2004-01 to 2019-11
  # 
  # :param filename: location of ONS earnings file - chr object
  # 
  # :return: dataframe with 
  #   column 1: the month - Date objects
  #   column 2: monthly UK tourism earnings - int objects
  
  df <- read.csv(filename, skip = 395, header = FALSE, nrows = 191, 
                 col.names = c("month", "earnings")) #relevant data is from line 396
                                                     # to line 586
  
  df$month <- dates(start = "20040101", end = "20191101") #create a column for the date
  
  return (df)
}

##
## Section 4: Merge and difference the data -----
##

mergeAll <- function(dfVisits = overseasVisits, dfEarnings = tourismEarnings,
                     dfTrends = allTrends){
  # Merges the Trends and ONS data into a single dataframe
  # 
  # :param dfVisits: dataframe that contains monthly overseas visits to the UK
  # :param dfEarnings: dataframe that contains monthly UK tourism earnings
  # :param dfTrends: dataframe that has Google Trends search volume for 5 landmark topics 
  #   individually and collectively 
  # 
  # :return: dataframe with columns
  #   column 1: the month - Date objects
  #   column 2: monthly overseas visits to the UK - int objects
  #   column 3: monthly UK tourism earnings - int objects
  #   column 4: worldwide search volume for Tower of London topic - int objects
  #   column 5: worldwide search volume for Stonehenge topic - int objects
  #   column 6: worldwide search volume for Giant's Causeway topic - int objects
  #   column 7: worldwide search volume for London Eye topic - int objects
  #   column 8: worldwide search volume for Royal Mile topic - int objects
  #   column 9: total worldwide search volume for all 5 topic - int objects
  # 
  
  dfONS <- merge(dfVisits, dfEarnings, by = "month")
  dfAll <- merge(dfONS, dfTrends, by = "month")
  
  colnames(dfAll) <- c("month", "visits", "earnings", "TOL", "S", "GC", "LE", 
                       "RM", "total")
  
  return (dfAll)
}

timeseriesDiffAll <- function(dfAll = allData){
  # Removes trend and seasonality from a dataframe and returns a timeseries object
  # 
  # :param dfAll: dataframe that contains ONS data for monthly tourism earnings and 
  #   visits, as well as Google Trends data on monthly searches for 5 landmarks
  # 
  # :return: timeseries object containing ONS and Trends data 
  # without trends and seasonality, and for period 2005 to 2020 
  
  startDate = c(2004,01) # Start of analysis period
  
  # Set up time series objects for our monthly data
  visits <- ts(dfAll$visits, frequency = 12, start = startDate)
  earnings <- ts(dfAll$earnings, frequency = 12, start = startDate)
  TOL <- ts(dfAll$TOL, frequency = 12, start = startDate)
  S <- ts(dfAll$S, frequency = 12, start = startDate)
  GC <- ts(dfAll$GC, frequency = 12, start = startDate)
  LE <- ts(dfAll$LE, frequency = 12, start = startDate)
  RM <- ts(dfAll$RM, frequency = 12, start = startDate)
  total <- ts(dfAll$total, frequency = 12, start = startDate)
  
  # Collate all the timeseries objects 
  allDataTs <- ts.intersect(visits, earnings, TOL, S, GC, LE, RM, total) 
  
  allDataTs <- diff(allDataTs, lag = 1) #remove trends 
  allDataTs <- diff(allDataTs, lag = 12) #remove seasonality
  
  return (allDataTs)
}

dfDiffAll <- function(tsAll = allDataDiffTs){
  # Converts the timeseries object containing all the trend and seasonality 
  # adjusted Trends and ONS data to a dataframe
  
  df <- as.data.frame(tsAll) #convert to data frame
  colnames(df) <- c("visits", "earnings", "TOL", "S", "GC", "LE", "RM", "total")
  month <- dates(start = "20050201", end = "20191101") #vector with months
  df <- cbind(month, df) #add the months to the data frame
  
  return (df)
}

##
## Section 5: Statistical methods -----
##

shapiro <- function(dfall = allDataDiff){
  # Conducts a Shapiro-Wilk test for normality on all the data
  # 
  # :param dfall: dataframe that contains monthly ONS and trends data 
  #   with trends and seasonality removed
  # 
  # :return: dataframe with 
  #   column 1: name of variable we are testing - chr objects
  #   column 2: FDR adjusted p-values for the shapiro wilk test - num objects
  # 
  
  # Run Shapiro-Wilk tests for monthly overseas visits, monthly tourism earnings
  # and all search terms (as well as the total of all search terms)
  visits <- shapiro.test(dfall$visits)
  earnings <- shapiro.test(dfall$earnings)
  TOL <- shapiro.test(dfall$TOL)
  S <- shapiro.test(dfall$S)
  GC <- shapiro.test(dfall$GC)
  LE <- shapiro.test(dfall$LE)
  RM <- shapiro.test(dfall$RM)
  total <- shapiro.test(dfall$total)
  
  # Collate p-values into a dataframe to return 
  pValues <- c(visits$p.value, earnings$p.value, TOL$p.value, S$p.value,
               GC$p.value, LE$p.value, RM$p.value, total$p.value)
  
  pValues <- p.adjust(pValues, method = "fdr") #adjusts the p-values using FDR
  
  # Collate the W statistic
  wStat <- c(visits$statistic, earnings$statistic, TOL$statistic, S$statistic,
             GC$statistic, LE$statistic, RM$statistic, total$statistic)
  
  # Create a dataframe to display the results of Shapiro-Wilk
  testSubject = c("visits", "earnings", "TOL", "S", "GC", "LE", "RM", 
                  "total searches")
  df <- data.frame(testSubject, wStat, pValues)
  colnames(df) <- c("Shapiro test for:","W statistic", "P values")
  
  return (df)
}

kendallVisits <- function(dfall = allDataDiff){
  # Runs a Kendall Tau test for correlation between all search terms and 
  # monthly overseas visits to the UK
  # 
  # :param dfall: dataframe that contains monthly ONS and trends data 
  #   with trends and seasonality removed
  # 
  # :return: dataframe with
  #   column 1: the search term - chr objects
  #   column 2: FDR adjusted p-values for each search term - num objects
  #   column 3: tau values for each search term - num objects
  # 
  
  # Run correlation tests for all search terms with monthly overseas visits
  TOLvisits <- cor.test(dfall$visits, dfall$TOL, method = "kendall")
  Svisits <- cor.test(dfall$visits, dfall$S, method = "kendall")
  GCvisits <- cor.test(dfall$visits, dfall$GC, method = "kendall")
  LEvisits <- cor.test(dfall$visits, dfall$LE, method = "kendall")
  RMvisits <- cor.test(dfall$visits, dfall$RM, method = "kendall")
  ALLvisits <- cor.test(dfall$visits, dfall$total, method = "kendall")
  
  # Collate the p-values and put into pValues
  pValues <- c(TOLvisits$p.value, Svisits$p.value, GCvisits$p.value,
                    LEvisits$p.value, RMvisits$p.value, ALLvisits$p.value)
  pValues <- p.adjust(pValues, method = "fdr") #adjusts the p-values using FDR
  
  # Collate the Tau values and put into estimates 
  estimates <- c(TOLvisits$estimate, Svisits$estimate, GCvisits$estimate, 
                LEvisits$estimate, RMvisits$estimate, ALLvisits$estimate)
  searchTerm <- c("TOL", "S", "GC", "LE", "RM", "total searches")
  
  # Collate the p-values and Tau values into a dataframe to be returned
  df <- data.frame(searchTerm, pValues, estimates)
  colnames(df) <- c("search term", "adjusted p-values", "tau estimates")
  
  return (df)
}

kendallEarning <- function(dfall = allDataDiff){
  # Runs a Kendall Tau test for correlation between all search terms and 
  # monthly UK tourism earnings
  # 
  # :param dfall: dataframe that contains monthly ONS and trends data 
  #   with trends and seasonality removed
  # 
  # :return: dataframe with
  #   column 1: the search term - chr objects
  #   column 2: FDR adjusted p-values for each search term - num objects
  #   column 3: tau values for each search term - num objects
  # 
  
  # Run Kendall Tau for correlation between search terms and earnings
  TOLearn <- cor.test(dfall$earnings, dfall$TOL, method = "kendall")
  Searn <- cor.test(dfall$earnings, dfall$S, method = "kendall")
  GCearn <- cor.test(dfall$earnings, dfall$GC, method = "kendall")
  LEearn <- cor.test(dfall$earnings, dfall$LE, method = "kendall")
  RMearn <- cor.test(dfall$earnings, dfall$RM, method = "kendall")
  ALLearn <- cor.test(dfall$earnings, dfall$total, method = "kendall")
  
  # Collate the p-values into pValues
  pValues <- c(TOLearn$p.value, Searn$p.value, GCearn$p.value,
               LEearn$p.value, RMearn$p.value, ALLearn$p.value)
  pValues <- p.adjust(pValues, method = "fdr") #adjust p-values using FDR
  
  # Collate the Tau values and put into estimates 
  estimates <- c(TOLearn$estimate, Searn$estimate, GCearn$estimate, 
                 LEearn$estimate, RMearn$estimate, ALLearn$estimate)
  searchTerm <- c("TOL", "S", "GC", "LE", "RM", "total searches")
  
  # Collate the p-values and Tau values into a dataframe to be returned
  df <- data.frame(searchTerm, pValues, estimates)
  colnames(df) <- c("search term", "adjusted p-values", "tau estimates")
  
  return (df)
}

MAPEchange <- function(base, trend){
  # Calculates the change in Mean Absolute Percentage Error between a 'base' ARIMA model
  # and an 'advanced' ARIMA model
  # 
  # :param base: list containing results of base ARIMA model 
  # :param adv: list containing results of trends-fitted ARIMA model
  # 
  # :return: the change in MAPE between base and trend ARIMA models - num object
  
  baseMAPE <- mean(abs(base$residuals/base$x)) #calculate MAE for basefit
  trendMAPE <- mean(abs(trend$residuals/base$x)) #calculate MAE for trends fit
  change <- (1 - (trendMAPE/baseMAPE)) * 100 
  
  return (change)
}

MAPE <- function(base, TOL, S, GC, LE, RM, TOTAL){
  # Finds the changes in MAPE for a base ARIMA model and trends-fitted
  # ARIMA models for each search term
  # 
  # :param base: base ARIMA model
  # :param TOL: visits ARIMA model with Tower of London search data as an external regressor
  # :param S: visits ARIMA model with Stonehenge search data as an external regressor
  # :param GC: visits ARIMA model with Giant's Causeway search data as an external regressor
  # :param LE: visits ARIMA model with London Eye search data as an external regressor
  # :param RM: visits ARIMA model with Royal Mile search data as an external regressor
  # :param TOTAL: visits ARIMA model with total search data as an external regressor
  # 
  # :return: dataframe with
  #   column 1: search term - chr object
  #   column 2: change in MAPE - num object
  # 
  
  # Find the changes in MAPE
  TOLchange <- MAPEchange(base, TOL)
  Schange <- MAPEchange(base, S)
  GCchange <- MAPEchange(base, GC)
  LEchange <- MAPEchange(base, LE)
  RMchange <- MAPEchange(base, RM)
  totalchange <- MAPEchange(base, TOTAL)
  
  searchTerms <- c("TOL", "S", "GC", "LE", "RM", "total searches") 
  MAPE <- c(TOLchange, Schange, GCchange, LEchange, RMchange,
            totalchange) #store changes in MAPE in MAPE object
  
  # create dataframe to output
  df <- data.frame(searchTerms, MAPE)
  colnames(df) <- c("search terms", "change in Mean Absolute Percentage Error (%)")
  
  return (df)
}

##
## Section 6: Plots -----
##

plot2 <- function(base, trends, plotTitle){
  # Plots a mirrored bar chart for earnings ARIMA models
  # ARIMA plotted above y=0 and ARIMA with trends plotted below y=0
  # 
  # :param base: base earnings ARIMA model
  # :param trends: trends-fitted earnings ARIMA model
  # :param searchTermString: string for the title of the graph
  # 
  # :return: the desired plot
  # 
  
  # Create a dataframe to store the month and absolute percentage errors for the 
  # base and trends-fitted ARIMA models
  month <- dates(start = "20040101", end = "20191101")
  baseAPE <- abs(base$residuals / base$x) * 100 #calculate base absolute percentage error
  trendsAPE <- abs(trends$residuals / trends$x) * 100 #calculate trends fitted absolute percentage error
  df <- data.frame(month, baseAPE, -trendsAPE) #we want the trends plot to be under the y=0 line
  colnames(df) <- c("month", "Above", "Below")
  
  df <- gather(df, model,errors,-c(month)) #convert to long format
  
  p <- ggplot(df, aes(x=month, y=errors, fill=model)) + 
    geom_bar(stat="identity", position="identity") +
    scale_y_continuous(breaks=seq(-20,20,by=10),labels=abs(seq(-20,20,by=10))) +
    theme_classic() +
    labs(x = "Time (months)", y = "Absolute percentage error (%)",
         title = plotTitle) +
    scale_fill_manual(values = c("#3182bd", "#9ecae1"), labels = c("ARIMA", "ARIMA with trends")) +
    theme(plot.title = element_text(size = 11),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 11),
          legend.position = c(0.65, 0.95),
          legend.direction = "vertical",
          legend.title = element_blank()) 
  return (p)
} 

plot2x <- function(base, trends, plotTitle){
  # Plots a mirrored bar chart for earnings ARIMA models without y axis labels
  # ARIMA plotted above y=0 and ARIMA with trends plotted below y=0 
  # 
  # :param base: base earnings ARIMA model
  # :param trends: trends-fitted earnings ARIMA model
  # :param searchTermString: string for the title of the graph
  # 
  # :return: the desired plot
  # 
  
  # Create a dataframe to store the month and absolute percentage errors for the 
  # base and trends-fitted ARIMA models
  month <- dates(start = "20040101", end = "20191101")
  baseAPE <- abs(base$residuals / base$x) * 100 #calculate base absolute percentage error
  trendsAPE <- abs(trends$residuals / trends$x) * 100 #calculate trends absolute percentage error
  df <- data.frame(month, baseAPE, -trendsAPE) #trends bar chart under y=0
  colnames(df) <- c("month", "Above", "Below")
  df <- gather(df, model,errors,-c(month)) #convert to long format
  
  p <- ggplot(df, aes(x=month, y=errors, fill=model)) + 
    geom_bar(stat="identity", position="identity") +
    scale_y_continuous(breaks=seq(-20,20,by=10),labels=abs(seq(-20,20,by=10))) +
    theme_classic() +
    labs(x = "Time (months)", 
         title = plotTitle) +
    scale_fill_manual(values = c("#3182bd", "#9ecae1"), labels = c("ARIMA", "ARIMA with trends")) +
    theme(plot.title = element_text(size = 11),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 11),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.65, 0.95),
          legend.direction = "vertical",
          legend.title = element_blank()) 
  
  return (p)
}

plot3 <- function(base, trends, plotTitle){
  # Plots a mirrored bar chart for visits ARIMA models 
  # ARIMA plotted above y=0 and ARIMA with trends plotted below y=0 
  # 
  # :param base: base visits ARIMA model
  # :param trends: trends-fitted visits ARIMA model
  # :param searchTermString: string for the title of the graph
  # 
  # :return: the desired plot
  # 
  
  month <- dates(start = "20040101", end = "20191101")
  baseAPE <- abs(base$residuals / base$x) * 100
  trendsAPE <- abs(trends$residuals / trends$x) * 100
  df <- data.frame(month, baseAPE, -trendsAPE) #trends model below y=0
  colnames(df) <- c("month", "Above", "Below")
  
  df <- gather(df, model,errors,-c(month)) #convert to long format
  
  p <- ggplot(df, aes(x=month, y=errors, fill=model)) + 
    geom_bar(stat="identity", position="identity") +
     scale_y_continuous(breaks=seq(-20,20,by=5),labels=abs(seq(-20,20,by=5))) +
    theme_classic() +
    labs(x = "Time (months)", y = "Absolute percentage error (%)",
         title = plotTitle) +
    scale_fill_manual(values = c("#3182bd", "#9ecae1"), labels = c("ARIMA", "ARIMA with trends")) +
    theme(plot.title = element_text(size = 11),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 11),
          legend.position = c(0.8, 0.95),
          legend.direction = "vertical",
          legend.title = element_blank()) 
  return (p)
}

plot3x <- function(base, trends, plotTitle){
  # Plots a mirrored bar chart for visits ARIMA models without y axis labels
  # ARIMA plotted above y=0 and ARIMA with trends plotted below y=0 
  # 
  # :param base: base visits ARIMA model
  # :param trends: trends-fitted visits ARIMA model
  # :param searchTermString: string for the title of the graph
  # 
  # :return: the desired plot
  # 
  
  month <- dates(start = "20040101", end = "20191101")
  baseAPE <- abs(base$residuals / base$x) * 100
  trendsAPE <- abs(trends$residuals / trends$x) * 100
  df <- data.frame(month, baseAPE, -trendsAPE) #trends below y=0
  colnames(df) <- c("month", "Above", "Below")
  
  df <- gather(df, model,errors,-c(month))#convert to long format
  
  p <- ggplot(df, aes(x=month, y=errors, fill=model)) + 
    geom_bar(stat="identity", position="identity") +
    scale_y_continuous(breaks=seq(-20,20,by=5),labels=abs(seq(-20,20,by=5))) +
    theme_classic() +
    labs(x = "Time (months)", y = "Absolute percentage error (%)",
         title = plotTitle) +
    scale_fill_manual(values = c("#3182bd", "#9ecae1"), labels = c("ARIMA", "ARIMA with trends")) +
    theme(plot.title = element_text(size = 11),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 11),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.8, 0.95),
          legend.direction = "vertical",
          legend.title = element_blank()) 
  return (p)
}

##
## Section 7: Running everything -----
##

waitForEnter <- function(){
  writeLines ("\n -------------------")
  cat ("Press [enter] to continue")
  writeLines ("\n -------------------")
  line <- readline()
}

doEverything <- function(){
  # Get the Google Trends data
  worldwideFilename <- "multiTimeline.csv"
  worldwideSearches <- worldwideSearch(filename = worldwideFilename)
  allTrends <- totalWorldwide(dfww = worldwideSearches)
  
  # Get the ONS data
  visitsFilename <- "visits.csv"
  earningsFilename <- "earnings.csv"
  overseasVisits <- visits(filename = visitsFilename)
  tourismEarnings <- earnings(filename = earningsFilename)
  
  # Merge all the data
  allData <- mergeAll(dfVisits = overseasVisits, dfEarnings = tourismEarnings,
                      dfTrends = allTrends)
  
  # Remove trends and seasonality
  allDataDiffTs <- timeseriesDiffAll(dfAll = allData)
  allDataDiff <- dfDiffAll(tsAll = allDataDiffTs)
  
  # Run a Shapiro-Wilk test for normality 
  writeLines ("Running a Shapiro-Wilk test for normality")
  waitForEnter()
  print(shapiro(dfall = allDataDiff))
  waitForEnter()
  
  # Run a Kendall Tau for Google searches against tourism visits
  writeLines ("Running Kendall Tau for Google searches against tourism visits")
  waitForEnter()
  print(kendallVisits(dfall = allDataDiff))
  waitForEnter()
  
  # Run a Kendall Tau for Google searches against tourism earnings
  writeLines ("Running Kendall Tau for Google searches against tourism earnings")
  waitForEnter()
  print(kendallEarning(dfall = allDataDiff))
  waitForEnter()
  
  # Create ARIMA models
  
  # Base models
  baseVisit <- auto.arima(allData$visits)
  baseEarn <- auto.arima(allData$earnings)
  
  # Trends fitted models for visits
  trendsVisitTOL <- auto.arima(allData$visits, xreg = allData$TOL)
  trendsVisitS <- auto.arima(allData$visits, xreg = allData$S)
  trendsVisitGC <- auto.arima(allData$visits, xreg = allData$GC)
  trendsVisitLE <- auto.arima(allData$visits, xreg = allData$LE)
  trendsVisitRM <- auto.arima(allData$visits, xreg = allData$RM)
  trendsVisitTotal <- auto.arima(allData$visits, xreg = allData$total)

  # Trends fitted models for earnings
  trendsEarnTOL <- auto.arima(allData$earnings, xreg = allData$TOL)
  trendsEarnS <- auto.arima(allData$earnings, xreg = allData$S)
  trendsEarnGC <- auto.arima(allData$earnings, xreg = allData$GC)
  trendsEarnLE <- auto.arima(allData$earnings, xreg = allData$LE)
  trendsEarnRM <- auto.arima(allData$earnings, xreg = allData$RM)
  trendsEarnTotal <- auto.arima(allData$earnings, xreg = allData$total)
  
  # Find the change in Mean Absolute Percentage Error for the base and trends fitted 
  # models 
  writeLines("Finding change in mean absolute percentage error for base and 
  trends fitted visits ARIMA models")
  waitForEnter()
  visitsMAPEchange <- MAPE(base = baseVisit, TOL = trendsVisitTOL, S = trendsVisitS,
                           GC = trendsVisitGC, LE = trendsVisitLE, RM = trendsVisitRM,
                           TOTAL = trendsVisitTotal) #for visits
  print(visitsMAPEchange)
  waitForEnter()
  
  writeLines("Finding change in mean absolute percentage error for base and 
  trends fitted visits ARIMA models")
  waitForEnter()
  earningsMAPEchange <- MAPE(base = baseEarn, TOL = trendsEarnTOL, S = trendsEarnS,
                           GC = trendsEarnGC, LE = trendsEarnLE, RM = trendsEarnRM,
                           TOTAL = trendsEarnTotal) #for visits
  print (earningsMAPEchange)
  waitForEnter()
  
  # Visits plots
  writeLines("Plotting changes in absolute percentage error for visits models")
  waitForEnter()
  print (plot3 (base = baseVisit, trends = trendsVisitTOL, "Tower of London"))
  waitForEnter()
  print (plot3 (base = baseVisit, trends = trendsVisitS, "Stonehenge"))
  waitForEnter()
  print (plot3 (base = baseVisit, trends = trendsVisitGC, "Giant's Causeway"))
  waitForEnter()
  print (plot3 (base = baseVisit, trends = trendsVisitLE, "London Eye"))
  waitForEnter()
  print (plot3 (base = baseVisit, trends = trendsVisitRM, "Royal Mile"))
  waitForEnter()
  print (plot3 (base = baseVisit, trends = trendsVisitTotal, "Total searches"))
  waitForEnter()
  
  # Earnings plots
  writeLines("Plotting changes in absolute percentage error for earnings models")
  waitForEnter()
  print (plot2 (base = baseEarn, trends = trendsEarnTOL, "Tower of London"))
  waitForEnter()
  print (plot2 (base = baseEarn, trends = trendsEarnS, "Stonehenge"))
  waitForEnter()
  print (plot2 (base = baseEarn, trends = trendsEarnGC, "Giant's Causeway"))
  waitForEnter()
  print (plot2 (base = baseEarn, trends = trendsEarnLE, "London Eye"))
  waitForEnter()
  print (plot2 (base = baseEarn, trends = trendsEarnRM, "Royal Mile"))
  waitForEnter()
  print( plot2 (base = baseEarn, trends = trendsEarnTotal, "Total searches"))

}