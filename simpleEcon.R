##################################################################################
## Simple look at econometrics
## overlaying organic rank, spend and share
## ps October 2015
##
## Data used- 
## monthly share data from Jon Wood Oct 2015
## monthly share of voice data from Jon Wood Oct 2015
## daily organic rank for key home and car terms from linkdex frmo Andy Kimberley
##
#################################################################################

#################################################################################
## set up

  ## locations
  #homeDir = '/Users/piers.stobbs/Documents/piers/Box Sync/datascience/simpleEcon/'
  #setwd = homeDir
  homeDir = getwd()
  dataDir = '/data/'
  
  ## libraries
  library(data.table)
  library(ggplot2)
  library(lubridate)
  #detach("package:lubridate", unload=TRUE)
  
  ## load raw data
  #### done initial clean up in excel- formatting, removing chars etc
  carShareFile = 'carShare.csv'
  homeShareFile = 'homeShare.csv'
  carRankFile = 'carRank.csv'
  homeRankFile = 'homeRank.csv'
  voiceShareFile = 'shareVoice.csv'
  
  carShareRaw = fread(paste(homeDir, dataDir, carShareFile, sep=""))
  homeShareRaw = fread(paste(homeDir, dataDir, homeShareFile, sep=""))
  carRankRaw = fread(paste(homeDir, dataDir, carRankFile, sep=""))
  homeRankRaw = fread(paste(homeDir, dataDir, homeRankFile, sep=""))
  voiceShareRaw = fread(paste(homeDir, dataDir, voiceShareFile, sep=""))
  
## end set up
#################################################################################
  
  
  
#################################################################################
## clean up data
  ## make sure dates all ok
  str(carShareRaw)
  carShareRaw[, date:= as.Date(date, "%d/%m/%y")]
  str(homeShareRaw)
  homeShareRaw[, date:= as.Date(date, "%d/%m/%y")]
  str(carRankRaw)
  carRankRaw[, date:= as.Date(date, "%d/%m/%y")]
  str(homeRankRaw)
  homeRankRaw[, date:= as.Date(date, "%d/%m/%y")]
  str(voiceShareRaw)
  voiceShareRaw[, date:= as.Date(date, "%d/%m/%y")]
  
  ## focus on ranks- 
  # build month column
  homeRankRaw[, day:=date]
  homeRankRaw[, date:=floor_date(date, "month")]
  carRankRaw[, day:=date]
  carRankRaw[, date:=floor_date(date, "month")]

  #quick look at data
  summary(homeRankRaw)
  #set all zeros to max, and cap at max
  homeTerms = setdiff(names(homeRankRaw), c("date", "day"))
  carTerms = setdiff(names(carRankRaw), c("date", "day"))
  maxRank = 10
  for (c in 1:length(homeTerms)) {
    tmpIdx = which(homeRankRaw[, homeTerms[c], with=FALSE]==0 | homeRankRaw[, homeTerms[c], with=FALSE]>maxRank) 
    homeRankRaw[tmpIdx, homeTerms[c]:=maxRank, with=FALSE]
  }
  for (c in 1:length(carTerms)) {
    tmpIdx = which(carRankRaw[, carTerms[c], with=FALSE]==0 | carRankRaw[, carTerms[c], with=FALSE]>maxRank) 
    carRankRaw[tmpIdx, carTerms[c]:=maxRank, with=FALSE]
  }
  
  #generate monthly averages
  homeRank = homeRankRaw[, lapply(.SD, mean, na.rm=TRUE), by=date, .SDcols=c(homeTerms) ] 
  carRank = carRankRaw[, lapply(.SD, mean, na.rm=TRUE), by=date, .SDcols=c(carTerms) ] 
  
  ##pull together relevant data
  #set up new monthly data table
  homeDT = data.table(date = unique(homeShareRaw[, date]), key="date")
  carDT = data.table(date = unique(carShareRaw[, date]), key="date")
  
  #pull in sov
  setkey(voiceShareRaw, date)
  homeDT[, sov:=voiceShareRaw[homeDT][, MSM]]
  carDT[, sov:=voiceShareRaw[carDT][, MSM]]

  #pull in rank
  setkey(homeRank, date)
  homeDT = homeRank[homeDT]
  setkey(carRank, date)
  carDT = carRank[carDT]

  #pull in dependent variable - msm share
  setkey(homeShareRaw, date)
  homeDT[, share:=homeShareRaw[homeDT][, MSM]]
  setkey(carShareRaw, date)
  carDT[, share:=carShareRaw[carDT][, MSM]]
  
  #strip whitespace from any columns
  setnames(homeDT, colnames(homeDT), gsub(" ", "", colnames(homeDT)))
  setnames(carDT, colnames(carDT), gsub(" ", "", colnames(carDT)))
  
## end clean up data
#################################################################################
  
#################################################################################
## model
    
## split into train and test
set.seed(1234)
n = nrow(homeDT)  
trainPct = 0.75

trainId = sort(sample(seq(1,n,1), floor(trainPct*n), replace=FALSE))

trainCar = carDT[trainId, ]
testCar = carDT[-trainId, ]
trainHome = homeDT[trainId, ]
testHome = homeDT[-trainId, ]



library(ggplot2)
library(GGally)
library(scales)

# lets explore the correlations and distributions of the variables
# matrix scatter plot

ggpairs(trainHome[, !"date", with=FALSE], 
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")



#ggpairs(trainHome[, c(colnames(rawData)[grep("bs", colnames(rawData))], "y"), with=FALSE], 
#        diag=list(continuous="density",   discrete="bar"), axisLabels="show")



  
## end model
#################################################################################
  
   