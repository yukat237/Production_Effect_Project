####------------------------####
####  Thesis2 Data Analysis ####  
####----------------------- ####

###############################################################################
######################### code to test 1 data #################################
###############################################################################

#------library------

library(dplyr)


####--- data cleaning ----####

# set your directory and folder name that contains all data you're processing 
# [need to modify these for your use!!] 
dir <- "/Users/yuka/iCloudDrive/@midd/Middlebury Senior/Thesis2/"
folderName <- "code_test"

#making a file name list
dataLoc <- paste0(dir, "/", folderName)
setwd(dataLoc)
fileList <- list.files(dataLoc)
fileNum <- length(fileList)

# make a dataframe for the full data
fullDat <- data.frame(ptID = numeric(0),
                      session = character(0),
                      resp = character(0))


#process each data file
tpath <- fileList[16]

  # import data
  originalCSV<-read.csv(tpath)
  # leave only needed columns
  dat<-data.frame(originalCSV$participant, originalCSV$session, originalCSV$textbox.text)
  # rename columns
  colnames(dat) <- c("ptID","session","resp")
  
#  dat <- dat %>% rename(
#    "ptID" = "originalCSV.participant",
#    "session" = "originalCSV.session",
#    "resp" = "originalCSV.textbox.text")
  
  # remove rows that are empty in resp column
  dat <- dat[dat$resp != "",]
  # append this df to the df with all data
  fullDat<-rbind(fullDat,dat)
  

#4/29/23 -- \
# this script works, but if the output probably will have an empty line. if all files are like this, need to add another line to erase that row.
# also this script saves the data separately as csv, but i could also jsut concatenate all data in 1 file, so that RAs can work more efficiently?
# --> if doing that, instead of the lines to save output as csv, i can write append line to a dataframe that was prepared before this lapply loop.

###############################################################################
######################### code to loop ALL data  ##############################
###############################################################################
  #"C:\Users\yuka\Desktop\Production_Effect_Project\retest.csv"
  
  dir <- "/Users/yuka/Desktop/Production_Effect_Project"
  folderName <- "data_midd"
  
  #making a file name list
  dataLoc <- paste0(dir, "/", folderName)
  setwd(dataLoc)
  fileList <- list.files(dataLoc)
  fileNum <- length(fileList)
  
  # make a dataframe for the full data
  fullDat <- data.frame(ptID = numeric(0),
                        session = character(0),
                        resp = character(0))

  #process each data file 
  lapply(X = fileList, FUN = function(path) {

  # import data
  originalCSV<-read.csv(path)
  # leave only needed columns
  dat<-data.frame(originalCSV$participant, originalCSV$session, originalCSV$textbox.text)
  # rename columns
  colnames(dat) <- c("ptID","session","resp")
  # remove rows that are empty in resp column
  dat <- dat[dat$resp != "",]
  # append this df to the df with all data
  fullDat<-rbind(fullDat,dat)
  })
  
#or, loop
  for (i in 1:fileNum){
  
    # import data
    originalCSV<-read.csv(fileList[i])
    # leave only needed columns
    dat<-data.frame(originalCSV$participant, originalCSV$session, originalCSV$textbox.text)
    # rename columns
    colnames(dat) <- c("ptID","session","resp")
    # remove rows that are empty in resp column
    dat <- dat[dat$resp != "",]
    # fix errors
    if (i == 26){
      dat$ptID <- replace(dat$ptID, dat$ptID == '32','31')
    }
    # append this df to the df with all data
    fullDat<-rbind(fullDat,dat)  
    
  }
  
  # set the output path
  pathOut <- paste0(dataLoc,'/fulldata.csv')
  
  # save the output
  write.csv(fullDat, pathOut, row.names = F)
  
  #notes:
  #5/5/23, 8:12pm: stops at i = 25 , turned out participant 31 did not have any data.  removed from the folder and rerun.
  #                there are two "32"s. first 8:55am is ran with AF list. 
  #                the other is ran with AE list. AE was suppsoed to be for pt31. so i guess mislabeled.  
  #8:28pm: finally for loop worked! 
  #        however, if you look at the output excel file, those who did not comma separated their resp were not correctly read. have to think of reading these.      
  
#exploration
  fileList[16]
  pathOut <- as.character('/Users/yuka/Desktop/Production_Effect_Project/testfull.xlsx')
  write.xlsx(fullDat, pathOut) #this works but now not separated by anything -- all concatenated. 
  library("readr")
  library(openxlsx)
  paste0()
  
  ###################################################################################
  #                              OLD
  #################################################################################
  
  
  
  
#-----prerequisites-----
# 1) all the data are in 1 folder
# 2) set your directory and folder name for the data on Line19 and 20


"C:\Users\yuka\iCloudDrive\@midd\Middlebury Senior\Thesis2\data_midd"

#------library------

library(dplyr)


####--- data cleaning ----####

# set your directory and folder name that contains all data you're processing 
# [need to modify these for your use!!] 
dir <- "/Users/yuka/iCloudDrive/@midd/Middlebury Senior/Thesis2/"
folderName <- "data_midd"

#making a file name list
dataLoc <- paste0(dir, "/", folderName)
setwd(dataLoc)
fileList <- list.files(dataLoc)
fileNum <- length(fileList)

#process each data file

lapply(X = fileList, FUN = function(path) {
  # import data
  originalCSV<-read.csv(path)
  # leave only needed columns
  dat<-data.frame(originalCSV$participant, originalCSV$session, originalCSV$textbox.text)
  # rename columns
  dat <- dat %>% rename(
    ptID = originalCSV.participant,
    session = originalCSV.session,
    resp = originalCSV.textbox.text
  )
  # set the output path
  pathOut <- paste0(dir, '/', dat[1,1], '.csv')
  
  # save the output
  write.csv(dat, pathOut, row.names = F)
  
})
