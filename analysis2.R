                  ####------------------------####
                  ####  Thesis2 Data Analysis ####  
                  ####----------------------- ####


######################### main code to process ALL data  ##############################

  #set directory & folder name that has all the csv data
  dir <- "/Users/yuka/Desktop/Production_Effect_Project"
  folderName <- "data_midd"
  
  #making a file name list
  dataLoc <- paste0(dir, "/", folderName)
  setwd(dataLoc)
  fileList <- list.files(dataLoc)
  fileNum <- length(fileList)
  
  # prepare a dataframe for the full data
  fullDat <- data.frame(ptID = numeric(0),
                        session = character(0),
                        resp = character(0))

  #loop through all data
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
      # there are two of ptId 32 data, so relabel one of them
      if (i == 26){
        dat$ptID <- replace(dat$ptID, dat$ptID == '32','31')
      }
      # some resp were entered with line breaks, no comma. so replacing those with commas
      dat$resp <- gsub("[\n]", ",", dat$resp)
    # append this df to the df with all data
    fullDat<-rbind(fullDat,dat)  
    
  }
  
  # set the output path
  pathOut <- paste0(dir,'/fulldata.csv')
  
  # save the output
  write.table(fullDat, pathOut, sep = ",", row.names = F)

  
#---------End-----------#
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #################################################################################
  #--------                        OLD CODES                               --------
  #################################################################################
  

#####---lapply version for the for main loop  -----
#       (for loop was more convenient to see where it stacked so removed this version. this does not deal with ptID32 & [\n] issue)
  
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
  
  
  
  
###---codes used to do "\n" troubleshooting -----
  fileList[16]
  fullDat <- data.frame(ptID = numeric(0),
                        session = character(0),
                        resp = character(0))
  tpath <- fileList[16]
  originalCSV<-read.csv(tpath)
  dat<-data.frame(originalCSV$participant, originalCSV$session, originalCSV$textbox.text)
  colnames(dat) <- c("ptID","session","resp")
  dat <- dat[dat$resp != "",]
  dat$resp <- gsub("[\n]", ",", dat$resp)
  fullDat<-rbind(fullDat,dat)
  pathOut <- as.character('/Users/yuka/Desktop/Production_Effect_Project/testfull.csv')
  write.table(fullDat, file = pathOut, sep= ",", row.names = F) 
  
  
  

#####--- the very first vesion -----
  
  
#-----prerequisites
# 1) all the data are in 1 folder
# 2) set your directory and folder name for the data on Line19 and 20


"C:\Users\yuka\iCloudDrive\@midd\Middlebury Senior\Thesis2\data_midd"

#------library

library(dplyr)


####--- data cleaning 

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





#####---codes I used to test 1 data-----

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
