####------------------------####
####  Thesis2 Data Analysis ####  
####----------------------- ####

#-----prerequisites-----
# 1) all the data are in 1 folder
# 2) set your directory and folder name for the data on Line19 and 20


#------library------

library(dplyr)


####--- data cleaning ----####

# set your directory and folder name that contains all data you're processing 
# [need to modify these for your use!!] 
dir <-setwd("/Users/yuka/Desktop/Production_Effect_Project")
folderName <- "data"

#making a file name list
fileList <- list.files(paste0(dir, "/", folderName))
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
  pathOut <- paste0(dir, '/', numeric(dat$ptID))
  
  # save the output
  write.csv(dat, pathOut)
  
})