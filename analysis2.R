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
dir <- "/Users/yuka/Desktop/Production_Effect_Project"
folderName <- "data"

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


#4/29/23 -- \
# this script works, but if the output probably will have an empty line. if all files are like this, need to add another line to erase that row.
# also this script saves the data separately as csv, but i could also jsut concatenate all data in 1 file, so that RAs can work more efficiently?
# --> if doing that, instead of the lines to save output as csv, i can write append line to a dataframe that was prepared before this lapply loop.

