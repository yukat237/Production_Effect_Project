### to prep for the scoring process ###

## obtain condition info
dir <- "/Users/yuka/Desktop/Production_Effect_Project/"
tgfilename<-"cond_info.csv"
tgfilepath<- paste0(dir,tgfilename)

condinfoDF<-read.csv(tgfilepath, header = F)
condinfoDF<-condinfoDF[1:60,]

flipDf<- t(condinfoDF) %>% as.data.frame()

write_csv(flipDf, paste0(dir,"cond_flipped.csv"))


## process each wordlist to have actual words
# "C:\Users\yuka\iCloudDrive\@midd\Middlebury Senior\Thesis2\wordlist"

dirWL <- "/Users/yuka/iCloudDrive/@midd/Middlebury Senior/Thesis2/wordlist"

#making a file name list
setwd(dirWL)
fileListWL <- list.files(dirWL)
fileNum <- length(fileListWL)

#read each file
for (i in 1:fileNum){
  
  # import data
  originalWL<-read.csv(fileListWL[i])
  # leave only needed columns
  dat<-data.frame(originalCSV$participant, originalCSV$session, originalCSV$textbox.text)
  # rename columns
  colnames(dat) <- c("ptID","session","resp")
  # remove rows that are empty in resp column
  dat <- dat[dat$resp != "",]
  # write out this info
  # "C:\Users\yuka\iCloudDrive\@midd\Middlebury Senior\Thesis2\wordlist_with_real_words"
  fullDat<-rbind(fullDat,dat)  
  
}

