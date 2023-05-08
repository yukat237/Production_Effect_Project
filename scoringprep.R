### to prep for the scoring process ###

## 1) obtain condition info------------------------------------------------------------
dir <- "/Users/yuka/Desktop/Production_Effect_Project/"
tgfilename<-"cond_info.csv"
tgfilepath<- paste0(dir,tgfilename)

condinfoDF<-read.csv(tgfilepath, header = F)
condinfoDF<-condinfoDF[1:60,]

flipDf<- t(condinfoDF) %>% as.data.frame()

write_csv(flipDf, paste0(dir,"cond_flipped.csv"))


## 2) process each wordlist to have actual words------------------------------------------------------------

dirWL <- "/Users/yuka/iCloudDrive/@midd/Middlebury Senior/Thesis2/wordlist"

# read in the realword table (word image file name & actual word stim comparison table)
wordinfo <- read.xlsx("/Users/yuka/iCloudDrive/@midd/Middlebury Senior/Thesis2/wordlist_with_real_words/stimuli list for scoring.xlsx")

#making a file name list
setwd(dirWL)
fileListWL <- list.files(dirWL)
fileNum <- length(fileListWL)

# prepare a dataframe for the full data
fullDatWL <- data.frame(listname = character(0),
                      filenum = character(0),
                      realword = character(0),
                      sayStim = character(0))

#read each file
for (i in 1:fileNum){
  
  # import data
  dat<-read.xlsx(fileListWL[i])
  
  # take only the numbers from this column
  dat$wordStim <- str_extract(dat$wordStim, pattern = "w[:digit:]*")
  dat$wordStim <- str_replace_all(dat$wordStim, "w", "")
  dat <- dat %>% rename( "filenum" = "wordStim" )
  
  # add another column based on the wordinfo df
  dat_w_realwords <- merge(dat, wordinfo, by = "filenum", sort = F)
  
  # make a new column name to represent listname (= filename)
  dat_w_realwords$listname <- rep(fileListWL[i], times = 30)
  dat_w_realwords$listname <- gsub(".xlsx", "", dat_w_realwords$listname)
  dat_w_realwords <- dat_w_realwords[,c(4,1,2,3)]
  
  # append this to full df
  # "C:\Users\yuka\iCloudDrive\@midd\Middlebury Senior\Thesis2\wordlist_with_real_words"
  fullDatWL<-rbind(fullDatWL,dat_w_realwords)
  
}

# set the output path
pathOut <- "/Users/yuka/iCloudDrive/@midd/Middlebury Senior/Thesis2/wordlist_with_real_words/realwords.csv"

# save the output
write.table(fullDatWL, pathOut,  sep = ",", row.names = F)

