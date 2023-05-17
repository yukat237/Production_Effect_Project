####------------------------####
####  Thesis2 Data analysis #### 
####----------------------- ####


# get full data from scoring sheet we completed on Google drive (Scored_data_full) ----------------------------------------
#"C:\Users\yzt5262\Documents\Rprojects\Production_Effect_Project\EXAMPLE_Scored_data_full.xlsx"

dir <- "/Users/yzt5262/Documents/Rprojects/Production_Effect_Project/" #The only place to change when using this script in a diff desktop
datfilename<-"EXAMPLE_Scored_data_full.csv" #change when working on real data! (vs fake data)
datfilepath<- paste0(dir,datfilename)

#this file's word incorrect is only ok by ptID40. also order measres are messed up
scoredDF<-read.csv(datfilepath, header = F)
scoredDF<-scoredDF[1:456,]

#