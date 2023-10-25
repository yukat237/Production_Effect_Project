####------------------------####
####  Thesis2 Data analysis #### 
####----------------------- ####

###-------###

library("stringi")
library("dplyr")
library("data.table")
library("ggplot2")
library("car")
###-------###

      
#Import data
  #working on my laptop
datPath <- "/Users/yuka/Desktop/Production_Effect_Project/Scored_data_full.csv"
scoredDf<-read.csv(datPath, header = T)

#Analysis plan
# 3 list types (no mix, 2mix, 4mix) * 4 production types (silent, aloud, Mouth, Loud)
# transform the df into: 
  # Columns: ID, mix_type, prod_type, hitProp (num correct/num listed)
# no mix -- divide by 30, 2mix -- divide by 15, 4mix -- 9M, 7L, 7S, 7A

# To get the hitProp easily, divide this into 3 by the list types

#1) no mix --------------
noMixDat <- scoredDf[grep("all[SAML]",scoredDf$List_Type),]
#assign mix type "nomix"
rownum <- nrow(noMixDat)
noMixDat$mix_type <- rep("nomix",rownum)
#assign production type based on the list
noMixDat$prod_type<- stri_sub(noMixDat$List_Type,-1,-1)
#add hitprop
noMixDat$hitsprop<-noMixDat$Total_Num_Correct/30
#leave only necessary columns and clean to prep for rbind
noMixDat<-noMixDat[,c(1,7,24,25,26)]
noMixDat <- noMixDat %>% 
  rename("num_correct" = "Total_Num_Correct")
# rename values
noMixDat$prod_type<-as.character(noMixDat$prod_type)
noMixDat[noMixDat == "A"] <- "Aloud"
noMixDat[noMixDat == "S"] <- "Silent"
noMixDat[noMixDat == "M"] <- "Mouth"
noMixDat[noMixDat == "L"] <- "Loud"
  
#2) 2 mix --------------
twoMixDat <- scoredDf[grep("[A-Z]{2}",scoredDf$List_Type),]
#melt into 2 rows based on the production type 
  #(i.e.,make a new df where, for each current row, get 2 rows for each prod type)
  #columns: ID, List_type, mix_type, prod_type (S/A/M/L) , rawhits
#to use this I need to omit unneeded data.
twoMixDat <- twoMixDat[,c(1,4,8,10,12,14)]
twoMixDat <- melt(data=as.data.table(twoMixDat),
                  id.vars = c("N", "List_Type"),
                  variable.name = "prod_type",
                  value.name = "num_correct")
#NA columns are not needed so delete.
twoMixDat <- twoMixDat[grep("[0-9]{1,2}",twoMixDat$num_correct),]
#assign mix type "2mix"
rownum <- nrow(twoMixDat)
twoMixDat$mix_type <- rep("2mix",rownum)
#add hitsprop
twoMixDat$hitsprop <- twoMixDat$num_correct/15
#clean colnames/values to prep for rbind
twoMixDat<-twoMixDat[,-c(2)]
#change prodtype into one character (S A M L)
twoMixDat$prod_type<-as.character(twoMixDat$prod_type)
twoMixDat[twoMixDat == "Aloud_Num_Correct"] <- "Aloud"
twoMixDat[twoMixDat == "Silent_Num_Correct"] <- "Silent"
twoMixDat[twoMixDat == "Mouth_Num_Correct"] <- "Mouth"
twoMixDat[twoMixDat == "Loud_Num_Correct"] <- "Loud"

#3) 4 mix --------------
fourMixDat <- scoredDf[grep("4mix",scoredDf$List_Type),]
#melt into 4 rows based on the production type 
fourMixDat <- fourMixDat[,c(1,4,8,10,12,14)]
fourMixDat <- melt(data=as.data.table(fourMixDat),
                  id.vars = c("N", "List_Type"),
                  variable.name = "prod_type",
                  value.name = "num_correct")
#assign mix type "4mix"
rownum <- nrow(twoMixDat)
fourMixDat$mix_type <- rep("4mix",rownum)
#add hitsprop based on its production type
fourMixDat<-fourMixDat %>% mutate( hitsprop = case_when(
  prod_type == "Mouth_Num_Correct" ~ num_correct/9,
  prod_type != "Mouth_Num_Correct" ~ num_correct/7
  ))
#clean colnames/values to prep for rbind
fourMixDat<-fourMixDat[,-c(2)]
#change prodtype into one character (S A M L)
fourMixDat$prod_type<-as.character(fourMixDat$prod_type)
fourMixDat[fourMixDat == "Aloud_Num_Correct"] <- "Aloud"
fourMixDat[fourMixDat == "Silent_Num_Correct"] <- "Silent"
fourMixDat[fourMixDat == "Mouth_Num_Correct"] <- "Mouth"
fourMixDat[fourMixDat == "Loud_Num_Correct"] <- "Loud"


###rbind these 3 data frames--------
newfulldat<-rbind(noMixDat,twoMixDat,fourMixDat)

#this does not work, review ggplot2
#reorder to present in the preferable order
newfulldat$mix_type <- factor(newfulldat$mix_type, levels = c("nomix","2mix","4mix"))
newfulldat$prod_type <- factor(newfulldat$prod_type, levels = c("Silent","Aloud","Mouth","Loud"))

mainbar <- ggplot(newfulldat, aes(x=mix_type, y=hitsprop, fill=prod_type)) +
  ylim(0,0.7) +
  geom_bar(stat = "identity",position=position_dodge(0.7), width = 0.6)+
  theme_bw() +
  labs(y="Proportion of Hits",x="List Type")+
  scale_fill_manual(values=c("#6fa8dc", "#ed8975", "#f886a8","#666666"))
mainbar
#i need something like this too:
#+geom_errorbar(aes(ymin=meanHitsProp-seHitsProp,ymax=meanHitsProp+seHitsProp,color=Production),
#                position=position_dodge(0.6), width = 0.2, colour="grey40" )
#also change x axis factor label to Pure, 2 mix, 4 mix

#this is what I did in analysis 1 *****NOT RUN*****----------------
dfforgraph<-finalDF %>%
  group_by(List_Type,AorS)%>%
  summarise(
    seHitsProp = sd(HitsProp)/(sqrt(nrow(finalDF)/3)),
    meanHitsProp = mean(HitsProp)
  )
dfforgraph<-as.data.frame(dfforgraph)
#and THEN PLOT
barHITS <- ggplot(
  data=dfforgraph, aes(x = List_Type, y = meanHitsProp,
                       fill= Production))+ geom_bar(stat="identity", width = 0.6,position=position_dodge())+
  ylim(0,0.6)+theme_bw()+labs(y="Proportion of Hits",x="List Type")+
  geom_errorbar(aes(ymin=meanHitsProp-seHitsProp,ymax=meanHitsProp+seHitsProp,color=Production),
                position=position_dodge(0.6), width = 0.2, colour="grey40" )
#=========not run end================================================



### inferential stats ----------------

#I have unbalanced data and interaction is expected so type III? (using car package Anova)
Anova(lm(hitsprop ~ mix_type * prod_type, data=newfulldat, contrasts=list(mix_type=contr.sum, prod_type=contr.sum)), type=3)

#post-hoc 
fitlm<- aov(hitsprop~mix_type*prod_type, data = newfulldat)  
tuk<-TukeyHSD(fitlm)
#good website for this:https://rpubs.com/WhataBurger/Anovatype3


      
      ### OLD ####
      

# get full data from scoring sheet we completed on Google drive (Scored_data_full) ----------------------------------------
#"C:\Users\yzt5262\Documents\Rprojects\Production_Effect_Project\EXAMPLE_Scored_data_full.xlsx"

dir <- "/Users/yzt5262/Documents/Rprojects/Production_Effect_Project/" #The only place to change when using this script in a diff desktop
datfilename<-"EXAMPLE_Scored_data_full.csv" #change when working on real data! (vs fake data)
datfilepath<- paste0(dir,datfilename)

#this file's word incorrect is only ok by ptID40. also order measres are messed up
scoredDF<-read.csv(datfilepath, header = F)
scoredDF<-scoredDF[1:456,]

#