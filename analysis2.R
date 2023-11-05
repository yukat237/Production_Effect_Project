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
datPath <- "/Users/yuka/Desktop/Production_Effect_Project/Scored_data_full_final.csv"
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
noMixDat<-noMixDat[,c(1,7,25,26,27)]
#this shows error: noMixDat <- noMixDat %>% rename(num_correct = Total_Num_Correct)
colnames(noMixDat)[colnames(noMixDat) == "Total_Num_Correct"] <- "num_correct"

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
#change character to numeric to prep for the next line
twoMixDat$num_correct <-twoMixDat$num_correct %>% as.numeric()
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
fourMixDat$num_correct <- fourMixDat$num_correct %>% as.numeric()
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
fourMixDat$num_correct[is.na(fourMixDat$num_correct)] <- 0
fourMixDat$hitsprop[is.na(fourMixDat$hitsprop)] <- 0

###rbind these 3 data frames--------
newfulldat<-rbind(noMixDat,twoMixDat,fourMixDat)

#this does not work, review ggplot2
#reorder to present in the preferable order
newfulldat$mix_type <- factor(newfulldat$mix_type, levels = c("nomix","2mix","4mix"))
newfulldat$prod_type <- factor(newfulldat$prod_type, levels = c("Loud","Aloud","Mouth","Silent"))

#prep for error bars (and in general plotting)a #edit on 11/5/23, summarise() not working anymore so fixed
plottingDF<-newfulldat %>%
  group_by(mix_type,prod_type)%>%
  mutate(
    seHitsProp = sd(hitsprop)/(sqrt(nrow(newfulldat)/12)),
    meanHitsProp = mean(hitsprop)
  )

#Creating plots
mainbar <- ggplot(plottingDF, aes(x=mix_type, y=meanHitsProp, fill=prod_type)) +
  ylim(0,0.6) +
  geom_bar(stat = "identity",position=position_dodge(0.6), width = 0.6)+
  theme_bw() +
  geom_errorbar(aes(ymin=meanHitsProp-seHitsProp,ymax=meanHitsProp+seHitsProp,color=prod_type),
                position=position_dodge(0.6), width = 0.2, color="grey40" )+
  labs(y="Proportion of Hits",x="List Type",fill = "Production Type")+
  scale_fill_manual(values=c("#666666","#ed8975","#B1D877","#6fa8dc"))+
  scale_x_discrete(labels=c('Pure', '2 Mix', '4 mix'))

mainbar


#hist
ggplot(newfulldat, aes(x=hitsprop, color = mix_type)) + 
         geom_histogram(fill = "#FFFFFF", alpha=0.5, position="identity") + theme_minimal()
       
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
mainanova_w_contrasts <- Anova(lm(hitsprop ~ mix_type * prod_type, data=newfulldat, contrasts=list(mix_type=contr.sum, prod_type=contr.sum)), type=3)
mainanova_default <- Anova(lm(hitsprop ~ mix_type * prod_type, data=newfulldat), type=3)

#post-hoc 
#<DISCRADING THIS BC NO ASSUMPTION OF TYPE3 ANOVA?>
      fitlm<- aov(hitsprop~mix_type*prod_type, data = newfulldat)  
      tuk<-TukeyHSD(fitlm)
      pander(tuk$`mix_type:prod_type`)
#website taught me this:https://rpubs.com/WhataBurger/Anovatype3

#making sure type 3 interaction tests
#based on:
        #https://www.datanovia.com/en/lessons/anova-in-r/#post-hoct-tests
        #and
        #https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#prep
model <- lm(hitsprop ~ mix_type * prod_type, data = newfulldat)

#pair-wise
library("emmeans")
library("rstatix")
#looking at, "for each list condition, were there between-production-type differences?"
pwc1 <- newfulldat %>% 
  group_by(mix_type) %>%
  emmeans_test(hitsprop ~ prod_type, p.adjust.method = "bonferroni") 
pwc1
# 8 2mix     prod_type hitsprop Loud   Mouth    768    2.78   5.62e- 3 3.37e- 2 *           
# 10 2mix     prod_type hitsprop Aloud  Mouth    768    3.07   2.24e- 3 1.34e- 2 *          
# 13 4mix     prod_type hitsprop Loud   Aloud    768    6.48   1.61e-10 9.65e-10 ****        
#  14 4mix     prod_type hitsprop Loud   Mouth    768    4.49   8.22e- 6 4.93e- 5 ****        
#  15 4mix     prod_type hitsprop Loud   Silent   768    7.37   4.38e-13 2.63e-12 ****        
# 18 4mix     prod_type hitsprop Mouth  Silent   768    2.88   4.07e- 3 2.44e- 2 * 

# within 2mix 
  # L-M *
  # A-M *
# within 4mix
  # L-A ****
  # L-M ****
  # L-S ****
  # M-S *


#looking at, "for each condition condition, were there between-list differences?"
pwc2 <- newfulldat %>% 
  group_by(prod_type) %>%
  emmeans_test(hitsprop ~ mix_type, p.adjust.method = "bonferroni") 
pwc2

# 2 Loud      mix_type hitsprop nomix  4mix     768    -2.55  0.0110       0.0330      *           
#  3 Loud      mix_type hitsprop 2mix   4mix     768    -2.86  0.00436      0.0131      *           
# 5 Aloud     mix_type hitsprop nomix  4mix     768     4.37  0.0000142    0.0000425   ****        
#  6 Aloud     mix_type hitsprop 2mix   4mix     768     3.91  0.0000990    0.000297    ***         
#  7 Mouth     mix_type hitsprop nomix  2mix     768     3.54  0.000419     0.00126     **         
# 10 Silent    mix_type hitsprop nomix  2mix     768     3.11  0.00195      0.00585     **          
#  11 Silent    mix_type hitsprop nomix  4mix     768     5.55  0.0000000399 0.000000120 ****        
#  12 Silent    mix_type hitsprop 2mix   4mix     768     2.44  0.0149       0.0448      *   

# within Loud
  # nomix-4mix
  # 2mix-4mix
# within Aloud
  # nomix-4mix
  # 2mix-4mix
# within Mouth
  # nomix-2mix
# within Silent
  # nomix-2mix
  # nomix-4mix
  # 2mix-4mix


#see if i get the same results
joint_tests(model, by = "mix_type")
joint_tests(model, by = "prod_type")


#pairwise tests as chatgpt told me------------
int_em <- emmeans(model,  ~ mix_type:prod_type)
paircomp1<-emmeans(int_em, pairwise ~ mix_type)
  #shows no main diff bet 2mix and 4mix. nomix-2mix and nomix-4mix are diff.
paircomp2<-emmeans(int_em, pairwise ~ prod_type)
  #L vs all the other prod_type are diff. the rest are non sig

#checking main effects directions
emmeans(model, pairwise ~ mix_type, adjust = "bonferroni") %>% summary()
emmeans(model, pairwise ~ prod_type, adjust = "bonferroni") %>% summary()

#main interest (all pairwise)
emmeans(model, pairwise ~ mix_type:prod_type, adjust = "bonferroni") %>% summary()

# nomix Loud - 4mix Loud 
# nomix Loud - 4mix Aloud 
# nomix Loud - 4mix Silent
# 2mix Loud - 4mix Aloud 
# 2mix Loud - 4mix Silent 
# 4mix Loud - 4mix Aloud  
# 4mix Loud - 2mix Mouth 
# 4mix Loud - 4mix Mouth 
# 4mix Loud - 2mix Silent
# 4mix Loud - 4mix Silent
# nomix Aloud - 4mix Aloud
# nomix Aloud - 4mix Aloud
# nomix Aloud - 2mix Mouth 
# nomix Aloud - 4mix Silent
# 2mix Aloud - 4mix Aloud 
# 2mix Aloud - 4mix Silent
# 4mix Aloud - nomix Mouth
# 4mix Aloud - nomix Silent 
# nomix Mouth - 2mix Mouth 
# nomix Mouth - 4mix Silent  
# nomix Mouth - 4mix Silent
# 2mix Mouth - nomix Silent 
# nomix Silent - 4mix Silent


#edits on 11/4 reading this: https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/#all-pairwise-comparisons
emmeans(model, pairwise ~ mix_type|prod_type, adjust = "bonferroni") %>% summary()
  #i love this bec this is doing the "separate" stats using the same emmeans() funciton, not emmeans_test
  #amazingly, Silent 2mix-4mix is p=0.0448 so i can say yes it's sig!  
  #this results say,
    # Loud obv diff, 4mix>2mix
    # Aloud: 2mix > 4mix
    # Mouth: 2mix = 4mix
    # Silent: 2mix > 4mix


#this does Tukey as a default, FYI. this is not appropriate for repeated measures and unbalanced? if im right
emmeans(model, pairwise ~ mix_type*prod_type) %>% summary()







      ### OLD ####=-=========================
      

# get full data from scoring sheet we completed on Google drive (Scored_data_full) ----------------------------------------
#"C:\Users\yzt5262\Documents\Rprojects\Production_Effect_Project\EXAMPLE_Scored_data_full.xlsx"

dir <- "/Users/yzt5262/Documents/Rprojects/Production_Effect_Project/" #The only place to change when using this script in a diff desktop
datfilename<-"EXAMPLE_Scored_data_full.csv" #change when working on real data! (vs fake data)
datfilepath<- paste0(dir,datfilename)

#this file's word incorrect is only ok by ptID40. also order measres are messed up
scoredDF<-read.csv(datfilepath, header = F)
scoredDF<-scoredDF[1:456,]

#