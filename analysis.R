####----------------------####
#### Thesis Data Analysis ####
####--------------------- ####

######---   workD and librareis    -----####

library("plyr")
library("dplyr")
library("mosaic")
library("lme4")
library("ggpubr")
library("ggplot2")
library("stringr")
library("data.table")
library("tidyr")
library("readxl")
library("rstatix")
setwd("C:/Users/tatsu/iCloudDrive/@midd/Middlebury Senior/Thesis/")
options(max.print=1000000)

#####------ read files --------#####


RAWdata<-read_excel("ScoringSheet (1).xlsx")
RAWdata<-as.data.frame(RAWdata)#from list to dataframe
RAWdata2<-RAWdata[,c(1:16)]#omit comment and the final empty column
head(RAWdata2)
class(RAWdata2)
ncol(RAWdata2)
colnames(RAWdata2)


#####------ Analysis --------#####


#?܂?ANOVA of proportion correct 
# results
#To analyze the production effect, 
#the proportion of correct recall will be measured and a 3 (list type: random-mixed, predictable-mixed, pure) 
#by 2 (production: read aloud, read silently) repeated measure ANOVA will be conducted.
#The ideal means of the proportion of correct recall is shown in Figure 1.
#As the visual inspection will reveal, it is expected that 
#both mixed lists show the significant difference between silent words and aloud words (random: F(??, ??)= ??, p <.05, predicted; F(??, ??)= ??, p <.05), indic
#indicatting greater recall for words read aloud than those read silently.
#On the other hand, the pure-list condition will be not significant (F(??, ??)= ??, p =    ) (Hypothesis 1).
#Also, the recall of aloud items in the pure lists is expected to be less than those in the predictable-mixed list (t(??) ??, p <.05, d = ?? ), and those in the random-mixed lists is also expected to be less than those in the predictable-mixed list (t(??) ??, p <.05, d = ?? ) (Hypothesis 2). Furthermore, the recall of silent items in the pure lists are expected to be the largest (t(??) ??, p <.05, d = ?? ), less in the predictable-mixed condition (t(??) ??, p <.05, d = ?? ), and the least in the random-mixed condition (t(??) ??, p <.05, d = ?? ) (Hypothesis 3). 
#?Ă??Ƃ͂܂????l?ЂƂ???proportion???v?Z????column???K?v
#???ɂ???column?́AA???��???S???��?????column?Alist type??column?B????ID
#????mix?̂???a??s?ŕʃ??C???ɕ????Ȃ???...
#???ƃ??X?g?^?C?v??pure?ɂ܂Ƃ߂Ȃ???...

###Make S&A into 1 line
simplerDF<-RAWdata2[,c(1,5,9,11)]#extract columns only needed
onlySDF<-simplerDF[grep("S",simplerDF$List_Type),]
onlyADF<-simplerDF[grep("A",simplerDF$List_Type),]
onlySandADF<-merge(onlySDF,onlyADF,by = c("ID"))
onlySandADF<-onlySandADF[,c(1,4,6)]
setnames(onlySandADF, "Silent_Num_Correct.x", "Silent_Num_Correct")
setnames(onlySandADF, "Aloud_Num_Correct.y", "Aloud_Num_Correct")
onlySandADF$List_Type<-rep("Pure",58)
#--Add hits porportion  (S = 16, A = 14)
onlySandADF$Aloud_Num_Correct<-as.numeric(onlySandADF$Aloud_Num_Correct)
onlySandADF$Silent_Num_Correct<-as.numeric(onlySandADF$Silent_Num_Correct)
onlySandADF$Aloud_HitsProp<-onlySandADF$Aloud_Num_Correct/30
onlySandADF$Silent_HitsProp<-onlySandADF$Silent_Num_Correct/30


###combine DF above to Mixed lists DF...
#--first, make a mixed only DF w/only needed columns
simplerDF<-RAWdata2[,c(1,5,9,11)]#extract columns only needed
onlyRDF<-simplerDF[grep("R",simplerDF$List_Type),]
onlyPDF<-simplerDF[grep("P",simplerDF$List_Type),]
onlyMixedDF<-rbind(onlyRDF,onlyPDF)
#---Add hits porportion  (S = 16, A = 14)
onlyMixedDF$Aloud_Num_Correct<-as.numeric(onlyMixedDF$Aloud_Num_Correct)
onlyMixedDF$Silent_Num_Correct<-as.numeric(onlyMixedDF$Silent_Num_Correct)
onlyMixedDF$Aloud_HitsProp<-onlyMixedDF$Aloud_Num_Correct/14
onlyMixedDF$Silent_HitsProp<-onlyMixedDF$Silent_Num_Correct/16

#---Next, make a combined DF (list type * a and s)
finalDF<-rbind(onlyMixedDF,onlySandADF)
finalDF<-finalDF[order(finalDF$ID),]

#---Finally, transfrom to do anova
finalDF<-finalDF[,c(1,2,5,6)] #omit raw score data
finalDF<-melt(data= finalDF, 
              id.vars = c("ID","List_Type"),
              variable.name = "AorS",
              value.name = "HitsProp"
)
finalDF<-finalDF[order(finalDF$ID),]


##Descriptive analysis-------------------------

###Check ggplot
#part1 --box plot
bxpHITS <- ggboxplot(
  finalDF, x = "List_Type", y = "HitsProp",
  color = "AorS", palette = "jco")

#part2 --bar graph
#data prep for standard error bars
dfforgraph<-finalDF %>%
  group_by(List_Type,AorS)%>%
  summarise(
    seHitsProp = sd(HitsProp)/(sqrt(nrow(finalDF)/3)),
    meanHitsProp = mean(HitsProp)
  )
dfforgraph<-as.data.frame(dfforgraph)
#data prep for ordering bars
dfforgraph$AorS<-as.character(dfforgraph$AorS)
dfforgraph$AorS[dfforgraph$AorS == "Silent_HitsProp"] <- "Silent"
dfforgraph$AorS[dfforgraph$AorS == "Aloud_HitsProp"] <- "Aloud"
dfforgraph$List_Type[dfforgraph$List_Type == "R"] <- "Random Mix"
dfforgraph$List_Type[dfforgraph$List_Type == "P"] <- "Predictable Mix"
dfforgraph$AorS<-as.factor(dfforgraph$AorS)
dfforgraph$List_Type<-as.factor(dfforgraph$List_Type)
dfforgraph<-transform(dfforgraph,List_Type=factor(List_Type, levels = c("Predictable Mix","Random Mix","Pure")) )
names(dfforgraph)[names(dfforgraph) == "AorS"] <- "Production"

#visualization
barHITS <- ggplot(
  data=dfforgraph, aes(x = List_Type, y = meanHitsProp,
                       fill= Production))+ geom_bar(stat="identity", width = 0.6,position=position_dodge())+
  ylim(0,0.6)+theme_bw()+labs(y="Proportion of Hits",x="List Type")+
  geom_errorbar(aes(ymin=meanHitsProp-seHitsProp,ymax=meanHitsProp+seHitsProp,color=Production),
                position=position_dodge(0.6), width = 0.2, colour="grey40" )
barHITS

#another ver.
dfforgraph
barplot(dfforgraph)


###MAIN ANALYSIS-----------------------

#---outliers
finalDF %>%
  group_by(List_Type,AorS) %>%
  identify_outliers(HitsProp)
###=====there were some extreme outliers!!!!=====###

#---Normality Assumption

finalDF %>%
  group_by(List_Type,AorS) %>%
  shapiro_test(HitsProp)

###==== score was normally distributed at each time point (p > 0.05), EXCEPT FOR PURE-A and PRED-S

#---QQplot

ggqqplot(finalDF, "HitsProp", ggtheme = theme_bw()) +
  facet_grid(List_Type ~ AorS, labeller = "label_both")

###=====all the points fall approximately along the reference line,

#--computation

res.aov <- anova_test(
  data = finalDF, dv = HitsProp, wid = ID,
  within = c(List_Type, AorS)
)
get_anova_table(res.aov)

#another ANOVA for MSE(mean squared error)
library("ez")
anova2<-ezANOVA(data = finalDF,
                dv = HitsProp,
                wid = ID,
                within = .(List_Type,AorS),
                detailed = TRUE)
#adding MSE column
anova2$ANOVA$MSE = anova2$ANOVA$SSd/anova2$ANOVA$DFd


###=====Only Main Effects of AvsS is significant.




#--Post Hoc

#-effect of A or S for each Listtype
one.way <- finalDF %>%
  group_by(List_Type) %>%
  anova_test(dv = HitsProp, wid = ID, within = AorS) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

pwc <- finalDF %>%
  group_by(List_Type) %>%
  pairwise_t_test(
    HitsProp ~ AorS, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

###===A was significantly scored higher than S for Pred list and Random list but NON-SIG for Pure List===###

#-checking the effect of List type 
one.way2 <- finalDF %>%
  group_by(AorS) %>%
  anova_test(dv = HitsProp, wid = ID, within = List_Type) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

pwc2 <- finalDF %>%
  group_by(AorS) %>%
  pairwise_t_test(
    HitsProp ~ List_Type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )


###===Results SUMMARY===###

#         Pred A -- Pure A
#         Rand A -- Pure A
#         Pred A -- Rand A
#                           --> ALL NON-SIG!!!

#         Pred S -- Pure S
#         Rand S -- Pure S
#                           --> BOTH SIG!!! 
#         Pred S -- Rand S
#                           --> NON-SIG!!!

#                                 ==>  (Pred S ???@RandS)  <  Pure S


##Comments --Maybe I shoud try taking out outliers? but just by looking at the graphs and where the outliers are, does not seem it would change the results



#####---------- Intrusion rates ------------#####
#analyzed by one-way ANOVA, 
#which are not expected to be significantly different (?? vs ?? vs ?? vs ??), (F(??, ??)= ??, MSE = ??, p = )
#consittant with the past literature (Forrin and MacLeod 2016; Lambert et al., 2016).

######ANALYSIS : raw counts#########

### make a DF------- 
IntruDF<-RAWdata2[,c(1,5,13,14)]#extract columns only needed (ID, List type, totalanswered, numIncorrectOtherlists, NumIncorrectTrueIntrusion)
IntruDF$IntruCount<- IntruDF$Num_Incorrect_from_otherlists+IntruDF$Num_Incorrect_Intrusion
IntruDF<-IntruDF[,c(1,2,5)]


### Analysis -------

#--visualization--
IntruBxp <- ggboxplot(IntruDF, x = "List_Type", y = "IntruCount", add = "point")
IntruBxp

ggqqplot(IntruDF, "IntruCount", facet.by = "List_Type") #checking assumption
##==some outliars==##
#...=> individual?

#--Computation (inferential)--
IntruANOVA <- anova_test(data = IntruDF, dv = IntruCount, wid = ID, within = List_Type)
get_anova_table(IntruANOVA)
##===NOT SIGNIFICANT!!==##

#--pairwise --
IntruPwc <- IntruDF %>%
  pairwise_t_test(
    IntruCount ~ List_Type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
IntruPwc
##===NOT SIGNIFICANT!!==##

#--Computation,BUT with Correction--
get_anova_table(IntruANOVA,correction = "auto")
#If run "IntruANOVA", u see the whole calculation. 


#######??????RATES?ł??��?????########
### make a DF------- 
IntruDF<-RAWdata2[,c(1,5,7,13,14)]#extract columns only needed (ID, List type, totalanswered, numIncorrectOtherlists, NumIncorrectTrueIntrusion)
IntruDF$IntruRate<- (IntruDF$Num_Incorrect_from_otherlists+IntruDF$Num_Incorrect_Intrusion) /IntruDF$Total_Num_Answered
IntruDF<-IntruDF[,c(1,2,6)]


### Analysis -------

#--visualization--
IntruBxp <- ggboxplot(IntruDF, x = "List_Type", y = "IntruRate", add = "point")
IntruBxp

ggqqplot(IntruDF, "IntruRate", facet.by = "List_Type") #checking assumption
##==some outliars==##
#...=> individual?

#--Computation (inferential)--
IntruANOVA <- anova_test(data = IntruDF, dv = IntruRate, wid = ID, within = List_Type)
get_anova_table(IntruANOVA)
##===NOT SIGNIFICANT!!==##

#--pairwise --
IntruPwc <- IntruDF %>%
  pairwise_t_test(
    IntruRate ~ List_Type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
IntruPwc
##===NOT SIGNIFICANT!!==##



##----------------Order-Measures-------------------
RAWdata3<-RAWdata[,c(1,5, 18:20)]#omit comment and the final empty column

#--Computation (descriptive)--
IOsummary<-RAWdata3 %>%
  group_by(List_Type)%>%
  summarise(
    seIO = sd(IO_Measure)/(sqrt(nrow(RAWdata3)/4)),
    meanIO = mean(IO_Measure)
  )
IOsummary<-as.data.frame(IOsummary)

BIsummary<-RAWdata3 %>%
  group_by(List_Type)%>%
  summarise(
    seBi = sd(Bi_Measure)/(sqrt(nrow(RAWdata3)/4)),
    meanBi = mean(Bi_Measure)
  )
BIsummary<-as.data.frame(BIsummary)

ADsummary<-RAWdata3 %>%
  group_by(List_Type)%>%
  summarise(
    seBi = sd(AD_Measure)/(sqrt(nrow(RAWdata3)/4)),
    meanBi = mean(AD_Measure)
  )
ADsummary<-as.data.frame(ADsummary)

#--Computation (inferential)--

# IO measure
IOIntruANOVA <- anova_test(data = RAWdata3, dv = IO_Measure, wid = ID, within = List_Type)
get_anova_table(IOIntruANOVA)
#####t.test(RAWdata3$IO_Measure~RAWdata3$List_Type, mu = 0.5,paired=TRUE)

# Bi Measure
BiIntruANOVA <- anova_test(data = RAWdata3, dv = Bi_Measure, wid = ID, within = List_Type)
get_anova_table(BiIntruANOVA)

# AD Measure 
ADIntruANOVA <- anova_test(data = RAWdata3, dv = AD_Measure, wid = ID, within = List_Type)
get_anova_table(ADIntruANOVA)

###=========ALL NOT SIGNIFICANT=========####









############################################TRASH#############################################
###Only PureS and PureA Intru data
onlySIntruDF<-IntruDF[grep("S",IntruDF$List_Type),]
onlyAIntruDF<-IntruDF[grep("A",IntruDF$List_Type),]
onlySandAIntruDF<-merge(onlySIntruDF,onlyAIntruDF,by = c("ID"))
onlySandAIntruDF<-onlySandAIntruDF[,c(1,3,5)]
setnames(onlySandAIntruDF, "IntruRate.x", "Silent_Intru_Rate")
setnames(onlySandAIntruDF, "IntruRate.y", "Aloud_Intru_Rate")
onlySandAIntruDF$List_Type<-rep("Pure",58)
head(onlySandAIntruDF)
###Only Mixed Intru data
onlyMixedIntruDF<-RAWdata2[,c(1,5,7,13,14)]#extract columns only needed
onlyRIntruDF<-onlyMixedIntruDF[grep("R",onlyMixedIntruDF$List_Type),]
onlyPIntruDF<-onlyMixedIntruDF[grep("P",onlyMixedIntruDF$List_Type),]
onlyMixedIntruDF<-rbind(onlyRIntruDF,onlyPIntruDF)
#---Add Intru Rate Column 
onlyMixedIntruDF$IntruRate<- (onlyMixedIntruDF$Num_Incorrect_from_otherlists+onlyMixedIntruDF$Num_Incorrect_Intrusion) /onlyMixedIntruDF$Total_Num_Answered
onlyMixedIntruDF<-onlyMixedIntruDF[,c(1,2,6)] #omit unneeded columns
head(onlyMixedIntruDF)
#---Next, make a combined DF (list type * a and s)
finalIntruDF<-rbind(onlySandAIntruDF,onlyMixedIntruDF)
finalIntruDF<-finalDF[order(finalDF$ID),]
#-------------

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
finalDF$HitsProp<-as.factor(finalDF$HitsProp)
finalDF2<-data_summary(finalDF,varname = "HitsProp", groupnames=c("List_Type","AorS"))

#=------------------------

#data prep for ordering bars
dfforgraph<-finalDF
dfforgraph$AorS<-as.character(dfforgraph$AorS)
dfforgraph$AorS[dfforgraph$AorS == "Silent_HitsProp"] <- "Silent"
dfforgraph$AorS[dfforgraph$AorS == "Aloud_HitsProp"] <- "Aloud"
dfforgraph$List_Type[dfforgraph$List_Type == "R"] <- "Random Mix"
dfforgraph$List_Type[dfforgraph$List_Type == "P"] <- "Predictable Mix"
dfforgraph$AorS<-as.factor(dfforgraph$AorS)
dfforgraph$List_Type<-as.factor(dfforgraph$List_Type)
dfforgraph<-transform(dfforgraph,List_Type=factor(List_Type, levels = c("Predictable Mix","Random Mix","Pure")) )

#S and A
orderonlySDF<-RAWdata3[grep("S",RAWdata3$List_Type),]
orderonlyADF<-RAWdata3[grep("A",RAWdata3$List_Type),]
orderonlySandADF<-merge(orderonlySDF,orderonlyADF,by = c("ID"))
orderonlySandADF<-orderonlySandADF[,c(1,3,4,5,7,8,9)]
setnames(orderonlySandADF, "IO_Measure.x", "S_IO")
setnames(orderonlySandADF, "IO_Measure.y", "A_IO")
setnames(orderonlySandADF, "Bi_Measure.x", "S_Bi")
setnames(orderonlySandADF, "Bi_Measure.y", "A_Bi")
setnames(orderonlySandADF, "AD_Measure.x", "S_AD")
setnames(orderonlySandADF, "AD_Measure.y", "A_AD")
orderonlySandADF$List_Type<-rep("Pure",58)  


#-------------------------


dfforgraph<-MeanSummary
dfforgraph$AorS<-as.character(dfforgraph$AorS)
dfforgraph$AorS[dfforgraph$AorS == "Silent_HitsProp"] <- "Silent"
dfforgraph$AorS[dfforgraph$AorS == "Aloud_HitsProp"] <- "Aloud"
dfforgraph$List_Type[dfforgraph$List_Type == "R"] <- "Random Mix"
dfforgraph$List_Type[dfforgraph$List_Type == "P"] <- "Predictable Mix"
dfforgraph$AorS<-as.factor(dfforgraph$AorS)
dfforgraph$List_Type<-as.factor(dfforgraph$List_Type)
dfforgraph<-transform(dfforgraph,List_Type=factor(List_Type, levels = c("Predictable Mix","Random Mix","Pure")) )
names(dfforgraph)[names(dfforgraph) == "AorS"] <- "Production"
#
#--means
MeanSummary<-finalDF %>%
  group_by(List_Type,AorS)%>%
  summarise_all("mean")

##results
#List_Type AorS               ID     HitsProp
#<chr>     <fct>              <dbl>   <dbl>
#1 P         Aloud_HitsProp   29.5    0.374
#2 P         Silent_HitsProp  29.5    0.325
#3 Pure      Aloud_HitsProp   29.5    0.375
#4 Pure      Silent_HitsProp  29.5    0.377
#5 R         Aloud_HitsProp   29.5    0.366
#6 R         Silent_HitsProp  29.5    0.315

MeanSummary<-as.data.frame(MeanSummary)
MeanSummary<-MeanSummary[,c(1,2,4)]



#----------------------------



#--calculate MSE 
# Main effect of list type
dfforListMse<-finalDF %>%
  group_by(List_Type)%>%
  summarise(
    sdHitsProp = sd(HitsProp),
    meanHitsProp = mean(HitsProp)
  )
dfforListMse<-as.data.frame(dfforListMse)

####===results
#  List_Type sdHitsProp meanHitsProp
#1         P  0.1387258    0.3499076
#2      Pure  0.1423989    0.3755747
#3         R  0.1539213    0.3396706

# Main effect of Production
dfforAorSMse<-finalDF %>%
  group_by(AorS)%>%
  summarise(
    sdHitsProp = sd(HitsProp),
    meanHitsProp = mean(HitsProp)
  )
dfforAorSMse<-as.data.frame(dfforAorSMse)

####===results
#   AorS       sdHitsProp   meanHitsProp
#1  Aloud      0.1413039    0.3716201
#2 Silent      0.1481912    0.3384818
