########################
## Evaluation pretest ##
########################

#Set working directory 
setwd("~/Desktop/Psychologie Master/Masterarbeit /Pretest/Auswertung_R")

##############
## Packages ##
##############

#Load packages
if(!require(psych)){install.packages("psych")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(plyr)){install.packages("plyr")}
if(!require(car)){install.packages("car")}
if(!require(haven)){install.packages("haven")}
if(!require(BayesFactor)){install.packages("BayesFactor")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(effsize)){install.packages("effsize")}
if(!require(janitor)){install.packages("janitor")}

#Activate packages
library(psych)
library(tidyverse)
library(plyr) 
library(car) 
library(BayesFactor)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(effsize)
library(janitor)

#Read data set/ Data import
pt <- haven::read_sav("Pretest.sav")
View(pt)
  

####################
##Data preparation##
####################

#Rename
pt<- plyr::rename(pt, c("GW01_01" = "gw01", 
                        "GW02_01" = "gw02", 
                        "GW03_01" = "gw03",
                        "GW04_01" = "gw04", 
                        "GW05_01" = "gw05",
                        "GW06_01" = "gw06", 
                        "GW07_01" = "gw07", 
                        "GW08_01" = "gw08",
                        "GW09_01" = "gw09",
                        "GW10_01" = "gw10",
                        "GW11_01" = "gw11",
                        "GW12_01" = "gw12",
                        "WI01_01" = "wi01",
                        "WI02_01" = "wi02",
                        "WI03_01" = "wi03",
                        "WI04_01" = "wi04",
                        "WI05_01" = "wi05",
                        "WI06_01" = "wi06",
                        "WI07_01" = "wi07", 
                        "WI08_01" = "wi08",
                        "WI09_01" = "wi09",
                        "WI10_01" = "wi10",
                        "WI11_01" = "wi11",
                        "WI12_01" = "wi12",
                        "IN01_01" = "in01",
                        "IN02_01" = "in02",
                        "IN03_01" = "in03",
                        "IN04_01" = "in04", 
                        "IN05_01" = "in05",
                        "IN06_01" = "in06",
                        "IN07_01" = "in07", 
                        "IN08_01" = "in08",
                        "IN09_01" = "in09",
                        "IN10_01" = "in10", 
                        "IN11_01" = "in11",
                        "IN12_01" = "in12", 
                        "CM01_01" = "cm01", 
                        "CM02_02" = "cm02", 
                        "CM03_03" = "cm03",
                        "CM04_04" = "cm04",
                        "CO01_01" = "co01",
                        "CO02_02" = "co02",
                        "CO03_03" = "co03",
                        "CO04_04" = "co04",
                        "AC01_01" = "ac01",
                        "AC02_02" = "ac02",
                        "AC03_03" = "ac03",
                        "AC04_04" = "ac04", 
                        "AC05_05" = "ac05",
                        "IP01_01" = "ip01",
                        "IP02_02" = "ip02",
                        "IP03_03" = "ip03"))

names(pt)


#Missings: No important missings 
anyNA(pt)
is.na(pt)

#Testing exclusion criteria: No exclusion based on questionnaire completion time 
psych::describe(pt$TIME_SUM)
IQR(pt$TIME_SUM)

#Adapt values 
dplyr::mutate 

##########################
##Subject characteristic##
##########################

#Gender
pt$DF02 <- factor(pt$DF02, levels = c(1, 2, 3), labels = c("female", "male", "diverse"))
table(pt$DF02)

#Age
psych::describe(pt$DF01_01)

#######################
##Internal consistency#
#######################

#Commitment 
psych::alpha(pt[, c("cm01","cm02", "cm03","cm04")], 
              na.rm=TRUE)

#Accuracy 
psych::alpha(pt[, c("ac01", "ac02", "ac03", "ac04", "ac05")], 
             na.rm=TRUE)

#Impression
psych::alpha(pt[, c("ip01", "ip02", "ip03")],
                na.rm=TRUE)

#Confidence
pt$co03 = co_re03 <- car::recode(pt$co03,
                "1=11; 2=10; 3=9; 4=8; 5=7; 6=6; 7=5; 8=4; 9=3; 10=2; 11=1")
pt$co04 = co_re04 <- car::recode(pt$co04,
                "1=11; 2=10; 3=9; 4=8; 5=7; 6=6; 7=5; 8=4; 9=3; 10=2; 11=1")

psych::alpha(pt[, c("co01", "co02", "co03", "co04")],
             na.rm=TRUE)

#10 - wert spalte 


###############################################
##Decision outcome: Extension of the contract##
###############################################

#Descriptive
pt$VE03 <- factor(pt$VE03, levels = c(1, 2), labels = c("yes", "no"))
table(pt$VE03)

#Percentage yes
11/30*100

#Percentage no
19/30*100

#Proportions test decision outcome
prop.test(11,30)

#Bayesian proportion test
bf_ex <- BayesFactor::proportionBF(y=11, N=30, p=.05)

#Evidence for the alternative
bf_ex

#Evidence for the null
1/bf_ex


##############################
##Testing provided arguments##
##############################

#------------#
#Credibility #
#------------#
GW <- pt[, c("gw01", "gw02", "gw03", "gw04", "gw05", "gw06",
             "gw07", "gw08", "gw09", "gw10", "gw11", "gw12")]

#Testing Reability 
psych::alpha(GW, 
             na.rm=TRUE)

#Row means GW
GWm <- rowMeans(GW, 
                na.rm=TRUE)


#Credibility: arguments pro extension
GW_ce <- pt[, c("gw01", "gw03", "gw05", "gw07",  "gw09", "gw11")]

GW_ce$GW_ce_m <-rowMeans(GW_ce, 
                         na.rm=TRUE)

psych::describe(GW_ce$GW_ce_m)

#Credibility: arguments against extension
GW_nce <- pt[, c("gw02", "gw04", "gw06", "gw08",  "gw10", "gw12")]

GW_nce$GW_nce_m <-rowMeans(GW_nce, 
                           na.rm=TRUE)

psych::describe(GW_nce$GW_nce_m)

#Evaluation bias credibility 
t.test(GW_ce$GW_ce_m, GW_nce$GW_nce_m,
       paired = TRUE)


#-----------#
#Importance#
#-----------#
WI <- pt[, c("wi01", "wi02", "wi03", "wi04", "wi05", "wi06", 
             "wi07", "wi08", "wi09", "wi10", "wi11", "wi12")]

#Testing Reability
psych::alpha(WI, 
             na.rm=TRUE)

#Row means WI
WIm <-rowMeans(WI,
               na.rm=TRUE)


#Importance: arguments pro extension
WI_ce <-pt[, c("wi01", "wi03",  "wi05", "wi07", "wi09","wi11")]

WI_ce$WI_ce_m <-rowMeans(WI_ce, 
                         na.rm=TRUE)

psych::describe(WI_ce$WI_ce_m)

#Importance: arguments against exetension
WI_nce <-pt[, c("wi02", "wi04",  "wi06", "wi08", "wi10","wi12")]

WI_nce$WI_nce_m <-rowMeans(WI_nce, 
                           na.rm=TRUE)

psych::describe(WI_nce$WI_nce_m)

#Evaluation bias importance
t.test(WI_ce$WI_ce_m, WI_nce$WI_nce_m,
       paired=TRUE)


#-------------------------#
#Overall Credibility Score#
#-------------------------#

#Correlation: High correlation -> merging credibility and importance in one score
cor.test(GWm,WIm,
         method = "spearman",
         exact = FALSE)

#Credibility: arguments pro extension
Cre_ce <- pt[, c("gw01", "gw03", "gw05", "gw07", "gw09", "gw11",
                 "wi01", "wi03", "wi05", "wi07", "wi09", "wi11")]

Cre_ce$Cre_ce_m <-rowMeans(Cre_ce,
                         na.rm=TRUE)

psych::describe(Cre_ce$Cre_ce_m)

#Credibility: arguments against extension
Cre_nce <- pt[, c("gw02", "gw04", "gw06", "gw08", "gw10", "gw12",
                  "wi02", "wi04", "wi06", "wi08", "wi10", "wi12")]

Cre_nce$Cre_nce_m <-rowMeans(Cre_nce,
                           na.rm=TRUE)

psych::describe(Cre_nce$Cre_nce_m)


#Evaluation bias credibility
t.test(Cre_ce$Cre_ce_m, Cre_nce$Cre_nce_m,
       paired = TRUE)

#Effectsize
effsize::cohen.d(Cre_ce$Cre_ce_m, Cre_nce$Cre_nce_m,
                 paired = TRUE)

#Bayesian t.test
bf_Cre <- BayesFactor::ttestBF(Cre_ce$Cre_ce_m, Cre_nce$Cre_nce_m,
                              paired=TRUE)

#Evidence for the alternative
bf_Cre

#Evidence for the null
1/bf_Cre


#--------#
#Interest#
#--------#
IN <- pt[,c("in01", "in02", "in03", "in04", "in05", "in06", 
            "in07", "in08", "in09", "in10","in11", "in12")] 

#Reliability
psych::alpha(IN,
             na.rm=TRUE)


#Interest: arguments pro extension
IN_ce <- pt[,c("in01", "in03", "in05", "in07", "in09", "in11")]

IN_ce$IN_ce_m <-rowMeans(IN_ce, 
                         na.rm=TRUE)

psych::describe(IN_ce$IN_ce_m)

#Interest: arguments against extension
IN_nce <- pt[,c("in02", "in04", "in06", "in08", "in10", "in12")]

IN_nce$IN_nce_m <-rowMeans(IN_nce, 
                           na.rm=TRUE)

psych::describe(IN_nce$IN_nce_m)

#Evaluation bias interest
t.test(IN_ce$IN_ce_m, IN_nce$IN_nce_m, 
       paired=TRUE)

#Effectsize
effsize::cohen.d(IN_ce$IN_ce_m, IN_nce$IN_nce_m, 
                 paired=TRUE)


#Bayesian t.test
bf_IN <- BayesFactor::ttestBF(IN_ce$IN_ce_m, IN_nce$IN_nce_m, 
                              paired=TRUE)

#Evidence for the alternative
bf_IN

#Evidence for the null
1/bf_IN


#---------------#
#Read: yes or no#
#---------------#

JN <- pt[, c("CASE","JN01", "JN02", "JN03", "JN04", "JN05", "JN06", 
               "JN07", "JN08", "JN09", "JN10", "JN11", "JN12")]


#Yes:arguments pro extension

Yes_ce<-JN%>%
  gather(key="Artikel", value= "score", 
         JN01, JN03, JN05, JN07, JN09, JN11)


Yes_ce_f<- filter(Yes_ce, score ==1)


Yes_ce_sum <- Yes_ce%>%
  group_by(Artikel) %>%
  count(Yes_ce, score == 1)


#Yes: arguments against extension
Yes_nce<-JN%>%
  gather(key="Artikel", value= "score", 
         JN02, JN04, JN06, JN08, JN10, JN12)

Yes_nce_f<- filter(Yes_nce, score ==1)


Yes_nce_sum <- Yes_nce_f%>%
  group_by(Artikel) %>%
  count()

#Proportions test reading yes no
prop.test

#Bayesian proportion test
bf_yn <- BayesFactor::proportionBF

#Evidence for the alternative
bf_yn

#Evidence for the null
1/bf_yn


################################
##Testing arguments separately##
###############################

#-----------#
#Credibility#
#-----------#

#Data preparation 
GW<-pt%>%select(1,gw01:gw12)

GW<-GW%>%
  gather(key="Artikel", value= "score", 
         gw01, gw02, gw03, gw04,  gw05, gw06, gw07, gw08, gw09, gw10, gw11, gw12)%>%
         convert_as_factor(id,Artikel)

Gw_mean <-GW%>%
  group_by(Artikel) %>%
  get_summary_stats(score, type = "mean_sd")

describe(Gw_mean$mean)

#visualization

ggboxplot(GW, 
          x = "Artikel",
          y = "score", 
          add = "jitter")

#one-way ANOVA model
GW%>%
  group_by(Artikel)%>%
  identify_outliers(score)

model<-lm(score~Artikel, data=GW)

#Test ANOVA assumptions
ggqqplot(residuals(model))

shapiro_test(residuals(model))

GW%>%
  group_by(Artikel)%>%
  shapiro_test(score)

GW%>%levene_test(score~Artikel) 


#Calculation Kruskal-Wallis Test since normal distribution assumptions is violated 
res.kuskal<-GW%>%kruskal_test(score~Artikel)
res.kuskal

GW%>% kruskal_effsize(score~Artikel)

#pairwise comparison 
pwc<-GW%>%
  dunn_test(score~Artikel, p.adjust.method = "bonferroni")
pwc


#----------#
#Importance#
#-----------#

#Data preparation 

#Model without outliner

WI<-pt%>%select(1,wi01:wi12)

WI<-WI%>%
  gather(key="Artikel", value= "score", 
         wi01,  wi02, wi03, wi041, wi05, wi06,
         wi07, wi08, wi09, wi10, wi11, wi12)%>%
  convert_as_factor(id,Artikel)

WI%>%
  group_by(Artikel) %>%
  get_summary_stats(score, type = "mean_sd")

#visualization

ggboxplot(WI,
          x = "Artikel",
          y = "score", 
          add = "jitter")

#one-way ANOVA model
WI%>%
  group_by(Artikel)%>%
  identify_outliers(score)   #achtung hier outliner

model<-lm(score~Artikel, data=WI)

#Test ANOVA assumptions
ggqqplot(residuals(model))

shapiro_test(residuals(model))

WI%>%
  group_by(Artikel)%>%
  shapiro_test(score)


WI%>%levene_test(score~Artikel)

#Calculation Kruskal-Wallis Test since normal distribution assumptions is violated 
res.kuskal<-WI%>%kruskal_test(score~Artikel)
res.kuskal

WI%>% kruskal_effsize(score~Artikel)

#pairwise comparison 
pwc<-WI%>%
  dunn_test(score~Artikel, p.adjust.method = "bonferroni")
pwc


#--------#
#Interest#
#--------#

#Data preparation 
IN<-pt%>%select(1,in01:in12)

IN<-IN%>%
  gather(key="Artikel", value= "score", 
         in01,  in02, in03, in04, in05,  in06,
         in07, in08, in09, in10, in11, in12)%>%
  convert_as_factor(id,Artikel)

IN%>%
  group_by(Artikel) %>%
  get_summary_stats(score, type = "mean_sd")

#visualization

ggboxplot(IN, 
          x = "Artikel", 
          y = "score", 
          add = "jitter")

#one-way ANOVA model
IN%>%
  group_by(Artikel)%>%
  identify_outliers(score)

model<-lm(score~Artikel, data=IN)

#Test ANOVA assumptions
ggqqplot(residuals(model))

shapiro_test(residuals(model))

IN%>%
  group_by(Artikel)%>%
  shapiro_test(score)

IN%>%levene_test(score~Artikel)

#Calculation Kruskal-Wallis Test since normal distribution assumptions is violated 
res.kuskal<-IN%>%kruskal_test(score~Artikel)
res.kuskal

IN%>% kruskal_effsize(score~Artikel)


#Calculation of ANOVA

res.aov <- IN%>% anova_test(score ~ Artikel)
res.aov



#Bayesian proportion test
bf_IN <- BayesFactor::oneWayAOV.Fstat(F=1.47, N=30, J=12, rscale = "medium", simple = TRUE)

#Evidence for the alternative
bf_IN

#Evidence for the null
1/bf_IN





