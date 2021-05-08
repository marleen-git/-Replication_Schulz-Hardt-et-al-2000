##################################################################
## Evaluation Experiment Replication Schulz-Hardt et al. (2000) ##
##################################################################

#Set working directory 
setwd("~/Desktop/Psychologie Master/Masterarbeit /MA_R-Skripte/Replication_Schulz-Hardt-et-al-2000/Evaluation Experiment")

##############
## Packages ##
#############

#Load packages
if(!require(psych)){install.packages("psych")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(BayesFactor)){install.packages("BayesFactor")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(effsize)){install.packages("effsize")}
if(!require(janitor)){install.packages("janitor")}

#Activate packages
library(psych)
library(tidyverse)
library(BayesFactor)
library(ggplot2)
library(effsize)
library(janitor)

###############
## Read data ##
###############

#Data import
ds<- readxl::read_excel("Datenblatt_Simulation_Schulz-Hardt et al. (2000).xlsx")
View(ds)

#Check variable class
str(ds)

######################
## Data preparation ##
######################

#Remove unneccessary columns 
ds[,c(6,
      8,
      52,
      106:108 )] <-NULL

glimpse(ds)

################################
## Testing exclusion criteria ##
################################

#Missings: No missings 
anyNA(ds)
which(is.na(ds))
sum(is.na(ds))
table(is.na(ds))

# overview missingsin all variables (columns)
colSums(is.na(ds))

# Overview missings in all participants (row); participants who answer less than  90% of the items will be excluded 
rowSums(is.na(ds))

#Visualization of missings
VIM::summary(aggr(ds))

#Long string (Johnson, 2005), Participants with a string of consistent responses equal or greater than half the length of the total scales will be excluded 


############################
## Subject characteristic ##
############################
             
#Gender female= 1, male= 2, diverse= 3
ds%>%
  select(gen)%>%
  apply(2,table)
             
#Age
psych::describe(ds$age)

############################################################
## Individual decision outcome: Extension of the contract ##
############################################################

#Descriptive yes= 1, no= 0
ds%>%
  select(ind_ce)%>%
  apply(2,table)

#Percentage yes=1 and no=0
prop.table(table(ds$ind_ce))

#Sum pro extension of the contract
pro_ce <- sum(c(ds$ind_ce==1),
               na.rm = TRUE)

#Sum contra extension of the contract
cont_ce <- sum(c(ds$ind_ce==0),
                na.rm = TRUE)


#Proportions test decision outcome
prop.test(x = pro_ce, 
          n = pro_ce + cont_ce,
          p = 0.5, 
          correct = FALSE)

#Cohen's h,if significant: small = 0.20, mdeium = 0.50, large = 0.80
phis <- 2*asin(sqrt(prop))
Cohenh <-phis[1]-phis[2]
Cohenh

#Bayesian proportion test
bf_ind_ce <- BayesFactor::proportionBF(y= pro_ce, 
                                   N= pro_ce + cont_ce,
                                   p=.05)

#Evidence for the alternative
bf_ind_ce

#Evidence for the null
1/bf_ind_ce

#######################################################
## Group decision outcome: Extension of the contract ##
#######################################################

#Descriptive yes= 1, no= 0
ds%>%
  select(grp_ce)%>%
  apply(2,table)

#Percentage yes=1 and no=0
prop.table(table(ds$grp_ce)) -> prop

#Sum pro extension of the contract
pro_ce <- sum(c(ds$grp_ce==1),
              na.rm = TRUE)

#Sum contra extension of the contract
cont_ce <- sum(c(ds$grp_ce==0),
                na.rm = TRUE)


#Proportions test decision outcome
prop.test(x = pro_ce, 
          n = pro_ce + cont_ce,
          p = 0.5, 
          correct = FALSE)

#Cohen's h,if significant: small = 0.20, mdeium = 0.50, large = 0.80
phis <- 2*asin(sqrt(prop))
Cohenh <-phis[1]-phis[2]
Cohenh


#Bayesian proportion test
bf_grp_ce <- BayesFactor::proportionBF(y= pro_ce, 
                                       N= pro_ce + cont_ce,
                                       p=.05)

#Evidence for the alternative
bf_grp_ce

#Evidence for the null
1/bf_grp_ce

############################################################
## Check for possible interfering effects: gender and age ##
############################################################

# Descriptive gen: female= 1, male= 2, diverse= 3; ind_ce: yes= 1, no=0
choice_gen<-ds%>%
              group_by(gen) %>%
              count(ind_ce)  

choice_gen

prop.table(choice_gen$n) 
  

pro_gen_score<-ds%>%
                 group_by(gen) %>%
                 filter(ind_ce==1) %>%
                 count()

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









contra_gen_score<-ds%>%
                    group_by(gen) %>%
                    filter(ind_ce==0) %>%
                    count() 




ds%>% anova_test()






# Descriptive age 
ds%>%
  group_by(age) %>%
  count(ind_ce) 


##################################
## Confirmation bias individual ##
##################################

#How many arguments pro contract extension where chosen by each participant
ds%>%
  select(ind_read01, 
      ind_read03, 
      ind_read05, 
      ind_read07, 
      ind_read09, 
      ind_read11)%>%
    rowSums(na.rm=TRUE) -> sum_pro_ce

sum(sum_pro_ce)

#How many arguments pro contract extension where chosen by each participant
ds%>%
  select(ind_read02, 
         ind_read04, 
         ind_read06, 
         ind_read08, 
         ind_read10, 
         ind_read12)%>%
  rowSums(na.rm=TRUE) -> sum_cont_ce

sum(sum_cont_ce)

#New columns with sum pro extension and sum against extension chosen by each participant 
ds_new<-cbind(ds, sum_pro_ce, sum_cont_ce)
glimpse(ds_new)

#########################################################
# Confirmation bias participants pro contract extension #
#########################################################

#Number of people who chose to extent contract and chose pro arguments 
ds_new%>%
  filter(ind_ce == 1)%>%
  select(sum_pro_ce)%>%
  colSums(na.rm=TRUE) ->pro_ce_pro

#Number of participants who chose to extent contract and chose conflicting arguments
ds_new%>%
  filter(ind_ce == 1)%>%
  select(sum_cont_ce)%>%
  colSums(na.rm=TRUE) ->pro_ce_cont

#Net confirmation bias for participants pro extension 
pro_ce_pro - pro_ce_cont

#Proportions test of article supporting and conflicting with pre-choice pro extension
prop.test(x = pro_ce_pro, 
          n = pro_ce_pro + pro_ce_cont,
          p = 0.5, 
          correct = FALSE)


#Bayesian proportion test
bf_pro_ce <- BayesFactor::proportionBF(y= pro_ce_pro, 
                                       N= pro_ce_pro + pro_ce_cont,
                                       p=.05)

#Evidence for the alternative
bf_pro_ce

#Evidence for the null
1/bf_pro_ce

#############################################################
# Confirmation bias participants against contract extension #
#############################################################

#Number of people who chose not to extent contract and chose contra arguments 
ds_new%>%
  filter(ind_ce == 0)%>%
  select(sum_cont_ce)%>%
  colSums(na.rm=TRUE) ->cont_ce_cont

#Number of participants who chose to extent contract and chose conflicting arguments
ds_new%>%
  filter(ind_ce == 0)%>%
  select(sum_pro_ce)%>%
  colSums(na.rm=TRUE) ->cont_ce_pro

#Net confirmation bias for participants pro extension 
cont_ce_cont - cont_ce_pro

#Proportions test of article supporting and conflicting with pre-choice against extension
prop.test(x = cont_ce_cont, 
          n = cont_ce_cont + cont_ce_pro,
          p = 0.5, 
          correct = FALSE)

#Bayesian proportion test
bf_cont_ce <- BayesFactor::proportionBF(y= cont_ce_cont, 
                                       N= cont_ce_cont + cont_ce_pro,
                                       p=.05)

#Evidence for the alternative
bf_pro_ce

#Evidence for the null
1/bf_pro_ce


##################################
# Extra Overview article choices #
##################################

# Overview individual choice pro contract extension and number of chosen supporting arguments
ind_pro_ce<-ds%>%
                filter(ind_ce==1)%>%
                select(ind_read01, 
                       ind_read03, 
                       ind_read05, 
                       ind_read07, 
                       ind_read09, 
                       ind_read11)

colSums(ind_pro_ce,na.rm=TRUE) 
colMeans(ind_pro_ce,na.rm=TRUE)
      
#Overview individual choice pro extension and number of chosen conflicting arguments 
ind_pro_ca<-ds%>%
                filter(ind_ce==1)%>%
                select(ind_read02, 
                       ind_read04, 
                       ind_read06, 
                       ind_read08, 
                       ind_read10, 
                       ind_read12) 
 
colSums(ind_pro_ca,na.rm=TRUE)
colMeans(ind_pro_ca,na.rm=TRUE)

# Overview individual choice against contract extension and number of chosen supporting arguments
ind_cont_ce<-ds%>%
  filter(ind_ce==0)%>%
  select(ind_read02, 
         ind_read04, 
         ind_read06, 
         ind_read08, 
         ind_read10, 
         ind_read12)

colSums(ind_cont_ce,na.rm=TRUE) 
colMeans(ind_cont_ce,na.rm=TRUE)

#Overview individual choice pro extension and number of chosen conflicting arguments 
ind_cont_ca<-ds%>%
  filter(ind_ce==0)%>%
  select(ind_read01, 
         ind_read03, 
         ind_read05, 
         ind_read07, 
         ind_read09, 
         ind_read11)

colSums(ind_cont_ca,na.rm=TRUE)
colMeans(ind_cont_ca,na.rm=TRUE)


##############################
## Confirmation bias groups ##
##############################

#How many arguments pro contract extension where chosen by each group
ds%>%
  select(grp_read01, 
         grp_read03, 
         grp_read05, 
         grp_read07, 
         grp_read09, 
         grp_read11)%>%
  rowSums(na.rm=TRUE) -> sum_pro_ce_grp

sum(sum_pro_ce_grp)

#How many arguments pro contract extension where chosen by each group
ds%>%
  select(grp_read02, 
         grp_read04, 
         grp_read06, 
         grp_read08, 
         grp_read10, 
         grp_read12)%>%
  rowSums(na.rm=TRUE) -> sum_cont_ce_grp

sum(sum_cont_ce_grp)

#New columns with sum pro extension and sum against extension chosen by each group 
ds_new<-cbind(ds, sum_pro_ce_grp, sum_cont_ce_grp)
glimpse(ds_new)

#########################################################
# Confirmation bias groups pro contract extension #
#########################################################

#Number of groups who chose to extent contract and chose pro arguments 
ds_new%>%
  filter(grp_ce == 1)%>%
  select(sum_pro_ce_grp)%>%
  colSums(na.rm=TRUE) ->pro_ce_grp_pro

#Number of groups who chose to extent contract and chose conflicting arguments
ds_new%>%
  filter(grp_ce == 1)%>%
  select(sum_cont_ce_grp)%>%
  colSums(na.rm=TRUE) ->pro_ce_grp_cont

#Net confirmation bias for groups pro extension 
pro_ce_grp_pro - pro_ce_grp_cont

#Proportions test of article supporting and conflicting with pre-choice pro extension
prop.test(x = pro_ce_grp_pro, 
          n = pro_ce_grp_pro + pro_ce_grp_cont,
          p = 0.5, 
          correct = FALSE)


#Bayesian proportion test
bf_pro_ce_grp <- BayesFactor::proportionBF(y= pro_ce_grp_pro, 
                                       N= pro_ce_grp_pro + pro_ce_grp_cont,
                                       p=.05)

#Evidence for the alternative
bf_pro_ce_grp

#Evidence for the null
1/bf_pro_ce_grp

#############################################################
# Confirmation bias groups against contract extension #
#############################################################

#Number of people who chose not to extent contract and chose contra arguments 
ds_new%>%
  filter(grp_ce == 0)%>%
  select(sum_cont_ce_grp)%>%
  colSums(na.rm=TRUE) ->cont_ce_grp_cont

#Number of participants who chose to extent contract and chose conflicting arguments
ds_new%>%
  filter(grp_ce == 0)%>%
  select(sum_pro_ce_grp)%>%
  colSums(na.rm=TRUE) ->cont_ce_grp_pro

#Net confirmation bias for participants pro extension 
cont_ce_grp_cont - cont_ce_grp_pro

#Proportions test of article supporting and conflicting with pre-choice against extension
prop.test(x = cont_ce_grp_cont, 
          n = cont_ce_grp_cont + cont_ce_grp_pro,
          p = 0.5, 
          correct = FALSE)

#Bayesian proportion test
bf_cont_ce_grp <- BayesFactor::proportionBF(y= cont_ce_grp_cont, 
                                        N= cont_ce_grp_cont + cont_ce_grp_pro,
                                        p=.05)

#Evidence for the alternative
bf_pro_ce_grp

#Evidence for the null
1/bf_pro_ce_grp


##########################
## Statisticized groups ##
##########################

#statisticized groups' information search was calculated by counting how many 
# of the articles supporting and conflicting with the subsequent group decision 
# were requested by at least one group member.
view(ds_new)

#Number supporting articles chosen by  groups 



ds_new%>%
  filter(id_grp == 1)%>%
  select(sum_pro_ce)%>%
  colSums(na.rm=TRUE) 

x<-1

for(i in 1:nrow(ds_new)/3)
{
  x_for<-
    ds_new%>%
    filter(id_grp == x)%>% 
    select(sum_pro_ce)%>%
    colSums(na.rm=TRUE) 
  
        
print(x_for)
}



#Number conflicting articles chosen by  groups 
ds_new%>%
  filter(id_grp == 1)%>%
  select(sum_cont_ce)%>%
  colSums(na.rm=TRUE)


