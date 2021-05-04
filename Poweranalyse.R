#*###*###*###*###*###*###*###*###*###*###*##*#*##*##
#*##                                            #*##
####  Replication Schulz-Hardt et al. (2000)    ####
#*##                                            #*##
#*###*###*###*###*###*###*###*###*###*###*##*#*##*##



# Monte Carlo based statistical power analysis for mediation models
# Using R package bmem (Zhang & Wang 2013)

#Install packages
if(!require(lavaan)){install.packages("lavaan", dependencies= TRUE)}
if(!require(bmem)){install.packages("bmem")}
if(!require(snowfall)){install.packages("snowfall")}

# Activate packages
library(lavaan)
library(bmem)



# Mediation Model and mediation effect specification for bmem

demo <- "
      y ~ c*x + start(.55)*x + b1*m1 + start(.55)*m1 + b2*m2 + start(.51)*m2 + b3*m3 + start(.30)*m3 + b4*m4 + start(.30)*m4
      m1 ~ a1*x + start(.56)*x
      m2 ~ a2*x + start(.43)*x
      m3 ~ a3*x + start(.30)*x
      m4 ~ a4*x + start(.30)*x
      x ~~ start(1)*x
      m1 ~~ start(1)*m1
      m2 ~~ start(1)*m2
      m3 ~~ start(1)*m3
      m4 ~~ start(1)*m4
      y ~~ start(1)*y
      "
# Mediation effect to conduct power analyses on
mediation <- "
          ind1 := a1*b1
          ind2 := a2*b2
          ind3 := a3*b3
          ind4 := a4*b4
          total := ind1 +ind2 +ind3 +ind4
         "

# Power analyses via bootstrap
set.seed(1234); power.result <- bmem::power.boot(model = demo, 
                                 indirect = mediation, 
                                 nobs = 90, 
                                 nrep = 1000,  #Zweiter Durchlauf mit 5.000
                                 nboot = 1000, 
                                 alpha = 0.95, 
                                 ci = "bc",
                                 parallel = "no",
                                 ncore = 10) 


summary(power.result)

#############################################################
## Power Analysis with package "pwr2ppl" from Aberson 2019 ##
#############################################################

install.packages("pwr2ppl")
library(pwr2ppl)

pwr2ppl::med(rxm1=.56, rxm2=.43, rxm3=.3, rxm4=.3, 
             rxy=.55,
             rym1=.55, rym2=.51, rym3=.3, rym4=.3,
             rm1m2=.23, rm1m3=.3, rm1m4=.3, rm2m3=.3, rm2m4=.3,rm3m4=.3,
             alpha= 0.05,
             mvars= 4, 
             n= 46)




