#Monte Carlo based statistical power analysis for mediation models
#Using R package bmem (Zhang & Wang 2013)

#Install packages
install.packages("lavaan", dependencies= TRUE)
install.packages("Amelia")
install.packages("Rcpp")
install.packages("MASS")
install.packages("snowfall")
install.packages("snow")
install.packages("bmem") 

#Activate packages
library(snow)
library(snowfall)
library(MASS)
library(Rcpp)
library(Amelia)
library(lavaan)
library(bmem)

#Mediation Model and mediation effect specification for bmem
demo <- "
      y ~ c*x + start(.55)*x + b1*m1 + start(.55)*m1 + b2*m2 + start(.51)*m2 + b3*m3 + start(.30)*m3
      m1 ~ a1*x+start(.56)*x
      m2 ~ a2*x+start(.43)*x
      m3 ~ a3*x+start(.30)*x
      x ~~ start(1)*x
      m1 ~~ start(1)*m1
      m2 ~~ start(1)*m2
      m3 ~~ start(1)*m3
      y ~~ start(1)*y
      "
#Mediation effect to conduct power analyses on
mediation <- "
          ind1 := a1*b1
          ind2 := a2*b2
          ind3 := a3*b3
          total := ind1 + ind2 +ind3
         "
#Power Analyse via bootstrap
power.result <- power.boot(model = demo, indirect = mediation, nobs =65, nrep = 10, nboot = 10, alpha = 0.95, parallel = "multicore") #nrep?

summary(power.result)
