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
power.result <- bmem::power.boot(model = demo, 
                                 indirect = mediation, 
                                 nobs = 90, 
                                 nrep = 10, 
                                 nboot = 10, 
                                 alpha = 0.95, 
                                 ci = "bc",
                                 parallel = "multicore") 

summary(power.result)

#Power curve
nobs <- seq(10, 90, by=5)


result.powercurve <- power.curve(model = demo,
                            indirect = mediation,
                            nobs = nobs, 
                            type = "boot",
                            nrep = 10,
                            nboot = 10,
                            alpha = .95,
                            parallel = "multicore")


# Biased-corrected confidence intervals
bmem <- bmem.ci.bc(par.boot = result.powercurve, 
                 par0 = 90, 
                 cl = 0.95)

summary(bmem)
