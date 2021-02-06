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

#######################
#Predict system times #
#######################

N <- 90 

system.time(bootstrap<-bmem::power.boot(demo, indirect = mediation, N, nrep = 10, nboot = 10, parallel = 'multicore'))
system.time(bootstrap<-bmem::power.boot(demo, indirect = mediation, N, nrep = 30, nboot = 30, parallel = 'multicore'))
system.time(bootstrap<-bmem::power.boot(demo, indirect = mediation, N, nrep = 60, nboot = 60, parallel = 'multicore'))
system.time(bootstrap<-bmem::power.boot(demo, indirect = mediation, N, nrep = 100, nboot = 100, parallel = 'multicore'))

library(tidyverse)
install.packages("tibble")
library(tibble)
library(ggplot2)

# Load the times from above into a dataframe
benchmark <- tibble(bootstraps = c(10, 30, 60, 100),
                    times = c(8.133, 43.086, 178.412, 544.586)) 

# Plot the points and fit a curve
ggplot(benchmark, aes(x = bootstraps, y = times)) +
   geom_point() +
   geom_smooth(se = FALSE, span = 5)

# Fit a model
fit <- lm(data = benchmark, times~poly(bootstraps,
                                       2, raw=TRUE))
newtimes <- data.frame(bootstraps = seq(100, 1000, length = 4))

# Predict the time it will take for larger bootstrap/rep values
predict(fit, newdata = newtimes)
>        1          2          3          4 
>  544.0899  9697.4746 30191.6173 62026.5178 

# Convert from seconds to hours
print(6290525.94 /60/60)
>[1] 17.22959

