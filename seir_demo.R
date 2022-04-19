library(deSolve)
library(tidyverse)

source("seir_demo_func.r")

#model parameters
N <- 100000
R0 <- 5 #basic reproduction number

dt <- 1 # output time increment in days
time_ints <- seq(0, 84, 7) #time intervals

parms <- c(gamma = 1/5, #recovery rate
           beta = NA,   #transmission rate
           alpha = 1/3, #incubation transition rate
           popsize = N) #

parms[["beta"]] <- R0 * parms[["gamma"]] #transmission rate

#initial conditions assuming everyone is susceptible
parms_init <- c(s_init = NA, e_init =0, i_init = 15, r_init =0 )
parms_init[["s_init"]] <- parms[["popsize"]] - parms_init[["i_init"]]

output_sim <- run_sim(dt,parms,parms_init,time_ints)

options(scipen = 999)

plot(time_ints/7, output_sim$s_trace ,type = "l", main = "Simple SEIR model", 
     ylab = "population", xlab = "weeks", col = "dodgerblue", lwd =3 )
lines(time_ints/7, output_sim$e_trace ,type = "l", col ="dimgrey",lwd =3)
lines(time_ints/7, output_sim$i_trace ,type = "l", col ="green4",lwd =3)
lines(time_ints/7, output_sim$r_trace ,type = "l", col ="darkorange",lwd =3)
legend(8, 60000, legend = c("Susceptible", "Exposed", "Infected", "Recovered"), 
       col = c("dodgerblue", "purple", "green4", "darkorange"),
       cex = 0.7, lty = 1, box.lty=0, xpd = TRUE)

