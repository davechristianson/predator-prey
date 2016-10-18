
rm(list=ls())  
### Lotka Volterra Predator Prey equation

## this script uses a funciton to derive predator and prey population dynamics
## from initial population sizes.  
## Assume negative density dependence in the prey population.
    
    ## starting population sizes
    ##          Nprey = starting pop size of prey
    ##          Npred = starting pop size of predators

    ## paramters in LV, 
    ##          rexp= instantanous growth rate of prey population (in the absence of negative density dependence)  (r), 
    ##                    e.g. rexp=0.33, implies one third of the populatoin will add one individual each when there are no constraints on growth
    ##          kill= rate of successfull encounter, attack AND kill in units of kills/predator/prey 
    ##                    e.g., kill=0.001 means each wolf will kill 1 out of every 1000 elk in the populatoin
    ##          con= conversion efficiency, the number of predators gained per prey killed, predators/prey
    ##                    e.g con=0.1, implies it takes 10 elk kills to produce one wolf
    ##          survival = survival rate of predator

    ## output from LV
    ##          Nprey = number of prey in a given iteration
    ##          Npred = number of predators in a given iteration
    ##          dprey = net change in the prey population (in total indivduals) between time steps
    ##          dpred = net change in the predator population (in total individuals) between time steps

library(simecol)

newlv <- new("odeModel",                                                              ## start odemodel
          main = function (time, init, parms) { ## Execute a function with the given inputs
              with(as.list(c(init, parms)), {
                  morts <- rbinom(1,ceiling(Npred),1-survival)  # makes predator mortalities stochastic drawn randomly from binomial distriubtion
                  dprey <-   rexp * Nprey * (1-(Nprey/mean(rbinom(15,preyK,0.65)) - kill * Nprey * Npred  ## prey side of LV 
                  dpred <-  -(morts) + kill*con * Nprey * Npred          ## predator side of LV
                  list(c(dprey, dpred))                                               ## end of function
              })
          },
          parms  = c(rexp = 0.33, kill = 0.00025, con=0.05,survival = 0.65,preyK=30000,  ## parameter settings for LV
          times  = c(from = 0, to = 100, by = 1),                                      ## time span to play out pop dynamics
          init   = c(Nprey = 10000, Npred = 250),                                       ## initial population sizes 
          solver = "rk4"                                                              ## specify method for solving the ordinary differential equations
)                                                                                       ## end of odemodel

newlvout<-sim(newlv)  ## run simulation with given parameters
plot(newlvout)        ## plot simulation with given parameters


out(newlvout)    ## output from simulation with given parameters

cbind(out(newlvout)$Npred,out(newlvout)$Npred - c(out(newlvout)$Npred[-1],NA),
      (out(newlvout)$Npred - c(out(newlvout)$Npred[-1],NA))/out(newlvout)$Npred)

