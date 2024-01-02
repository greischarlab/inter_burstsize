
###Plasmodium falciparum simulator
Simulator_Malaria_PF <- function(R, C_V){
  
  ###Remember because the generational time for falciparum
  ###is two days... 

  parameters_n <- c(lambda =2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
                    K= 6315789,# carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
                    pmax =8.35e-6 , # rate of infection (From American Naturalist- Greischar et al. 2014)
                    muR = 1/120, #Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
                    muI = 1/120, #Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
                    c = C_V, # transmission investment (Vary)
                    B = R^(1/2), # the burst size (Vary)
                    alpha1 = 1/2, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
                    alpha2 = 1/7, #the rate of development (SimulatedTimeSeries.R)
                    muM = 200, # background mortality of the merozoite (SimulatedTimeSeries.R)
                    muG = log(2)/2.4, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
                    n1=100, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
                    n2=100) # background mortality of the merozoite (SimulatedTimeSeries.R))
              ###The number of subcompartments for infected rbc(n1)
              n1=parameters_n['n1'];
              ###The number of subcompartments for immature gametocytes
              n2=parameters_n['n2']
              ###The initial numbers
              inits_n <- c(R =5e6 ,
                           I =rep(25000/n1,n1),
                           M = 0,
                           IG=rep(0,n2),
                           G =0)
              #
             times <- seq(0, 100, by = 1/10)
             out_DDE <- 
                 ode(y = inits_n, times = times,
                 func = Erlang_Malaria,
                 parms = parameters_n)
            
              return(out_DDE)
}
###Plasmodium chabaudi simulator
Simulator_Malaria_PC <- function(R, C_V){

parameters_n <- 
  c(lambda =370000, # replenishment rate of RBC (#SimulatedTimeSeries.R)
    K = 19968254,# carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
    pmax =  4.0e-6 , # rate of infection (From American Naturalist- Greischar et al. 2014)
    muR = 0.025, #Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
    muI = 0.025, #Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
    c = C_V, # transmission investment (THE VARYING FACTOR)
    B = B_V, # the burst size (THE VARYING FACTOR)
    alpha1 = 1, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
    alpha2 = 1/2, #the rate of development (SimulatedTimeSeries.R)
    muM = 48, # background mortality of the merozoite (SimulatedTimeSeries.R)
    muG = 4, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
    n1= 100, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
    n2= 100 # background mortality of the merozoite (SimulatedTimeSeries.R)
  )

###The number of subcompartments for infected rbc(n1)
n1=parameters_n['n1'];
###The number of subcompartments for immature gametocytes
n2=parameters_n['n2']

###The initial numbers
inits_n <- c(R = 8500000, 
             I = rep(43859.65/n1,n1),
             M = 0,
             IG=rep(0,n2),
             G =0)

#
}


