# define epidemic functions
simulate <- function (parms, state, times){
  SEIR_model <- function(time, state, parms){
    S <- state[["s_init"]]
    E <- state[["e_init"]]
    I <- state[["i_init"]]
    R <- state[["r_init"]]
    
    #extract parameters
    gamma <- parms[["gamma"]]
    beta <- parms[["beta"]]
    alpha <- parms[["alpha"]]
    N <- parms[["popsize"]]
    
    foi <- beta*(I/N)
    
    #differential equations
    dS <- -S*foi
    dE <- S*foi - alpha*E
    dI <- alpha*E - gamma*I
    dR <- gamma*I
    return(list(c(dS, dE, dI, dR)))
  }
  
  ode_output <- as.data.frame(ode(state, times, SEIR_model, parms, method = "ode45"))
  return(ode_output)
}

run_sim <- function(dt, parms, parms_init, time_ints){
  
  init_cond <- c( s_init = parms_init[["s_init"]],
                  e_init = parms_init[["e_init"]],
                  i_init = parms_init[["i_init"]],
                  r_init = parms_init[["r_init"]]) 
  
  output <- simulate(parms, init_cond, seq(0, max(time_ints), 0.5))
  s_out<- output[match(time_ints,output$time),"s_init"]
  e_out<- output[match(time_ints,output$time),"e_init"]
  i_out<- output[match(time_ints,output$time),"i_init"]
  r_out<- output[match(time_ints,output$time),"r_init"]
  
  return(list(s_trace = s_out, e_trace = e_out, i_trace = i_out, r_trace = r_out))
}

