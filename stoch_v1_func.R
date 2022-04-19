run.stoch.model = function(parms, parms_init, t.max, t.step)
{
  
  gamma <- parms[["gamma"]]
  beta <- parms[["beta"]]
  alpha <- parms[["alpha"]]
  N <- parms[["popsize"]]
  i_init <- parms_init[["i_init"]]
  
  S = N - i_init
  E = 0
  I = i_init
  R = 0
  
  T = seq(1, t.max, by=t.step)
  res = matrix(nrow=(t.max+1),ncol=5)
  colnames(res)= c('time','S','E','I','R')
  res[1,] = c(1,S,E,I,R)
  
  #loop through remaining timestep
  for (step in 2:length(T)) {
    
    #Draw new exposed 
    p_E = 1 - exp(-beta*(I/N)*t.step)
    Enew = rbinom(1, size = S, prob = p_E)
    
    #Draw new infections
    p_I = 1-exp(-alpha*t.step)
    Inew = rbinom(1,size = E, prob = p_I)
    
    #Draw new removals
    p_R = 1-exp(-gamma*t.step)
    Rnew = rbinom(1, size=I, prob = p_R)
    
    #update state
    S = S- Enew
    E = E+ Enew - Inew
    I = I+ Inew - Rnew
    R = R + Rnew
    
    # store
    res[step,] = c(T[step],S,E,I,R)
  }
  res
  
}
