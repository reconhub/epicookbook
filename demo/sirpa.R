sir.pa.ode <- new("odeModel",
  main = function(time, init, parms, ...){
    with(as.list(c(init,parms)),{
      # Closure
      SSI <- (k-1)*SS*SI/(k*S)
      ISS <- (k-1)*SI*SS/(k*S)
      ISI <- (k-1)*SI*SI/(k*S)
      # ODEs
      dS <- -beta*SI
      dI <- beta*SI-gamma*I
      dSS <- -beta*(SSI+ISS)
      dSI <- beta*(SSI-ISI)-beta*SI-gamma*SI
      dII <- beta*(2*ISI)+2*beta*SI-2*gamma*II
      list(c(dS,dI,dSS,dSI,dII))
    })},
  parms = c(beta=3,gamma=1.0,k=5),
  times = c(from=0,to=20,by=0.01),
  init = c(S=0.999,I=0.001,SS=0.999*0.999,SI=0.999*0.001,II=0.001*0.001),
  solver = "lsoda"
)
sir.pa.ode <- sim(sir.pa.ode)

