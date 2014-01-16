sir.function <- new("odeModel",
  main=function(time,init,parms){
    with(as.list(c(init,parms)),{
    # ODEs
    dS <- -f(S,I,parms)
    dI <- f(S,I,parms)-mu*I
    dR <- mu*I
    list(c(dS,dI,dR))
   })},
   equations=list(
     f1=function(S,I,parms){with(as.list(parms),{beta*S*I})},
     f2=function(S,I,parms){with(as.list(parms),{beta*(S^a)*(I^b)})}
   ),
  parms=list(beta=0.1,mu=0.05),
  init=c(S=0.99,I=0.01,R=0),
  times=c(from=0,to=200,by=0.01),
  solver="lsoda"
)
