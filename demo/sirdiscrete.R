sir.discrete <- new("discreteModel",
  main=function(time,init,parms){
    with(as.list(c(init,parms)),{
      Snew <- S-a*S*I*DELTAT
      Inew <- I+a*S*I*DELTAT-b*I*DELTAT
      Rnew <- R+b*I*DELTAT
      data.frame(S=Snew,I=Inew,R=Rnew)
  })},
  parms=list(a=0.05,b=0.1),
  init=list(S=50,I=1,R=0),
  times=c(from=0,to=50,by=0.1),
  solver="modeliterator"
)
sir.discrete <- sim(sir.discrete)
