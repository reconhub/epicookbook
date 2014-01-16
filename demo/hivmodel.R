hivmodel <- new("odeModel",
  main = function(time, init, parms, ...){
    with(as.list(c(init,parms)),{
      # ODEs
      N <- S+I
      dS <- lambd-beta*c*S*I/N-mu*S
      dI <- beta*c*S*I/N-gamma*I-mu*I
      list(c(dS,dI))
    })},
    parms = c(beta=0.01,c=1,gamma=1./520,mu=1./(70*52),lambd=10000./(70*52)),
    times = c(from=0,to=30*52,by=1),
    init = c(S=9999,I=1),
    solver = "lsoda"
)
hivmodel <- sim(hivmodel)
hivmodel.out <- out(hivmodel)
# Turn output into 'long' format
hivmodel.out.long <- melt(as.data.frame(hivmodel.out),"time")
# Now plot
ggplot(hivmodel.out.long,aes(x=time,y=value,colour=variable,group=variable))+
  # Add line
  geom_line(lwd=1)+
  #Add labels
  xlab("Time")+ylab("Number")
