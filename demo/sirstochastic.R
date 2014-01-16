sir.parms <- c(beta=0.1/1000,gamma=0.05,N=1000)
sir.x0 <- c(S=999,I=1,R=0)
sir.a <- c("beta*S*I","gamma*I")
sir.nu <- matrix(c(-1,+1,0,0,-1,+1),nrow=3,ncol=2,byrow=FALSE)
runs <- 10
set.seed(1)
sir.out <- data.frame(time=numeric(),S=integer(),I=integer(),R=integer())
for(i in 1:runs){
  sim <- ssa(sir.x0,sir.a,sir.nu,sir.parms,tf=250,simName="SIR")
  sim.out <- data.frame(time=sim$data[,1],S=sim$data[,2],I=sim$data[,3],R=sim$data[,4])
  sim.out$run <- i
  sir.out <- rbind(sir.out,sim.out)
}
