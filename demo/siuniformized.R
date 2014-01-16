# Set population size
N <- 100
# Initial number of infecteds
Iinit <- 1
# Set infectivity parameter
beta <- 1
# Set maximum time (approximately)
maxt <- 20
# Set the number of runs
numruns <- 10
# Calculate the maximum rate and number of timepoints
maxrate <- beta*N/2*N/2*(1/N)
numt <- floor(maxt*maxrate)
# Main loop
results <- list()
for(r in 1:numruns){
  # Generate vector of times
  tvec <- cumsum(c(0,rexp(numt-1,maxrate)))
  ivec <- c(Iinit,rep(NA,numt-1))
  for(i in 2:numt){
    p <- (beta*ivec[i-1]*(N-ivec[i-1])/N)/maxrate
    u <- runif(1,0,1)
    if(u<p){
      ivec[i] <- ivec[i-1]+1
    }
    else{
      ivec[i] <- ivec[i-1]
    }
  }
  results[[r]] <- data.frame(time=tvec,I=ivec)
}
# Plot simulation runs
mycols <- rainbow(numruns)
siplot <- ggplot(results[[1]],aes(x=time,y=I))+geom_line(lwd=1,col=mycols[1])+xlab("Time")+ylab("Number")
for(i in 2:numruns){
  siplot <- siplot + geom_line(lwd=1,col=mycols[i],data=results[[i]])
}
