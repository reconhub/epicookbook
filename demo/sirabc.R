# Initialise
parms <- c(beta = 0.1, gamma = 0.05)
init <- c(S = 0.99, I = 0.01, R = 0) 
times <- seq(0, 400, by = 0.01)
#
kMaxIt <- 50
theta <- rep(0, kMaxIt)
kLow.bound <- 0
kHigh.bound <- 0.2
kTol <- 0.001
# Define model
sir.ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
  # ODEs
  dS <- -beta*S*I
  dI <- beta*S*I-gamma*I
  dR <- gamma*I
  list(c(dS,dI,dR))
  })
}	 
# Run ODE solver
sir.out <- as.data.frame(lsoda(init, times, sir.ode, parms))			 
			 
# Select target	from ODE solver results		 
target <- sir.out[nrow(sir.out), c("S", "I")]

#
pb <- txtProgressBar(min=0,max=kMaxIt,initial=0)
# Run sampler
for (t in 1:kMaxIt) {
  setTxtProgressBar(pb,i)
  repeat {
    # Sample from prior
    theta.star <- runif(1, kLow.bound, kHigh.bound)
    # Simulate
    parms2 <- c(beta = 0.1, gamma = theta.star)
    sir.out2 <- as.data.frame(lsoda(init, times, sir.ode, parms2))
    x.star <- sir.out2[nrow(sir.out2), c("S", "I")]
    # Calculate distance
    if(sum((x.star - target)^2) < kTol) {
      # Update particle population
      theta[t] <- theta.star
      break
    } 
  } 
}
close(pb)
						   
# Plot
hist(theta, xlab = "mu", breaks=3, main = "ABC rejection algorithm")
	   
		   

