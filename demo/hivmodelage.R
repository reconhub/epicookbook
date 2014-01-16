hivmodel.discrete.age <- new("discreteModel",
	main = function(time, init, parms, ...){
		with(parms,{
                	S <- init[1:m]
                	I <- init[(m+1):(2*m)]
                	Stot <- sum(S)
                	Itot <- sum(I)
                        N <- Stot+Itot
                        Snew <- S
                        Inew <- I
                        Snew[1] <- S[1]+(lambd-mu*S[1]-beta*c*S[1]*Itot/N)*DELTAT-(S[1])*(DELTAT/DELTAX)
                        Snew[2:(m-1)] <- S[2:(m-1)]+(-mu*S[2:(m-1)]-beta*c*S[2:(m-1)]*Itot/N)*DELTAT-(S[2:(m-1)]-S[1:(m-2)])*(DELTAT/DELTAX)
                        Snew[m] <- S[m]+(-mu*S[m]-beta*c*S[m]*Itot/N)*DELTAT-(-S[m-1])*(DELTAT/DELTAX)
                	Inew[1] <- I[1] + (beta*c*S[1]*Itot/N-gamma*I[1]-mu*I[1])*DELTAT-(I[1])*(DELTAT/DELTAX)
			Inew[2:(m-1)] <- I[2:(m-1)]+(beta*c*S[2:(m-1)]*Itot/N-gamma*I[2:(m-1)]-mu*I[2:(m-1)])*DELTAT-(I[2:(m-1)]-I[1:(m-2)])*(DELTAT/DELTAX)
			Inew[m] <- I[m]+(beta*c*S[m]*Itot/N-gamma*I[m]-mu*I[m])*DELTAT-(-I[m-1])*(DELTAT/DELTAX)				
		c(S=as.numeric(Snew),I=as.numeric(Inew))
	})},	
	parms = list(beta=0.01,c=1,gamma=1./520,mu=1./(70*52),lambd=10000./(70*52),DELTAX=52,m=15*70),
        init = c(S=vector(mode="numeric"),I=vector(mode="numeric")),
	times = c(from=0,to=3000,by=1),
	solver = "modeliterator"
)
hivmodel.discrete.age <- sim(hivmodel.discrete.age)
