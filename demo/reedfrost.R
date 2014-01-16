reedfrost<-function(q=0.975,S0=99,I0=1,R0=0,nsteps=10)
{

  time <- seq(0,nsteps)

  S <- c(S0,rep(0,nsteps))
  I <- c(I0,rep(0,nsteps))
  R <- c(R0,rep(0,nsteps))

  for (i in seq(2,nsteps)){
    S[i] <- rbinom(1,n=S[i-1],prob=1-q^I[i-1])
    I[i] <- S[i-1]-S[i]
    R[i] <- R[i-1]+I[i-1]
    print(paste(i,S[i],I[i],R[i]))
    if(I[i]==0){
      break
      # really need to carry forward
    }
  }
  data.frame(time,S,I,R)
}
