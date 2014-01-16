setClass("discreteModel",representation(parms="list",init="numeric"),contains="simObj")
modeliterator <- function(y,times=NULL,func=NULL,parms=NULL,animate=FALSE,...){
	init <- y@init
  	times <- fromtoby(y@times)
  	func <- y@main
  	parms <- y@parms
  	inputs <- y@inputs
  	equations <- y@equations
  	equations <- addtoenv(equations)
  	environment(func) <- environment()
  	parms$DELTAT <- 0
  	out <- init
 
  	for(i in 2:length(times)){
    		time <- times[i]
    		parms$DELTAT <- times[i]-times[i-1]
    		init <- func(time,init,parms)
    		out <- rbind(out,init)
	}
  	row.names(out) <- NULL
 	out <- cbind(times,out)
  	as.data.frame(out)
}

setMethod("plot",c("discreteModel","missing"), function(x,y,...){
	o <- out(x)
  	nm <- names(o)
  	nplot <- length(nm)-1
  	par(mfrow=c(1,nplot),pty="s")
  	for(i in 1:nplot){
    		plot(o$times,o[,i+1],xlab="Time",ylab=nm[i+1])
  	}
})

