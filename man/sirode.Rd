\name{sirode}
\alias{sirode}
\concept{SIR}
\concept{differential equation model}

\title{Susceptible-infected-recovered (SIR) differential equation model using simecol}

\description{
  This is a susceptible-infected-recovered (SIR) differential equation model using simecol.
}

\usage{example(sirode)}

\details{
 \deqn{\frac{dS(t)}{dt}  = -\beta S(t) I(t)}{dS(t)/dt = -\beta S(t) I(t)}
    \deqn{\frac{dI(t)}{dt}  = \beta S(t) I(t)- \gamma I(t)}{dI(t)/dt = \beta S(t) I(t)- \gamma I(t)}
    \deqn{\frac{dR(t)}{dt}  = \mu I(t)}{dR(t)/dt = \gamma I(t)}

    The state variables of the model are as follows.

    \describe{
      \item{\eqn{S}}{The number of susceptible individuals.}
      \item{\eqn{I}}{The number of infected individuals.}
      \item{\eqn{R}}{The number of recovered individuals.}
    }

    The parameters of the model, and the values used in the example, are as follows.
    
    \tabular{clr}{
    \bold{Symbol} \tab \bold{Description} \tab \bold{Value}\cr
    \eqn{\beta} \tab Infectivity parameter \tab 0.1\cr
    \eqn{\gamma} \tab Recovery rate \tab 0.05
    }
}

\author{Simon Frost (\email{sdwfrost@gmail.com})}

\seealso{
       \code{\link{sircn}}.
}

\examples{
sir.ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
  # ODEs
  dS <- -beta*S*I
  dI <- beta*S*I-gamma*I
  dR <- gamma*I
  list(c(dS,dI,dR))
  })
}
parms <- c(beta=0.1,gamma=0.05)
init <- c(S=0.99,I=0.01,R=0) 
times <- seq(0,200,by=0.01)
sir.out <- lsoda(init,times,sir.ode,parms)
}
