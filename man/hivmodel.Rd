\name{hivmodel}
\alias{hivmodel}
\concept{SI}
\concept{differential equation model}

\title{A simple model of HIV infection}

\description{
  This is a simple model of HIV infection, an SI type model in an open population.
}

\usage{example(hivmodel)}

\details{
    deqn{\frac{dS(t)}{dt}  = \Lambda -\beta S(t) \frac{I(t)}{N(t)}-\mu S(t)}{dS(t)/dt = -\Lambda -\beta S(t) I(t)/N(t)-\mu S(t)}
    \deqn{\frac{dI(t)}{dt}  = \beta S(t) \frac{I(t)}{N(t)}- (\mu+\gamma) I(t)}{dI(t)/dt = \beta S(t) I(t)/N(t) - (\mu+\gamma) I(t)}

    The state variables of the model are as follows.
    
    \describe{
      \item{\eqn{S}}{The number of susceptible individuals.}
      \item{\eqn{I}}{The number of HIV-infected individuals.}
    }

    Time is in weeks; years are assumed to comprise of 52 weeks. The parameters of the model, and the values used in the example, are as follows.
    
    \tabular{clr}{
    \bold{Symbol} \tab \bold{Description} \tab \bold{Value} \tab \bold{Notes} \cr
    \eqn{\Lambda} \tab Rate of new susceptibles \tab 10000/(70*52) \tab Rate set to give an equilibrium p\cr
    \eqn{\beta} \tab Probability of infection per contact \tab 0.01 \tab \cr
    \eqn{c} \tab Contact rate \tab 1 (per week) \tab \cr
    \eqn{\gamma} \tab Progression rate to AIDS/death \tab 1/520 \tab Progression rate is 10 years on averafe\cr
    \eqn{\mu} \tab Natural mortality rate \tab 1/(70*52) \tab Individuals live for 70 years on average
    }
}

\author{Simon Frost (\email{sdwfrost@gmail.com})}

\seealso{
       \code{\link{sirode}}.
}

\examples{
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
}

\keyword{si,HIV}
