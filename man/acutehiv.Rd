\name{acutehiv}
\alias{acutehiv}
\concept{SI}
\concept{differential equation model}

\title{A model of HIV infection with acute and chronic infection stages}

\description{
  A model of HIV infection with acute and chronic infection stages.
}

\usage{example(hivmodel)}

\details{
    deqn{\frac{dS(t)}{dt}  = \Lambda -\beta_1 c S(t) \frac{I_1(t)}{N(t)}-\beta_2 c S(t) \frac{I_2(t)}{N(t)}-\mu S(t)}{dS(t)/dt = -\Lambda -\beta_1 c S(t) I_1(t)/N(t)-\beta_2 c S(t) I_2(t)/N(t)-\mu S(t)}
    \deqn{\frac{dI_1(t)}{dt}  = \beta_1 c S(t) \frac{I_1(t)}{N(t)}+\beta_2 c S(t) \frac{I_2(t)}{N(t)}- (\mu+\alpha) I_1(t)}{dI_1(t)/dt = \beta_1 c S(t) I_1(t)/N(t)+\beta_2 c S(t) I_2(t)/N(t) - (\mu+\alpha) I_1(t)}
    \deqn{\frac{dI_1(t)}{dt}=\alpha I_1(t)-(\mu+\gamma)I_2(t)}{dI_2(t)/dt = \alpha I_1(t)-(\mu+\gamma)I_2(t)}

    The state variables of the model are as follows.
    
    \describe{
      \item{\eqn{S}}{The number of susceptible individuals.}
      \item{\eqn{I_1}}{The number of individuals with acute HIV infection.}
      \item{\eqn{I_2}}{The number of individuals with chronic HIV infection.}
    }

    Time is in weeks; years are assumed to comprise of 52 weeks. The parameters of the model, and the values used in the example, are as follows.
    
    \tabular{clr}{
    \bold{Symbol} \tab \bold{Description} \tab \bold{Value} \tab \bold{Notes} \cr
    \eqn{\Lambda} \tab Rate of new susceptibles \tab 10000/(70*52) \tab Rate set to give an equilibrium population of 10,000\cr
    \eqn{\beta} \tab Probability of infection per contact for acute infection\tab 1/50 \tab \cr
    \eqn{\beta} \tab Probability of infection per contact for chronic infection\tab 1/200 \tab \cr
    \eqn{c} \tab Contact rate \tab 1 (per week) \tab \cr
    \eqn{\gamma} \tab Progression rate to AIDS/death \tab 1/520 \tab Progression rate is 10 years on average\cr
    \eqn{\alpha} \tab Progression rate to chronic infection \tab 1/8 \tab Acute infection lasts 8 weeks on average\cr
    \eqn{\mu} \tab Natural mortality rate \tab 1/(70*52) \tab Individuals live for 70 years on average
    }
}

\author{Simon Frost (\email{sdwfrost@gmail.com})}

\seealso{
       \code{\link{hivmodel}}
}

\examples{
acutehiv <- new("odeModel",
  main = function(time, init, parms, ...){
    with(as.list(c(init,parms)),{
    # ODEs
    N <- S+I1+I2
    dS <- lambd-beta1*c*S*I1/N-beta2*c*S*I2/N-mu*S
    dI1 <- beta1*c*S*I1/N+beta2*c*S*I2/N-alpha*I1-mu*I1
    dI2 <- alpha*I1-mu*I2-gamma*I2
    list(c(dS,dI1,dI2))
   })},
  parms = c(beta1=1./50,beta2=1./200,c=1,alpha=1./8,gamma=1./520,mu=1./(70*52),lambd=10000./(70*52)),
  times = c(from=0,to=60*52,by=1),
  init = c(S=9999,I1=1,I2=0),
  solver = "lsoda"
)
acutehiv <- sim(acutehiv)
}

\keyword{si,HIV}
