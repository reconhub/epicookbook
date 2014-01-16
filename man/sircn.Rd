\name{sircn}
\alias{sircn}
\concept{SIR}
\concept{differential equation model}

\title{Susceptible-infected-recovered (SIR) differential equation model on a configuration network.}

\description{
  This is a susceptible-infected-recovered (SIR) differential equation model on a configuration network.
}

\usage{example(sircn)}

\details{
    \deqn{\frac{d\theta(t)}{dt} = -\beta \theta(t)+\beta\frac{\psi'(\theta(t))}{\psi'(1)}+\gamma\left(1-\theta(t)\right)}{d\theta(t)/dt = -\beta \theta(t)+\beta(\psi'(\theta(t))/\psi'(1))+\gamma(1-\theta(t))}
    \deqn{S(t) = \psi(\theta(t))}{S(t) = \psi(\theta(t))}
    \deqn{I(t) = 1-S(t)-R(t)}{I(t) = 1-S(t)-R(t)}
    \deqn{\frac{dR(t)}{dt} = \gamma I}{dR(t)/dt = \gamma I}

    The state variables of the model are as follows.
    
    \describe{
      \item{\eqn{\theta}}{The fraction of degree one nodes that are susceptible.}
      \item{\eqn{S}}{The fraction of nodes that are susceptible.}
      \item{\eqn{I}}{The fraction of nodes that are infected.}
      \item{\eqn{R}}{The fraction of nodes that are recovered.}
    }

    The parameters of the model, and the values used in the example, are as follows.
    
    \tabular{clr}{
    \bold{Symbol} \tab \bold{Description} \tab \bold{Value}\cr
    \eqn{\beta} \tab Infectivity parameter \tab 0.1\cr
    \eqn{\gamma} \tab Recovery rate \tab 0.05\cr
    \eqn{k} \tab Mean of Poisson-distributed degree distribution \tab 5
    }
}

\references{
  \describe{
    \item{1}{Volz, E.M. (2008) SIR dynamics in random networks with heterogeneous connectivity.\emph{Mathematical Biology} \bold{56}:293-310.}
    \item{2}{Decreusefond, L., Dhersin, J.-S., Moyal, P., and Tran, V.C. (2012) Large graph limit for an SIR process in random network with heterogeneous connectivity. \emph{Annals of Applied Probability} \bold{22}:541-575.}
  }
}

\author{Simon Frost (\email{sdwfrost@gmail.com})}

\seealso{
       \code{\link{sirode}}.
}

\examples{
sir.cn.ode <- new("odeModel",
  main = function(time, init, parms, ...){
    with(as.list(c(init,parms)),{
      dtheta <- -beta*theta+beta*(dpsi(theta,k)/dpsi(1,k))+gamma*(1-theta)
      S <- psi(theta,k)
      I <- 1-S-R
      dR <- gamma*I
      list(c(dtheta,dR))
    })},
  equations = list(),
  parms = c(beta=0.1,gamma=0.05,k=5),
  times = c(from=0,to=125,by=0.01),
  init = c(theta=0.999,R=0),
  solver = "lsoda"
)
poisgn <- list(
  psi = function(theta,k){theta^k},
  dpsi = function(theta,k){k*theta^(k-1)},
  dpsi2 = function(theta,k){k*(k-1)*theta^(k-2)}
)
equations(sir.cn.ode) <- poisgn
sir.cn.ode <- sim(sir.cn.ode)
sir.cn.out <- out(sir.cn.ode)
sir.cn.out$S <- sir.cn.out$theta^parms(sir.cn.ode)[["k"]]
sir.cn.out$I <- 1-sir.cn.out$S-sir.cn.out$R
}

\keyword{sir,network}
