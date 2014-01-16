wnvode <- new("odeModel",
  main = function(time, init, parms, ...){
    with(as.list(c(init,parms)),{
      # ODEs
      dsa <- mua-(b*betaa*beta(nv,av,omega,time)*iv*sa/na)-mua*sa
      dia <- (b*betaa*beta(nv,av,omega,time)*iv*sa/na)-(gammaa+mua+alphaa)*ia
      div <- (b*betav/na)*ia*(1-iv)+(av*omega*cos(omega*time)/beta(nv,av,omega,time))*iv-(1-p)*muv*iv
      dna <- mua-mua*na-alphaa*ia
      list(c(dsa,dia,div,dna))
  })},
  equations = list(
    beta = function(nv,av,omega,time){nv-av*sin(omega*time)}
  ),
  parms = c(b=0.5,p=0.007,betaa=1,uv=0.06,omega=2*pi/365,nv=5,av=0,betav=0.68,gammaa=0.26,alphaa=0.15,mua=0.0002,muv=0.06),
  times = c(from=0,to=3000,by=1),
  init = c(sa=1,ia=0,iv=0.001,na=1),
  solver = "lsoda"
)
wnvode.bj.noseason <- wnvode
parms(wnvode.bj.noseason) <- c(b=0.5,p=0.007,betaa=1,uv=0.06,omega=2*pi/365,nv=5,av=0,betav=0.68,gammaa=0.26,alphaa=0.15,mua=0.0002,muv=0.06)
wnvode.bj.noseason <- sim(wnvode)
wnvode.bj.noseason.inits <- out(wnvode.bj.noseason)[dim(out(wnvode.bj.noseason))[1],][2:5]
wnvode.bj <- wnvode
parms(wnvode.bj)["av"] <- 3
init(wnvode.bj) <- unlist(wnvode.bj.noseason.inits)
wnvode.bj <- sim(wnvode.bj)

