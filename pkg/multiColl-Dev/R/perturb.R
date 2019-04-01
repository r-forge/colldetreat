perturb <-
function(x, media, dv, tol=0.01){
  p = rnorm(length(x), media, dv)
  x.p = x + p*tol*(norm(x,"2")/norm(p,"2"))
  return(x.p)
}
