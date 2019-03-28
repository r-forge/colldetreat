multiColLM <-
function(y, X, dummy=F, pos1=NULL, n, media, dv, tol=0.01, pos2=NULL){
  reg = lm(y~X)
  cte = array(1,length(y))
  datos = cbind(y,cte,X)
  p.n = perturb.n(datos, n, media, dv, tol, pos2)
  cuantiles = c(quantile(p.n[,2], prob=0.025), quantile(p.n[,2], prob=0.975))
  mc = multiCol(X, dummy, pos1)
  salida = list(summary(reg), cuantiles, mc)
  names(salida) = c("Linear Model", "Perturbation", "multiCol")
  return(salida)
}
