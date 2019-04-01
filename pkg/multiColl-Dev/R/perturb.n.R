perturb.n <-
function(datos, n, media, dv, tol=0.01, pos=NULL){
  y = datos[,1]
  x = as.matrix(datos[,-c(1,2)])
  #
  reg = lm(y~x)
  summary(reg)
  beta = as.double(reg$coefficients)
  #
  tols = array(, n)
  normas = array(, n)
  for (j in 1:n){
    x.p = x
    for (i in pos){
      x.p[,i] = perturb(x[,i], media, dv, tol)
    }
    reg.p = lm(y~x.p)
    summary(reg.p)
    beta.p = as.double(reg.p$coefficients)
    #
    tole = matrix(, 1, pos)
    for (i in pos){
      tole[i] = (norm(x[,i]-x.p[,i],"2")/norm(x[,i],"2"))*100
    }
    tols[j] = mean(tole)
    #
    normas[j] = (norm(beta-beta.p,"2")/norm(beta,"2"))*100
  }
  ##
  return(cbind(tols,normas))
}
