RVIF <-
function(X, l_u=TRUE, l=40, intercept=TRUE, graf=TRUE){
  n = dim(X)[1]
  p = dim(X)[2]
  if (l_u == TRUE){X = lu(X)} # lu is a multiColl R package function
  RVIFs = integer()
  a = integer()
  if (intercept == TRUE) row_names = c("Intercept") else row_names = paste("Variable", 1)
  for (i in 1:p){
    if (det(crossprod(X[,-i],X[,-i])) != 0) a_i = crossprod(X[,i],X[,-i])%*%
        solve(crossprod(X[,-i],X[,-i]))%*%crossprod(X[,-i],X[,i]) else a_i = NaN
        d_i = crossprod(X[,i])
        RVIFs[i] = 1/(d_i-a_i)
        a[i] = round(a_i*100/d_i, digits=4)
        if (i>1){row_names = c(row_names, paste("Variable", i))}
  }
  if (graf == TRUE) {
    if (intercept == FALSE) {
      X = cbind(rep(1,n),X)
      CV_VIF(X, size = a, limit = l, intercept = intercept)
    } else CV_VIF(X, size = a[-1], limit = l, intercept = intercept)
  }
  salida = data.frame(RVIFs, a)
  rownames(salida) = row_names
  colnames(salida) = c("RVIF", "%")
  return(salida)
}
