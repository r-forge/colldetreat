CV_VIF <-
function(X, size=NULL, top=82.64, limit=40, dummy=FALSE, pos=NULL, intercept=TRUE){
  X = as.matrix(X)
  n = dim(X)[1]
  p = dim(X)[2]
  graf = 1
  if ((p-1) != length(size)){
    print("The number of elements of the argument 'size' is not adequate. It is not possible to represent the scatterplot")
    graf = 0
  }
  if (p <= 2){
    print("At least 3 independent variables are needed (including the intercept) to represent the scatterplot")
    graf = 0
  } 
  if (graf == 1) {
    cvs = CVs(X, dummy, pos) # CVs is a multiColl R package function
    fivs = VIF(X, dummy, pos) # VIF is a multiColl R package function
    #
    plot(cvs, fivs, type = "n", xlim=c(0, max(cvs)+2), ylim=c(-limit, max(fivs)+limit), xlab="Coefficient of Variation", ylab="Variance Inflation Factor")
    for (i in 1:length(cvs)){
      points(cvs[i], fivs[i], col = ifelse(size[i]<top, "blue", "red"), pch = 20)
    }
    abline(h=10, col="black", lwd=2, lty=2)
    abline(v=0.1002506, col="black", lwd=2, lty=3)
    etiquetas = c()
    if (intercept==TRUE) {
      for(i in 1:length(cvs)){etiquetas = c(etiquetas, i+1)}
    } else { 
      for(i in 1:length(cvs)){etiquetas = c(etiquetas, i)}
    }
    text(cvs, fivs, labels = etiquetas, pos=1)
    #
    salida = data.frame(cvs, fivs)
    row_names = paste("Variable", 2)
    for (i in 3:p){row_names = c(row_names, paste("Variable", i))}
    rownames(salida) = row_names
    colnames(salida) = c("CV", "VIF")
    return(salida)
  }
}
