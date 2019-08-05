alpha <-
function(data, tau) {
  m <- length(tau)
  nc <- ncol(data)/m
  if (nc < 1) {
    stop("Incorrect input matrix")
  }
  
  xsplit <- rep(1:nc, times = rep(m, nc)) #split condition
  
  alphaMatrix <-
    sapply(data, function(x)
      return(ifelse(x != 0, 1, 0)))
  alphaMatrixT <- t(as.matrix(alphaMatrix))
  alphaMatrixTList <- split.data.frame(alphaMatrixT, xsplit)
  return(
    list(
      alphaMatrix = alphaMatrix,
      alphaMatrixT = alphaMatrixT,
      alphaMatrixTList = alphaMatrixTList
    )
  )
}
