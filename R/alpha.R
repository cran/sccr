alpha <-
function(data, tau) {
        m <- length(tau)
        nc <- ncol(data)/m
        if (nc < 1) {
            stop("Incorrect input matrix")
        }
        
        data[data != 0] <- 1
        alphaMatrixT <- t(as.matrix(data))
        xsplit <- rownames(alphaMatrixT) #split condition
        alphaMatrixTList <- split.data.frame(alphaMatrixT, xsplit)
        
        return(alphaMatrixTList = alphaMatrixTList)
    }
