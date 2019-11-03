survCompeting <-
function(data, tau, n, nc, epsilon) {
        m <- length(tau)
        
        if (m != nrow(data)) {
            stop("Incorrect number of rows in input matrix")
        }
        
        alfaa <- data
        alfaa[alfaa != 0] <- 1
        
        alfaa <- t(as.matrix(alfaa))
        xsplit <- rownames(alfaa) #split condition
        alfaaa <- split.data.frame(alfaa, xsplit)
        
        #split alfa matrix
        alfa <- lapply(alfaaa, function(x)
            return(t(x)))
        alfa
        
        #split input matrix
        inputt <- t(as.matrix(data))
        inputtt <- split.data.frame(inputt, xsplit)
        inputList <- lapply(inputtt, function(x)
            return(t(x)))
        inputList
        
        ##Step 1 - j in rows!
        pjList <- lapply(inputList, function(x)
            return(apply(x, 1, sum) / m))
        pjList
        
        sum(unlist(pjList)) == 1
        
        ##loop
        eps <- 1
        iter <- 1
        
        while (eps >= epsilon) {
            ##Step 2
            alfapj <- NULL
            alfapj <- lapply(1:nc, function(c) {
                for (i in 1:m) {
                    #changing n
                    alfapj <- cbind(alfapj, alfa[[c]][, i] * pjList[[c]])
                }
                return(alfapj)
            })
            
            alfapj
            
            #sum in columns
            aisum <-
                lapply(1:nc, function(c)
                    return(apply(alfapj[[c]], 2, sum)))
            aisum
            
            aisumc <- dplyr::bind_cols(lapply(1:nc, function(c)
                data.frame(aisum[[c]])))
            colnames(aisumc) <- c(1:ncol(aisumc))
            aisumc
            
            aisumcc <- apply(aisumc, 1, sum)
            
            
            ##d
            dd <- NULL
            dd <- lapply(1:nc, function(c) {
                for (i in 1:n) {
                    dd <- cbind(dd, alfapj[[c]][, i] / aisumcc[i])
                }
                return(dd)
            })
            
            
            dList <- lapply(1:nc, function(c) {
                return(apply(dd[[c]], 1, sum))
            })
            
            djc <- NULL
            djc <- dplyr::bind_cols(lapply(1:nc, function(c)
                data.frame(dList[[c]])))
            colnames(djc) <- c(1:ncol(djc))
            djc <- cbind(djc, tau)
            
            #check
            sum(djc[-1]) == 8
            
            #Step 3
            Y <- vector(length = m)
            Y[1] <- m
            
            for (j in 2:m) {
                Y[j] <- Y[j - 1] - sum(djc[j - 1,-4])
            }
            
            Yj <- data.frame(Y, tau)
            
            ##Step4
            pjListold <- pjList
            pjList <- lapply(dList, function(x)
                return(x / Y))
            
            #pjc table
            pjc <-  dplyr::bind_cols(lapply(1:nc, function(c)
                data.frame(pjList[[c]])))
            colnames(pjc) <- c(1:ncol(pjc))
            pjc <- cbind(pjc, tau)
            
            eps <- max(abs(unlist(pjListold) - unlist(pjList)))
            print(paste('STOP criteria eps: ', eps))
            
            
            print(paste('Sum of OLD pjc: ', sum(unlist(pjListold))))
            print(paste('Sum of NEW pjc: ', sum(unlist(pjList))))
            
            print(paste('Iteriation: ', iter))
            iter = iter + 1
            
            print('--------------')
        }
        
        return(
            list(
                Yj = Yj,
                djc = djc,
                pjc = pjc,
                iter = iter,
                eps = eps,
                djList = dList,
                pjList = pjList,
                pjListold = pjListold
            )
        )
    }
