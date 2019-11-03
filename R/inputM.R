inputM <-
function(data){
        tau <- unique(c(data$L, data$R))
        tau <- sort(tau[is.finite(tau) & tau > 0])
        cols <- colnames(data)
        cols <- cols[startsWith(cols,'C')]
        m_out <- matrix(0, nrow = length(tau), ncol = nrow(data)*length(cols))
        rownames(m_out) <- tau
        colnames(m_out) <- c(sapply(cols, function(x) rep(x, nrow(data))))
        
        for(i in 1:nrow(data)){
            L <- data$L[i]
            R <- data$R[i]
            if(L<R){
                time_slots <- sum(tau > L & tau <= R)  
            }else{
                time_slots <- sum(tau == R)
            }
            
            possible_risks <- sum(data[i,cols])
            
            try <- 1/(possible_risks * time_slots)
            
            if(L<R){
                time_bucket <- which(tau > L & tau <= R)
            }else{
                time_bucket <- which(tau == R)
            }
            C_bucket <- (which(data[i,cols]==1)-1)*nrow(data)+i
            
            m_out[time_bucket, C_bucket] <- try
        }
        return(list(input = m_out, tau = tau))
    }
