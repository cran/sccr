\name{survCompeting}
\alias{survCompeting}

\title{Self-Consistent, Competing Risks (SC-CR) Algorithms}

\description{
This package describes an algorithm for producing fully non-parametric and self-consistent estimators of the cause-specific failure probabilities in the presence of interval-censoring and possible masking of the failure cause in a competing risks environment.   It is a generalization of Turnbull's (1976) classic univariate algorithm.  
The algorithm was published in Adamic et al. (2010) and Adamic & Caron (2014).
}

\usage{
survCompeting(data, tau, n, nc, epsilon)
}

\arguments{
\item{data}{input matrix of probabilities}
\item{tau}{the vector of time points corresponding to columns in input matrix}
\item{n}{the number of intervals in the dataset corresponding to rows in input matrix}
\item{nc}{the number of causes (competing risks)}
\item{epsilon}{small predermined value > 0}

}

\value{
\item{Yj}{estimated number at risk at time tau_j}
\item{djc}{estimated number of events occuring at time tau_j by cause c}
\item{pjc}{estimated probability for risk at time tau_j by cause c}
\item{djList}{the list of d_j for every cause c}
\item{pjList}{the list of p_j for every cause c}
\item{pjListold}{the list of p_j for every cause c in the (iter - 1) iteration}
\item{iter}{the number of iterations in the algorithm}
}

\references{
1. Adamic, P., Caron, S. (2014),
"SC-CR Algorithms with Informative Masking", 
Scandinavian Actuarial Journal, 2014(4), 339-351. \cr

2. Adamic, P., Dixon, S., Gillis, D. (2010), 
"Multiple Decrement Modeling in the Presence of Interval
Censoring and Masking", Scandinavian Actuarial Journal, 2010(4), 312-327. \cr

3. Adamic, P., Ouadah, S. (2009), 
"A Kernel Method for Modeling Interval Censored Competing
Risks", South African Statistical Journal, 43(1), 1-20. \cr

4. Turnbull, B. (1976).  The Empirical Distribution Function with Arbitrarily Grouped, Censored and Truncated Data, 
Journal of the Royal Statistical Society. Series B (Methodological), 38(3), 290-295.
}


\author{Peter Adamic, Alicja Wolny-Dominiak}

\examples{
data(censoredMatrix)
df <- inputM(censoredMatrix)

res <- survCompeting(df$input, df$tau, 8, 3, 0.01)
res

#summary
round(res$Yj, 2)
round(res$djc, 2)
round(res$pjc, 2)
res$iter
sum(unlist(res$pjList))
sum(unlist(res$pjListold))
}

