\name{survCompeting}
\alias{survCompeting}

\title{Self-Consistent, Competing Risks (SC-CR) Algorithm}

\description{
The SC-CR Algorithms (which stands for Self-Consistent, Competing Risks Algorithms) are a class of algorithms designed to estimate, in a fully nonparametric manner, the failure probabilities for each cause in a competing risks environment.  We present the interval-censored form here.  Starting with an initial matrix of failure probabilities for each time point and competing risk, the algorithm iterates until the discrepancy between all time points and risks is less than some predetermined tolerance of error, epsilon.  When probabilities converge to non-zero values, these are called "cause-specific innermost intervals".  Masking, which can be defined as lack of certitude in the exact cause of failure, is permitted for this algorithm.  Finally, although we provide decidedly self-consistent estimators for each of the incidence functions, it is also strongly suspected that the estimators are also nonparametric maximum likelihood estimators as well.}

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
res <- survCompeting(censoredMatrix, 2:9, 8, 3, 0.01)
res

#summary
round(res$Yj, 2)
round(res$djc, 2)
round(res$pjc, 2)
res$iter
sum(unlist(res$pjList))
sum(unlist(res$pjListold))
}

