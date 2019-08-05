\name{alpha}
\alias{alpha}
\title{The alpha matrix}
\usage{
alpha(data, tau)
}
\arguments{
\item{data}{input matrix of probabilities}
\item{tau}{the vector of time points corresponding to columns in input matrix}
}
\description{
The matrix corresponding I_(ijy) function
}

\references{
Adamic, P., Caron, S. (2014),
"SC-CR Algorithms with Informative Masking", 
Scandinavian Actuarial Journal, 2014(4), 339-351. 
}

\examples{
data(censoredMatrix)
alpha(censoredMatrix, 2:9)
}
