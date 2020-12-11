\name{inputM}
\alias{inputM}

\title{
The probability matrix creator}

\description{
The function creating the probability matrix and tau time vector from the double-censored data.
}

\arguments{
  \item{data}{censored data}
}


\value{
  \item{input}{the probability matrix}
  \item{tau}{time tau}
}


\author{
Alicja Wolny-Dominiak, Peter Adamic}

\examples{
data(censoredMatrix)
res <- inputM(censoredMatrix)

res$input
res$tau
}