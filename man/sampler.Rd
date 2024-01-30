\name{sampler}
\alias{sampler}

\title{
Execute sampling with replacement
}
\description{
Auxiliar function (employed only for internal use)
}
\usage{
sampler(n, a)
}

\arguments{
  \item{n}{
Number of sampling iterations
}
  \item{a}{
data vector 
}
}

\value{
Resampled vector of length n - Object class "numeric"
}

\author{
F. Barrio-Parra
}

\examples{

a <- rnorm (n = 20, mean = 0, sd = 1)

b <- sampler (n = 100, a = a)
}

\keyword{ methods }
