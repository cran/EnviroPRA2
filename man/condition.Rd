\name{condition}
\alias{condition}

\title{
p-value significance checking function
}
\description{
Auxiliar function to check p-value significance (Function created for internal use of the model).
}
\usage{
condition(n)
}

\arguments{
  \item{n}{
p-value
}
}

\value{
Return "Significant" or "Not-significant" - Object class "character"
}


\examples{

condition ( 0.001)

condition (0.1)

}

\keyword{ methods }