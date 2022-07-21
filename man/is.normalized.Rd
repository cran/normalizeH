\name{is.normalized}
\alias{is.normalized}
\title{Normalized Hadamard Matrix}
\description{Checks whether a Hadamard matrix is normalized or not.}
\usage{is.normalized(H)}
\arguments{
	\item{H}{A Hadamard matrix}	
	}
\value{
Returns TRUE when the Hadamard matrix is normalized, otherwise returns FALSE. 
}
\examples{
h2 <- matrix(c(1,1,1,-1),nrow = 2)
is.normalized(h2)
h4 <-h2 %x%t(h2)
is.normalized(h4)
h2 <- matrix(c(1,-1,1,1),nrow = 2)
is.normalized(h2)}
\author{Baidya Nath Mandal <mandal.stat@gmail.com>}
\keyword{internal}