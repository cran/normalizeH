\name{normalizeH}
\alias{normalizeH}
\title{Normalized Hadamard Matrix}
\description{Converts a given Hadamard matrix to its normalized form}
\usage{normalizeH(H)}
\arguments{
	\item{H}{A Hadamard matrix}	
	}
\value{
A normalize Hadamard matrix of same dimension as the input matrix. 
}
\examples{
H = matrix(c(1,1,1,-1),nrow = 2)
normalizeH(H)
\donttest{
	require(HadamardR)
	h8 <- Hadamard_Matrix(8)
	normalizeH(h8)
}	
}
\author{Baidya Nath Mandal <mandal.stat@gmail.com>}
\keyword{Hadamard matrix}
\keyword{normalized}