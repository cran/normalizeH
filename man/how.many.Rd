\name{how.many}
\alias{how.many}
\title{How many elements are TRUE in a logical vector}
\description{This function finds number of elements which are TRUE in a logical vector}
\usage{how.many(x)}
\arguments{
	\item{x}{A logical vector}	
	}
\value{
A non-negative integer of length 1.
}
\examples{
x <- c(1,-1,1,-1,1,-1)
how.many(x == -1)
how.many(seq(1,7,2) == 3)
}
\author{Baidya Nath Mandal <mandal.stat@gmail.com>}
\keyword{internal}