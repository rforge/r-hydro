\name{topidx.classes}
\alias{topidx.classes}
\title{make topographic index classes from a topographic index map}
\description{This function splits a dataset in n evenly distributed classes and calculates the number of elements of each class. It is very similar to \code{hist()}, but the latter does not always keep the number of breaks requested}
\usage{topidx.classes(array,n)}
\arguments{
  \item{array}{A vector of numeric values}
  \item{n}{Number of breaks}
}
\value{The function returns a data matrix with 2 columns. Column \code{breaks} contains the upper limits of each class, while \code{counts} contains the number of elements}
\references{ thePaper}
\author{Wouter Buytaert, Imperial College London}
\seealso{\code{\link{topidx}}, \code{\link{topmodel}}}
\examples{
# ToDo()
}
\keyword{models}
