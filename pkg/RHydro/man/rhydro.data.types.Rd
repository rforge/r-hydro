\name{rhydro.data.types}
\alias{rhydro.data.types}
\docType{data}
\title{Definitions for default data types for the RHydro package}
\description{
This data.frame defines names, symbols and default units for RHydro
data types. The data.frame also tells, in which mass or energy balance
calculations the data types are included.
}
%\usage{data(rhydro.data.types)} !Remove to avoid error message from R check
\format{
  A data frame with 5 variables.
  \describe{
    \item{\code{data.type}}{character vector with names}
    \item{\code{symbol}}{character vector with short symbols}
    \item{\code{max.slope}}{parameter for consistency check}
    \item{\code{max.value}}{parameter for consistency check}
    \item{\code{min.slope}}{parameter for consistency check}
    \item{\code{min.value}}{parameter for consistency check}
    \item{\code{preferred.units}}{character vector with prefered units}
    \item{\code{balance.type}}{character vector with comma separated list of balance types in whicht the data.type is of importance}
  }
}
\examples{
str(rhydro.data.types)
}
\keyword{datasets}
