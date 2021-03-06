% Generated by roxygen2 (4.0.2): do not edit by hand
\name{GeneratePsetsFUSE}
\alias{GeneratePsetsFUSE}
\title{This function generates parameter sets for FUSE (Clark et al., 2008).}
\usage{
GeneratePsetsFUSE(NumberOfRuns, SamplingType = "LHS",
  RoutingParameterRange = c(0.01, 5))
}
\arguments{
\item{NumberOfRuns}{number of samples to generate, can be any integer}

\item{SamplingType}{sampling procedure to use, can be "URS" or "LHS"}

\item{RoutingParameterRange}{can be any range. Default range is c(0.01,5), or a single value can be calculated with \code{KirpichT} (in this case enter something like c(0.026,0.026)))}
}
\description{
This function generates parameter sets for FUSE (Clark et al., 2008).
}
\examples{
# For reproducible results, use set.seed() before running this function.
# set.seed(123)
# parameters <- GeneratePsetsFUSE(NumberOfRuns=1000,SamplingType="LHS",RoutingParameterRange=c(0.01,5)
}

