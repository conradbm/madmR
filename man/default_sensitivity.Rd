% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sensitivity.R
\name{default_sensitivity}
\alias{default_sensitivity}
\title{default_sensitivty}
\usage{
default_sensitivity(DM, DB, attr, algs, algParams, step, verbose, window)
}
\arguments{
\item{DM, }{the original decision matrix, in read.decision.matrix format}

\item{DB, }{wearhouse to store all of our iterations of data being created.}

\item{attr, }{specified attribute(s) to run sensitivty on.}

\item{algs, }{specified algorithms to run sensitivity on.}

\item{algParams, }{parameters to be passed around to each function in sensitivity}

\item{step, }{incremental step size each iteration.}

\item{verbose, }{echos print statements to understand input/output relation.}

\item{window, }{specified window size vector e.g., c(0,.5) only to allow weights to change between the given window range.}
}
\value{
data.frame
}
\description{
Helper function to sensitivity analysis
}
\author{
Blake Conrad \email{bmc.cs@outlook.com}
}
\keyword{data.frame,}
\keyword{ranking}
