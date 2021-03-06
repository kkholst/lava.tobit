% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clprobit.R
\name{clprobit}
\alias{clprobit}
\title{Composite Likelihood for probit latent variable models}
\usage{
clprobit(
  x,
  data,
  k = 2,
  type = c("nearest", "all"),
  pairlist,
  silent = TRUE,
  ...
)
}
\arguments{
\item{x}{\code{lvm}-object}

\item{data}{data.frame}

\item{k}{Size of composite groups}

\item{type}{Determines number of groups. With \code{type="nearest"} (default)
only neighboring items will be grouped, e.g. for \code{k=2}
(y1,y2),(y2,y3),... With \code{type="all"} all combinations of size \code{k}
are included}

\item{pairlist}{A list of indices specifying the composite groups. Optional
argument which overrides \code{k} and \code{type} but gives complete
flexibility in the specification of the composite likelihood}

\item{silent}{Turn output messsages on/off}

\item{\dots}{Additional arguments parsed on to lower-level functions}
}
\value{
An object of class \code{clprobit} inheriting methods from \code{lvm}
}
\description{
Estimate parameters in a probit latent variable model via a composite
likelihood decomposition.
}
\seealso{
\link{estimate}
}
\author{
Klaus K. Holst
}
\keyword{models}
\keyword{regression}
