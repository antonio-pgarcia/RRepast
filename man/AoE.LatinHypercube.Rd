% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrepast-aoe.R
\name{AoE.LatinHypercube}
\alias{AoE.LatinHypercube}
\title{AoE.LatinHypercube}
\usage{
AoE.LatinHypercube(n = 10, factors = c(), convert = TRUE)
}
\arguments{
\item{n}{The number of samples}

\item{factors}{The model's parameters which will be evaluated}

\item{convert}{Adjust experiment matrix to parameter scale}
}
\value{
The LHS design matrix for provided parameters
}
\description{
Generate a LHS sample for model parameters
}
\details{
Generate the LHS sampling for evaluating
the parameters of a model.
}
\examples{
\dontrun{
 f<- AddFactor(name="cyclePoint",min=40,max=90)
 f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
 d<- AoE.LatinHypercube(2,f)}

}
