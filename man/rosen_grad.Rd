% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rosen_grad.R
\name{rosen_grad}
\alias{rosen_grad}
\title{Compute the gradient of Rosenbrock's function for given parameters}
\usage{
rosen_grad(x, y, a = 1, b = 100)
}
\arguments{
\item{x}{A numeric value for the first parameter.}

\item{y}{A numeric value for the second parameter.}
}
\value{
A vector containing the gradient of Rosenbrock's function for the given parameters.
}
\description{
Computes the gradient of the Rosenbrock's function for given parameters x and y
}
\examples{
rosen_grad(1, 1)

}
