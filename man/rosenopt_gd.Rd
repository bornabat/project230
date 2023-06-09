% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rosenopt_gd.R
\name{rosenopt_gd}
\alias{rosenopt_gd}
\title{Optimize the Rosenbrock's function using gradient descent}
\usage{
rosenopt_gd(init = c(5, 5), n.iter = 1000, alpha = 1)
}
\arguments{
\item{init}{A numeric vector with the initial values of the parameters x and y (default: c(5, 5)).}

\item{n.iter}{A numeric value specifying the number of iterations to perform (default: 1e3).}

\item{alpha}{A numeric value specifying the initial learning rate.}
}
\value{
A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the functions output after each iteration on each column.
}
\description{
Optimize the Rosenbrock's function using gradient descent.
}
\examples{
rosenopt_gd(n.iter=1e5)

}
