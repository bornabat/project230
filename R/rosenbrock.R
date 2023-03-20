#' Compute the value of Rosenbrock's function for given parameters
#'
#' Computes the value of the Rosenbrock's function for given parameters x and y
#'
#' @param x A numeric value for the first parameter.
#' @param y A numeric value for the second parameter.
#'
#' @return The value of Rosenbrock's function for the given parameters.
#'
#' @examples
#' rosenbrok(1, 1)
#'
#' @export

rosenbrock <-
  function(x, y, a=1, b=100) {
    return((a - x)^2 + b*(y - x^2)^2)
  }
