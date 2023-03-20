#' Compute the gradient of Rosenbrock's function for given parameters
#'
#' Computes the gradient of the Rosenbrock's function for given parameters x and y
#'
#' @param x A numeric value for the first parameter.
#' @param y A numeric value for the second parameter.
#'
#' @return A vector containing the gradient of Rosenbrock's function for the given parameters.
#'
#' @examples
#' rosen_grad(1, 1)
#'
#' @export

rosen_grad <-
  function(x,y,a=1,b=100){
    dx = 2*(x-a) + b*2*(x^2-y)*2*x
    dy = -b*2*(x^2-y)
    return(c(dx,dy))
  }
