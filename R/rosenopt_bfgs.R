#' Optimize the Rosenbrock's function using the BFGS algorithm
#'
#' Optimize the Rosenbrock's function using the BFGS (Broyden-Fletcher-Goldfarb-Shanno) algorithm.
#'
#' @param init A numeric vector with the initial values of the parameters x and y (default: c(5, 5)).
#' @param n.iter A numeric value specifying the number of iterations to perform (default: 1e3).
#' @param alpha A numeric value specifying the initial step size.
#' @param epsilon A small positive numeric value as the threshold for the change in function value between iterations (default: 1e-6).
#'
#' @return A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the function's output after each iteration. Each row represents an iteration, and the columns contain the x, y, and function output values, respectively.
#'
#' @examples
#' rosenopt_bfgs(init = c(5, 5), n.iter = 1e3, alpha = 1, epsilon = 1e-6)
#' @export
rosenopt_bfgs = function(init = c(5,5), n.iter=1e3, alpha=1, epsilon=1e-6) {
  alp.org=alpha
  xyz = matrix(nrow = n.iter, ncol = 3)
  vals = init
  lk = rosenbrock(vals[1], vals[2])
  grads = rosen_grad(vals[1], vals[2])
  H = diag(2)  # initialize Hessian approximation as identity matrix

  for (i in 1:n.iter) {
    vals.candid = vals - alpha * solve(H, grads)
    lk.candid = rosenbrock(vals.candid[1], vals.candid[2])
    if (abs(lk.candid - lk) < epsilon) {  # check if change in function value is below threshold
      break
    }
    s = matrix(vals.candid - vals, nrow = 2, ncol = 1)
    y = matrix(rosen_grad(vals.candid[1], vals.candid[2]) - grads, nrow = 2, ncol = 1)
    rho = 1 / (t(y) %*% s)
    I = diag(2)
    H = (I - rho * (s %*% t(y))) %*% H %*% (I - rho * (y %*% t(s))) + rho * (s %*% t(s))
    if (lk.candid < lk) {
      vals = vals.candid
      lk = lk.candid
      grads = rosen_grad(vals[1], vals[2])
      xyz[i,] = c(vals[1], vals[2], lk)
      alpha = 1
    } else {
      alpha = alpha / 2
    }
  }
  return(na.omit(xyz))
}
