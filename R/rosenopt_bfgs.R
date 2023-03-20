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
rosenopt_bfgs = function(init = c(5, 5), n.iter = 1e3, alpha = 0.001, epsilon = 1e-6, delta = 1e-8) {

  try_solve <- function(H, delta_init = delta) {
    H.reg <- H
    success <- FALSE
    delta <- delta_init

    while (!success) {
      result <- tryCatch(solve(H.reg), error = function(e) NULL)
      if (!is.null(result)) {
        success <- TRUE
      } else {
        H.reg <- H.reg + delta * diag(rep(1, nrow(H.reg)))
        delta <- delta * 2
      }
    }

    return(H.reg)
  }


  alp.org = alpha

  xyz = matrix(nrow = n.iter, ncol = 3)
  vals = init
  lk = rosenbrock(vals[1], vals[2])
  grads = rosen_grad(vals[1], vals[2])
  H = diag(2)  # initialize Hessian approximation as identity matrix

  for (i in 1:n.iter) {

    H.reg = try_solve(H)

    vals.candid = vals - alpha * (solve(H.reg) %*% grads)
    lk.candid = rosenbrock(vals.candid[1], vals.candid[2])

    if (abs(lk.candid - lk) < epsilon) {  # check if change in function value is below threshold
      break
    }

    s = matrix(vals.candid - vals, nrow = 2, ncol = 1)
    y = matrix(rosen_grad(vals.candid[1], vals.candid[2]) - grads, nrow = 2, ncol = 1)
    rho = 1 / (t(y) %*% s)
    I = diag(2)
    H = (I - c(rho) * (s %*% t(y))) %*% H %*% (I - c(rho) * (y %*% t(s))) + c(rho) * (s %*% t(s))

    if (lk.candid < lk) {
      vals = vals.candid
      lk = lk.candid
      grads = rosen_grad(vals[1], vals[2])
      xyz[i, ] = c(vals[1], vals[2], lk)
      alpha = alp.org
    } else {
      alpha = alpha / 2
    }
  }

  return(na.omit(xyz))
}
