#' Optimize the Rosenbrock's function using the Polack-Riberie algorithm
#'
#' Optimize the Rosenbrock's function using the Polack-Riberie algorithm.
#'
#' @param init A numeric vector with the initial values of the parameters x and y (default: c(5, 5)).
#' @param n.iter A numeric value specifying the number of iterations to perform (default: 1e3).
#' @param alpha A numeric value specifying the initial learning rate.
#'
#' @return A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the functions output after each iteration on each column.
#'
#' @examples
#' rosenopt_pr(n.iter=1e5)
#'
#' @export
rosenopt_pr = function(init = c(5,5), n.iter=1e3, alpha=.001) {
  alp.org=alpha
  xyz = matrix(nrow = n.iter, ncol = 3)
  vals = init
  lk = rosenbrock(vals[1], vals[2])
  grads = rosen_grad(vals[1], vals[2])
  d = -grads  # initialize direction as negative gradient

  for (i in 1:n.iter) {
    vals.candid = vals + alpha * d
    lk.candid = rosenbrock(vals.candid[1], vals.candid[2])
    if (lk.candid < lk) {
      beta = sum((rosen_grad(vals.candid[1], vals.candid[2]) - grads) * rosen_grad(vals.candid[1], vals.candid[2])) / sum(grads^2)  # compute beta using Polack-Ribiere formula
      d = -rosen_grad(vals.candid[1], vals.candid[2]) + beta * d  # update direction using Polack-Ribiere formula
      vals = vals.candid
      lk = lk.candid
      grads = rosen_grad(vals[1], vals[2])
      xyz[i,] = c(vals[1], vals[2], lk)
      alpha = alp.org
    } else {
      alpha = alpha / 2
    }
  }
  return(na.omit(xyz))
}
