#' Optimize the Rosenbrock's function using the momentum algorithm
#'
#' Optimize the Rosenbrock's function using the momentum algorithm.
#'
#' @param init A numeric vector with the initial values of the parameters x and y (default: c(5, 5)).
#' @param n.iter A numeric value specifying the number of iterations to perform (default: 1e3).
#' @param alpha A numeric value specifying the initial learning rate.
#' @param gamma A numeric value specifying the momentum coefficient (default: 0.9).
#'
#' @return A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the functions output after each iteration on each column.
#'
#' @examples
#' rosenopt_m(n.iter=1e5)
#'
#' @export
rosenopt_m = function(init = c(5,5), n.iter=1e3, alpha=1, gamma=0.9) {
  alp.org = alpha
  gam.org = gamma
  xyz = matrix(nrow = n.iter, ncol = 3)
  vals = init
  lk = rosenbrock(vals[1], vals[2])
  grads = rosen_grad(vals[1], vals[2])
  v = 0

  for (i in 1:n.iter) {
    v.candid = gamma * v - alpha * grads
    vals.candid = vals + v.candid
    lk.candid = rosenbrock(vals.candid[1], vals.candid[2])
    if (lk.candid < lk) {
      vals = vals.candid
      lk = lk.candid
      v = v.candid
      grads = rosen_grad(vals[1], vals[2])
      xyz[i,] = c(vals[1], vals[2], lk)
      alpha = alp.org
      gamma = gam.org
    } else {
      alpha = alpha / 2
      gamma = gamma/2
    }
  }
  return(na.omit(xyz))
}
