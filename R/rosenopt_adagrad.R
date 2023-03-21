#' Optimize the Rosenbrock's function using the Adagrad algorithm
#'
#' Optimize the Rosenbrock's function using the Adagrad algorithm.
#'
#' @param init A numeric vector with the initial values of the parameters x and y (default: c(5, 5)).
#' @param n.iter A numeric value specifying the number of iterations to perform (default: 1e3).
#' @param alpha A numeric value specifying the initial step size.
#' @param epsilon A small positive numeric value as the threshold for the change in function value between iterations (default: 1e-6).
#'
#' @return A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the function's output after each iteration. Each row represents an iteration, and the columns contain the x, y, and function output values, respectively.
#'
#' @examples
#' rosenopt_adagrad(init = c(5, 5), n.iter = 1e3, alpha = 1, epsilon = 1e-6)
#' @export
rosenopt_adagrad = function(init = c(5,5), n.iter=1e3, alpha=0.1, epsilon=1e-6) {
alp.org = alpha
xyz = matrix(nrow = n.iter, ncol = 3)
vals = init
lk = rosenbrock(vals[1], vals[2])
grads = rosen_grad(vals[1], vals[2])
g = 0

for (i in 1:n.iter) {
  g = g + grads^2  # update sum of squared gradients
  vals.candid = vals - alpha * grads / (sqrt(g) + epsilon)  # update candidate position using Adagrad formula
  lk.candid = rosenbrock(vals.candid[1], vals.candid[2])
  if (abs(lk.candid - lk) < epsilon) {  # check if change in function value is below threshold
    break
  }
  if (lk.candid < lk) {
    vals = vals.candid
    lk = lk.candid
    grads = rosen_grad(vals[1], vals[2])
    xyz[i,] = c(vals[1], vals[2], lk)
    alpha = alp.org  # reset step size to default value
  } else {
    alpha = alpha / 2  # reduce step size
  }
}
return(na.omit(xyz))
}
