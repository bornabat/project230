#' Optimize the Rosenbrock's function using the ADAM algorithm
#'
#' Optimize the Rosenbrock's function using the ADAM (Adaptive Moment Estimation) algorithm.
#'
#' @param init A numeric vector containing the initial values of the parameters x and y (default: c(5, 5)).
#' @param n.iter A numeric value specifying the number of iterations to perform (default: 1e3).
#' @param alpha A numeric value specifying the initial step size.
#' @param beta1 A numeric value specifying the exponential decay rate for the first moment estimates (default: 0.9).
#' @param beta2 A numeric value specifying the exponential decay rate for the second moment estimates (default: 0.999).
#' @param epsilon A small positive numeric value as the threshold for the change in function value between iterations (default: 1e-8).#'
#' @return A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the function's output after each iteration. Each row represents an iteration, and the columns contain the x, y, and function output values, respectively.
#'
#' @examples
#' rosenopt_adam(init = c(5, 5), n.iter = 1e3, alpha = 1, epsilon = 1e-6)
#' @export
rosenopt_adam = function(init = c(5,5), n.iter=1e3, alpha=0.001, beta1=0.9, beta2=0.999, epsilon=1e-8) {
alp.org = alpha
xyz = matrix(nrow = n.iter, ncol = 3)
vals = init
lk = rosenbrock(vals[1], vals[2])
grads = rosen_grad(vals[1], vals[2])
m = 0
v = 0
t = 0

for (i in 1:n.iter) {
  t = t + 1  # increment time step
  m = beta1 * m + (1 - beta1) * grads  # update first moment estimate
  v = beta2 * v + (1 - beta2) * grads^2  # update second moment estimate
  m_hat = m / (1 - beta1^t)  # compute bias-corrected first moment estimate
  v_hat = v / (1 - beta2^t)  # compute bias-corrected second moment estimate
  vals.candid = vals - alpha * m_hat / (sqrt(v_hat) + epsilon)  # update candidate position using ADAM formula
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
