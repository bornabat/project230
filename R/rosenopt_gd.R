#' Optimize the Rosenbrock's function using gradient descent
#'
#' Optimize the Rosenbrock's function using gradient descent.
#'
#' @param init A numeric vector with the initial values of the parameters x and y (default: c(5, 5)).
#' @param n.iter A numeric value specifying the number of iterations to perform (default: 1e3).
#' @param alpha A numeric value specifying the initial learning rate.
#'
#' @return A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the functions output after each iteration on each column.
#'
#' @examples
#' rosenopt_gd(n.iter=1e5)
#'
#' @export
rosenopt_gd = function(init = c(5,5),n.iter=1e3,alpha=1){
  alp.org = alpha
  xyz = matrix(nrow=n.iter,ncol=3)
  vals=init
  lk = rosenbrock(vals[1],vals[2])
  grads = rosen_grad(vals[1],vals[2])

  for(i in 1:n.iter){


    vals.candid = vals - alpha*grads/norm(grads,'2')
    lk.candid = rosenbrock(vals.candid[1],vals.candid[2])
    if(lk.candid<lk){
      vals = vals.candid
      lk = lk.candid
      grads = rosen_grad(vals[1],vals[2])
      xyz[i,] = c(vals[1],vals[2],lk)
      alpha = alp.org
    }
    else{
      alpha = alpha/2
    }
  }
  return(na.omit(xyz))
}
