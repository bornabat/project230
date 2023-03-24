#' Optimize the Rosenbrock's function using a stochastic hill climbing algorithm
#'
#' Optimize the Rosenbrock's function using a stochastic hill climbing algorithm.
#'
#' @param init A numeric vector with the initial values of the parameters x and y (default: c(5, 5)).
#' @param n.iter A numeric value specifying the number of iterations to perform (default: 1e3).
#' @param alpha A numeric value specifying the step size.
#' @param batch An integer value specifying the number of candidates to generate at each iteration.
#' @param sigma A numeric value specifying the standard deviation for generating candidates.
#'
#' @return A matrix with the value of the optimized Rosenbrock's parameters (x and y) and the function's output after each iteration. Each row represents an iteration, and the columns contain the x, y, and function output values, respectively.
#'
#' @examples
#' rosenopt_shc(init = c(5, 5), n.iter = 1e3, alpha = 1e-3, batch = 32, sigma = 1e-6)
#' @export
rosenopt_shc = function(init = c(5,5),n.iter=1e3,alpha=.5,batch=32,sigma=1e-2){
  alp.org = alpha
  xyz = matrix(nrow=n.iter,ncol=3)
  vals=init
  lk = rosenbrock(vals[1],vals[2])
  grads = rosen_grad(vals[1],vals[2])

  for(i in 1:n.iter){


    vals.candid = vals - alpha*grads/norm(grads,'2')
    vals.batch = cbind(rnorm(batch,vals.candid[1],sigma),rnorm(batch,vals.candid[2],sigma))
    vals.batch = cbind(vals.batch,apply(vals.batch,1,function(x){rosenbrock(x[1],x[2])}))
    candid = which.min(vals.batch[,3])
    lk.candid = vals.batch[candid,3]
    if(lk.candid<lk){
      vals = vals.batch[candid,c(1,2)]
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
