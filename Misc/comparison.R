#produce figures for the comparison of optimization methods

iter = 200
init = c(1.75,1.25)

rosen_vizopt(optimizer_output=data[1:min(iter,nrow(data)),],psy=1,R=sqrt(2)*abs(init[1]-1),pnts = F)
rosen_contour_with_trajectory(R = abs(init[1]-1)*sqrt(2), trajectory = data[1:min(iter,nrow(data)),c(1,2)], init = c(1,1))

data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_gd(n.iter=1e5,init=init,alpha=.25))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_sgd(n.iter=1e5,init=init,alpha=.25,sigma=1e-1))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_fr(n.iter=1e5,init=init,alpha=.0001))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_pr(n.iter=1e5,init=init,alpha=.0002))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_m(n.iter=1e5,init=init,alpha=.005))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_nm(n.iter=1e5,init=init,alpha=.003))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_bfgs(n.iter=1e5,init=init,alpha=1e-7))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_adagrad(n.iter=1e5,init=init,alpha=.05))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_rmsprop(n.iter=1e5,init=init,alpha=.005))


data = rbind(c(init,rosenbrock(init[1],init[2])),rosenopt_adam(n.iter=1e5,init=init,alpha=.04))
