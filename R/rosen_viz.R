#' Visualize Rosenbrock's Function in 3D using Plotly
#'
#' This function generates a 3D visualization of Rosenbrock's function using Plotly. The function is visualized over a range of values for x and y.
#'
#' @param R A numeric value specifying the maximum range of x and y values to visualize.
#' @param cls A vector of length 2 specifying the colors for the minimum and maximum values of the function (default: c('orange', 'steelblue')).
#'
#' @return A Plotly visualization of Rosenbrock's function in 3D.
#'
#' @examples
#' rosen_viz(R = 1)
#'
#' @export
rosen_viz = function(R = .825, cls = c('steelblue', 'brown1'),Rseq=50,lseq=100){
  r = seq(0, R, length.out = Rseq)
  theta = seq(0, 2*pi, length.out = lseq)
  grd = expand.grid(r, theta)
  x = grd[1] * cos(grd[2])
  y = grd[1] * sin(grd[2])
  x=x+1
  y=y+1
  xy = matrix(cbind(x[,1],y[,1]))
  z = apply(cbind(x[,1],y[,1]),1, FUN = function(w) rosenbrock(w[1],w[2]))
  mat.points=as.matrix(cbind(x,y,z))

  library(plotly)

  # Create a custom colorscale
  custom_colorscale <- list(
    list(0, cls[1]),
    list(1, cls[2])
  )

  # Create a heatmap of the loss
  fig1 = plot_ly(
    x = mat.points[, 1], # X-axis
    y = mat.points[, 2], # Y-axis
    z = mat.points[, 3], # Z-axis
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 1.2, color = mat.points[, 3], colorscale = custom_colorscale) # Use the custom colorscale
  )

  fig1 <- fig1 %>% layout(title = list(text = '', font = list(size = 28)),
                          scene = list(xaxis = list(title = "x", tickfont = list(size = 14), titlefont = list(size = 34)),
                                       yaxis = list(title = "y", tickfont = list(size = 14), titlefont = list(size = 34)),
                                       zaxis = list(title = "Loss", tickfont = list(size = 14), titlefont = list(size = 34))))


  fig1
}
