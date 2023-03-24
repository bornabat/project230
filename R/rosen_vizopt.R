#' Visualize Optimization trajectroy for Rosenbrock's Function
#'
#'
#' @param R A numeric value specifying the maximum range of x and y values to visualize.
#' @param cls A vector of length 3 specifying the colors for the minimum and maximum values of the function and the trajectory (default: c('orange', 'steelblue','red')).
#' @param psy A positive integer value specifying how many rows of the optimizer_output to skip before adding a point to the plot (default: 100).
#'
#' @return A Plotly visualization in 3D.
#'
#' @examples
#' data = rosenopt_gd(n.iter=1e6,init=c(2,2))
#' rosen_vizopt(optimizer_output=data,psy=1000,R=1)
#'
#' @export
rosen_vizopt = function(R = 3, cls = c('orange', 'steelblue','red','navy'),optimizer_output,psy=100,main,pnts=TRUE, mag=1.5){
  r = seq(0, R, length.out = 50)
  theta = seq(0, 2*pi, length.out = 100)
  grd = expand.grid(r, theta)
  x = grd[1] * cos(grd[2])
  y = grd[1] * sin(grd[2])
  x= x+1
  y= y+1
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
    marker = list(size = 0.8*mag, color = mat.points[, 3], colorscale = custom_colorscale) # Use the custom colorscale
  )

  fig1 <- fig1 %>% layout(title = "",
                          scene = list(xaxis = list(title = "x"),
                                       yaxis = list(title = "y"),
                                       zaxis = list(title = "Loss")),
                          showlegend = FALSE)


  traj_points = optimizer_output[seq(1, nrow(optimizer_output), psy), ]

  # add points and lines to the plot
  if(pnts){
  fig1 <- fig1 %>% add_markers(x = traj_points[, 1], y = traj_points[, 2], z = traj_points[, 3],
                               mode = "markers",
                               marker = list(size = 1.5, color = cls[3])) %>%
    add_trace(x = traj_points[, 1], y = traj_points[, 2], z = traj_points[, 3],
              type = "scatter3d",
              mode = "lines",
              line = list(color = cls[3], width = 1*mag)) %>%
    add_markers(x = traj_points[1, 1], y = traj_points[1, 2], z = traj_points[1, 3],
                mode = "markers",
                marker = list(size = 2*mag, color = cls[4]))}
  else{
    fig1 <- fig1%>%
      add_trace(x = traj_points[, 1], y = traj_points[, 2], z = traj_points[, 3],
                type = "scatter3d",
                mode = "lines",
                line = list(color = cls[3], width = 1*mag)) %>%
      add_markers(x = traj_points[1, 1], y = traj_points[1, 2], z = traj_points[1, 3],
                  mode = "markers",
                  marker = list(size = 2*mag, color = cls[4]))%>%
      add_markers(x = traj_points[nrow(traj_points), 1], y = traj_points[nrow(traj_points), 2], z = traj_points[nrow(traj_points), 3],
                  mode = "markers",
                  marker = list(size = 2*mag, color = cls[4]))}


  fig1
}
