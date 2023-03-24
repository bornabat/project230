library(ggplot2)

rosen_contour = function(R = .825, cls = c('brown1', 'steelblue'), rseq=0.05) {
  x_seq = seq(-R, R, rseq) +1
  y_seq = seq(-R, R, rseq) +1
  z_grid = outer(x_seq, y_seq, Vectorize(rosenbrock))
  z_grid = log(z_grid + 1)

  # Create a dataframe for ggplot2
  df = data.frame(expand.grid(x = x_seq, y = y_seq), z = as.vector(z_grid))

  # Create a 2D filled contour plot with contour lines using ggplot2
  fig2 <- ggplot(df, aes(x = x, y = y, z = z)) +
    geom_raster(aes(fill = z), interpolate = TRUE) +
    scale_fill_gradientn(colors = cls) +
    geom_contour(aes(colour = ..level..), size = 0.5) +
    scale_color_gradientn(colors = 'black') +
    ggtitle("") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "right") +
    xlab("x") +
    ylab("y")

  fig2
}

library(ggplot2)

rosen_contour = function(R = .825, cls = c('brown1', 'steelblue'), rseq=0.05) {
  x_seq = seq(-R, R, rseq) +1
  y_seq = seq(-R, R, rseq) +1
  z_grid = outer(x_seq, y_seq, Vectorize(rosenbrock))
  z_grid = log(z_grid + 1)

  # Create a dataframe for ggplot2
  df = data.frame(expand.grid(x = x_seq, y = y_seq), z = as.vector(z_grid))

  # Create a 2D filled contour plot with contour lines using ggplot2
  fig2 <- ggplot(df, aes(x = x, y = y, z = z)) +
    geom_raster(aes(fill = z), interpolate = TRUE) +
    scale_fill_gradientn(colors = cls) +
    geom_contour(aes(colour = ..level..), size = 0.5) +
    scale_color_gradientn(colors = 'black') +
    ggtitle("") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "right") +
    xlab("x") +
    ylab("y")

  fig2
}

rosen_contour_with_trajectory = function(R = .825, trajectory=NULL, init=NULL) {
  x_seq = seq(-R, R, 0.05) + 1
  y_seq = seq(-R, R, 0.05) + 1
  z_grid = outer(x_seq, y_seq, Vectorize(rosenbrock))
  z_grid = log(z_grid + 1)
  # Create a dataframe for ggplot2
  df = data.frame(expand.grid(x = x_seq, y = y_seq), z = as.vector(z_grid))

  # Create a 2D contour plot with ggplot2
  fig2 <- ggplot(df, aes(x = x, y = y)) +
    geom_contour(aes(z = z, colour = ..level..), size = .75) +
    scale_color_gradientn(colors = adjustcolor('black',alpha=.5)) +
    ggtitle("") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          panel.grid.major = element_line(adjustcolor("steelblue", alpha = 0.75/2),size = .5),
          panel.grid.minor = element_line(adjustcolor("steelblue", alpha = 0.25/2),size = .5),
          panel.background = element_rect(fill = "gray98"),
          plot.background = element_rect(fill = "gray98", color = "white", size = 1),
          legend.position = "none")  +
    xlab("x") +
    ylab("y")

  # Add trajectory as a red line
  if (!is.null(trajectory)) {
    fig2 <- fig2 + geom_path(data=data.frame(x=trajectory[,1], y=trajectory[,2]), aes(x=x, y=y), color="red", size=1)
  }
  if (!is.null(init)) {
    fig2 <- fig2 + geom_point(data=data.frame(x=init[1], y=init[2]), aes(x=x, y=y), shape=23, size=2.5, fill="goldenrod")
  }

  fig2
}
