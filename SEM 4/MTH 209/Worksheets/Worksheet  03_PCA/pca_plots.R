##########################
## Code to plot factor loadings
##########################

# pc: principal component
# data: needed only for the variable names
plot_loadings <- function(pc, data)
{
  # Sort from large to small
  ord <- order(pc)
  pc_ord <- pc[ord]
  vars_ord <- colnames(X)[ord]
  
  # Make plot now
  # adjust margin
  par(mar = c(4, 8, 3, 1))
  
  # make the box for the plot
  plot(
    pc_ord,
    seq_along(pc_ord),
    pch = 16,
    xlim = range(c(pc_ord, 0)),
    yaxt = "n",
    xlab = "Loading",
    ylab = "",
    main = "PC loadings"
  )
  
  # add the segments
  segments(
    x0 = 0,
    y0 = seq_along(pc_ord),
    x1 = pc_ord,
    y1 = seq_along(pc_ord),
    col = "gray50",
    lwd = 2
  )
  
  # make the y axis with variable names
  axis(
    side = 2,
    at = seq_along(vars_ord),
    labels = vars_ord,
    las = 1
  )
  # return to previous margins
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}


# plot makes bivariate PC plots
# two_pc: matrix with two columns for PC1 and PC2
# data: needed only for variable names
plot_twoPC <- function(two_pc, data)
{
  plot(
    two_pc[, 1], two_pc[, 2],
    type = "n",
    xlab = "PC1",
    ylab = "PC2",
    main = "Loading plot",
    asp = 1
  )
  
  
  abline(h = 0, v = 0, col = "gray")
  
  text(two_pc[, 1], two_pc[, 2], rownames(two_pc), cex = 0.9)
}
