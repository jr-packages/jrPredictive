#' Convience function for creating boundary plots
#'
#' @param model A caret classification model
#' @param x Data
#' @param y Data
#' @param z Observed values
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param xlim x axis limit
#' @param ylim y axis limit
#' @param jitter Default \code{TRUE}. Should the points be jittered.
#' @importFrom graphics .filled.contour axTicks axis plot.new plot.window
#' @export
boundary_plot = function(model, x, y, z,
                         xlab = all.vars(formula(model))[2],
                         ylab = all.vars(formula(model))[3],
                         jitter = TRUE, xlim = range(x), ylim = range(y)) {

  cols = function(alpha = 255)
    c(rgb(187, 95, 76, maxColorValue = 255, alpha = alpha),
           rgb(114, 155, 87, maxColorValue = 255, alpha = alpha))
  ## Set up a grid for prediction

  x_seq = seq(xlim[1], xlim[2], length.out = 100)
  y_seq = seq(ylim[1], ylim[2], length.out = 100)

  grid = expand.grid(x_seq, y_seq)
  names = all.vars(formula(model))
  colnames(grid) = names[2:3]

  # make the predictions
  predictions = predict(model, grid, type = "prob")
  # turn the predictions into a matrix for a contour plot
  predmat = matrix(predictions[, 2], nrow = 100)

  # Nice par set-up
  op  = par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -0.01,
            cex.axis = 0.9, las = 1)
  on.exit(par(op))


  # Use filled.contour, but need to hack it to avoid the legend
  plot.new()
  plot.window(xlim, ylim, "",
              xaxs = "i", yaxs = "i", asp = NA,
              xlab = xlab, ylab = ylab)
  .filled.contour(x_seq, y_seq, predmat, pretty(range(predmat), 2), cols(100))


  ticks_y = axTicks(2)
  axis(2, ticks_y, ticks_y,
       tick = TRUE,
       lwd = 0,
       lwd.ticks = 1, col = "grey50", col.axis = "grey30")

  ticks_x = axTicks(1)
  axis(1, ticks_x, ticks_x,
       tick = TRUE, lwd = 0,
       lwd.ticks = 1, col = "grey50", col.axis = "grey30")

  # there are few unique combinations of prices,
  # jitter can help see the points
  # points of prices coloured by purchased brand
  if (jitter) {
    x = jitter(x)
    y = jitter(y)
  }
  points(x,
         y,
         bg = cols(225)[z],
         pch = 21, cex = 0.6,
         col = NA)

    invisible()
}
