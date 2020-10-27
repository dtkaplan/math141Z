#' Draw gradient arrows on a plot made with `mosaicCalc::contour_plot()`
#'
#' @param P The contour plot. Typically, you will pipe the `contour_plot()` command
#' into  `draw_gradient()` and so you can ignore this argument: the piping process will fill
#' it in.
#' @param f the mathematical function of two inputs whose gradient is to be displayed.
#' @param x Numerical value(s) of the x-coordinate at which to evaluate the gradient.
#' @param y Similar to `x`
#' @param scale A numerical fudge factor so that the gradient arrows can be scaled to look neither
#' overlong nor overshort on the graph.
#' @param color Default "red"
#'
#' @details Depending on the relative extent of the x and y axes, the gradient arrows
#' may not appear to the eye orthogonal to the contour lines, even though they are numerically.
#' Use `coord_fixed()` as shown in the example to fix this graphical artifact.
#'
#'
#' @examples
#' \dontrun{
#' library(mosaicCalc)
#' library(mosaic)
#' library(ggformula)
#' landscape <- rfun( ~ x + y, seed = 101)
#' contour_plot(landscape(x, y) ~ x + y, domain(x=c(-5,5), y = c(-5, 5))) %>%
#'   draw_gradient(landscape, x = -5:5, y = 5:-5, scale=1 )  %>%
#'   gf_refine(coord_fixed())
#' }
#' @export
draw_gradient <- function(P = NULL, f, x, y, scale=0.1, color="red"){
  dx_f <- D(f(x,y) ~ x)
  dy_f <- D(f(x,y) ~ y)

  if (length(y) > length(x)) {
    x <- rep(x, length.out = length(y))
  }

  alpha <- .5
  Arrows <- tibble(
    x = x,  y = y,
    dx = dx_f(x=x, y=y), dy = dy_f(x=x, y=y),
    xend = x + scale*alpha*dx,
    yend = y + scale*alpha*dy,
  )

  P %>% gf_segment(y + yend ~ x + xend, data = Arrows,
                   color = color,
                   arrow = arrow(length=unit(0.1, "inches")))
}
