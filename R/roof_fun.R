#' A mathematical function shaped like a roof
#'
#' Roof-shaped functions to illustrate the properties of partial derivatives.
#'
#' @param x the x-coordinates at which to evaluate the function
#' @param y the y-coordinates at which to evaluate the function
#' @param type one of `"plain"`,  `"dormer"`, or `"silo"`.
#'
#'
#' @details Note that when used in `contour_plot()` or `interactive_plot()`, you
#' need only specify the symbolic expression `roof_fun(x, y) ~ x + y`. The plotting function
#' will calculate and provide the needed numerical values for the coordinate.
#' The domain for this function is `x=c(-3,3)` and `y=c(-1,1)`.
#'
#' @examples
#' \dontrun{
#' interactive_plot(roof_fun(x, y, type="dormer") ~ x & y,  domain(x=c(-3,3), y=c(-1,1)))
#' }
#' @export
roof_fun <- function(x, y,  type=c("plain", "dormer", "silo")) {
  type <- match.arg(type)
  res <-
    ifelse(x < -3  | x > 3,
           0,
           ifelse(y < -1  | y  > 1,
                  0,
                  30.2 - 10 * abs(y)
           )
    )

  if (type == "dormer") {
    res <- ifelse(y > 0 | y < -0.8 | abs(x) > 1.5,
                  res,
                  pmax(29 - 2*abs(x), res))

  }

  if (type == "silo") {
    res <- ifelse(y > 0 | y < -0.8 | abs(x) > 1.5,
                  res,
                  pmax(29 - (x^2 + (2*y)^2), res))

  }

  res
}
