#' Functions for drawing phase-plane dynamics
#'
#' @param dynamics a list containing two one-sided formulas specifying
#' the dynamics, e.g. `list(~ x + y, ~ y - x)`
#' @param n number of arrows to draw on a side. $n^2$ will be the number of arrows
#' overall.
#' @param pseudo Logical flag. Whether to position the arrows pseudo-randomly rather than
#' on a strict grid.
#'
#' @export
flow_field <- function(dynamics, xrange = 0:1, yrange=0:1,
                       n = 10, pseudo=FALSE,
                       nsteps = 10, stepsize=0.05) {
  dynfuns <- make_dynamics(dynamics)
  library(magrittr)
  if (!pseudo) {
    xpts <- seq(min(xrange), max(xrange), length = n+2)
    xpts <- xpts[-c(1, length(xpts))]
    ypts <- seq(min(yrange), max(yrange), length = n+2)
    ypts <- ypts[-c(1, length(ypts))]
    Grid <- expand.grid(xpts, ypts)
    names(Grid) <- c("x", "y")
  } else {
    # not implemented yet
  }

  Flows <- list()
  for (point in 1:nrow(Grid)) {
    x <- y <- numeric(nsteps)
    x[1] <- Grid$x[point]
    y[1] <- Grid$y[point]

    for (k in 2:length(x)) {
      dx = stepsize*dynfuns[[1]](x[k-1], y[k-1])
      dy = stepsize*dynfuns[[2]](x[k-1], y[k-1])
      x[k] <- x[k-1] + dx
      y[k] <- y[k-1] + dy
    }

    Flows[[point]] <- tibble::tibble(x=x, y=y, group=point)
  }
  # Evaluate the dynamics on the grid
  # dx <- dynfuns[[1]](Grid$x, Grid$y) # small step
  # dy <- dynfuns[[2]](Grid$x, Grid$y)
  # normalize <- max( max(abs(dx) + 1/n), max(abs(dy) + 1/n))
  # arrow_lengthx <- abs(diff(xrange))/(n+2)
  #
  # Grid$dx <- dx * arrow_lengthx / normalize
  # Grid$dy <- dy * arrow_lengthx / normalize
  # Grid$endx <- Grid$x + Grid$dx
  # Grid$endy <- Grid$y + Grid$dy

  ggformula::gf_path(y ~ x, data = dplyr::bind_rows(Flows),
                     group = ~ group) %>%
    ggformula::gf_point(y ~ x, data = Grid, inherit=FALSE)
  # ggformula::gf_segment(y + endy ~ x + endx, data = Grid,
  #            arrow =
  #              grid::arrow(length = grid::unit(0.01, "snpc"))) +
  #  ggplot2::labs(x = dynfuns[[3]][1], y = dynfuns[[3]][2])
}


make_dynamics <- function(dynamics) {
  first <- dynamics[[1]]
  second <- dynamics[[2]]
  if (is.function(first) && is.function(second)) {
    return(c(dynamics, names(formals(first))))
  }
  if (!is.list(dynamics) ||length(dynamics) != 2) stop("dynamics must be a list of two formulas")

  if (length(dynamics[[1]]) != 2 || length(dynamics[[2]]) != 2)
    stop("dynamics formulas must be one sided, e.g. ~ x + y")

  vnames <- union(all.vars(dynamics[[1]]), all.vars(dynamics[[2]]))
  if (length(vnames) != 2) stop("dynamics must involve two variables, e.g. x and y")


  # Create the dynamical functions
  dx <- function() {}
  dy <- function() {}
  args <- alist(x = , y = )
  names(args) <- vnames
  formals(dx) <- formals(dy) <- args
  body(dx) <- dynamics[[1]][[2]]
  body(dy) <- dynamics[[2]][[2]]
  return(list(dx, dy, vnames))
}



goo <- flow_field(list(~ u+v, ~ u - v))
