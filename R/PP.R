#' Functions for drawing phase-plane dynamics
#'
#' @param dynamics a list containing two one-sided formulas specifying
#' the dynamics, e.g. `list(~ x + y, ~ y - x)`
#' @param domain list of two vectors specifying domain of the initial
#' points of each streamline Vector of length 2 specifying the domain
#' @param n number of arrows to draw on a side. $n^2$ will be the number of arrows
#' overall.
#'
#' @examples
#' streamlines(list(~ u+v, ~ u - v), stepsize=0.01, n=20)
#'
#' @export
streamlines <- function(dynamics, domain=list(x=c(0,1), y=c(0,1)),
                       n = 10,
                       nsteps = 10, stepsize=0.05) {
  dynfuns <- make_dynamics(dynamics)
  library(magrittr)
  xrange <- domain[[1]]
  xstep <- 0.1*diff(xrange)/nsteps # for random displacement of starting points
  yrange <- domain[[2]]
  ystep <- 0.1*diff(yrange)/nsteps
  xpts <- seq(min(xrange), max(xrange), length = n+2)
  xpts <- xpts[-c(1, length(xpts))]
  ypts <- seq(min(yrange), max(yrange), length = n+2)
  ypts <- ypts[-c(1, length(ypts))]
  Grid <- expand.grid(xpts, ypts)
  names(Grid) <- c("x", "y")
  Grid$x <-Grid$x + runif(nrow(Grid),min=-xstep, max=xstep)
  Grid$y <-Grid$y + runif(nrow(Grid),min=-ystep, max=ystep)

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

    Flows[[point]] <- tibble::tibble(x=x, y=y,
                                     group=point,
                                     alpha = 0.2+(1:nsteps)/(1.2*nsteps),
                                     size = alpha + 0.6
                                     )
  }
  Paths <- dplyr::bind_rows(Flows)
  # get the last point in the path
  Last <- Paths %>% group_by(group) %>%
    filter(row_number() == n())

  ggformula::gf_path(y ~ x, data = Paths,
                     lineend = "round",
                     group = ~ group, color="black", alpha=~ alpha, size= ~ size) %>%
    #ggformula::gf_point(y ~ x, data = Last, shape=23, fill="black", inherit=FALSE) %>%
    ggformula::gf_labs(x = names(domain)[1], y = names(domain)[2]) %>%
    gf_refine(scale_alpha_identity(),
              scale_size_identity())

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

