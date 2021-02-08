#' Iterate a function on an initial condition
#'
#' Iterates a function a specified number of times on an initial condition.
#'
#' @details The function `f` can take one or more arguments. The first of these
#' should represent the *state* of the dynamical system, e.g. x or x and y, etc.
#' At the end of the argument list to `f` can come named parameters. The length of the
#' initial condition `x0` must match the number of state arguments. Numerical values for
#' parameters (if any) must be provided in the `...` slot.
#'
#' @param f a function of one or more state variables which returns a vector containing
#' those same state variables. Parameters to `f()`, if any, should be named and at
#' the end of the argument list. State variables should **not** have default values.
#' @param x0 a vector with the numerical initial condition. There should be 1 component
#' in `x0` for each of the state variables in `f()`.
#' @param n an integer specifying the number of iterations
#' @param fargs list containing values for numerical parameters to the function `f()`
#'
#' @return A data frame with a column `.i` listing the iteration number
#' and columns for each component of the initial condition. There will be n+1
#' rows, the first for the initial condition and the remaining for the n iterations.
#'
#' @examples
#' iterate(function(x, mu=3.5) mu*x*(1-x), .232, n=10, list(mu=4)) # chaos
#' iterate(function(x, y) c(x+y, x-y), c(1,1), n=10)
#' iterate(function(x, y) c(x+y, x), c(0,1), n=10) # fibonacci
#' @export
Iterate <- function(f=cos, x0=0, n=10, fargs=list()) {
  if (inherits(f, "formula")) f <- makeFun(f)
  vnames <- names(formals(f))
  has_default <- sapply(formals(f), class) != "name"
  if (is.null(vnames)) vnames <- "x"
  if ((length(x0)+ length(fargs)) < length(vnames) - sum(has_default))
    stop("x0 must be a vector with one element for each argument to f.")
  res <- matrix(0, nrow=n+1, ncol=length(x0))
  res[1,] <- x0
  for (k in 2:(n+1)) res[k,] <- do.call(f, c(as.list(res[k-1,]), fargs))
  out <- cbind(0:n, as.data.frame(res))
  names(out) <- c(".i", setdiff(vnames, vnames[has_default]))

  out
}
