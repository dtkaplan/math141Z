#' Simplify a mathematical expression in a function using Ryacas
#'
#' @param fun The function whose body contains a mathematical expression.
#'
#'
#' @export
mcalc_simplify <- function(fun) {
  bod <- body(fun)
  if (is_mathematical_function(bod)) {

    # It's a candidate for simplification
    require(Ryacas)
    for_yac <- paste0("Simplify(", deparse(bod),")")
    body(fun) <- as_r(for_yac)
  }

  fun
}

#' @export
mcalc_D <- function(tilde, ...) {
  mcalc_calc_op(tilde, "D", ...)
}
#' @export
mcalc_antiD <- function(tilde, ...) {
  mcalc_calc_op(tilde, "Integrate", ...)
}

#' @export
mcalc_calc_op <- function(tilde, operation = c("D", "Integrate"), ...) {
  ## NEED TO CALL THIS RECURSIVELY IF THERE ARE MULTIBLE expressions on RHS of tilde
  # Use all.vars(tilde[[3]], unique = FALSE) to get the whole set of
  # "with respect to"

  operation <- match.arg(operation)
  if (length(tilde) != 3)
    stop("Tilde expression needs both left and right sides, e.g. cos(x^2) ~ x")
  left <- tilde[[2]]
  right <- tilde[[3]]
  if  (is_mathematical_function(left) && require("Ryacas")) {
    # need to have Ryacas to do this stuff
    left_as_str <- deparse(left)
    right_as_str <- deparse(right)
    for_yac <- paste(operation, "(",  right_as_str, ")" , left_as_str)

    res <- function() {}
    formals(res) <- eval(parse(text=paste("alist(", right_as_str, "= )")))

    # DO I WANT TO REPLACE THE ABOVE WITH THE MCALC_FORMAL() FUNCTION?

    yac_result <- try(as_r(for_yac), silent = TRUE) # trap any error
    if (inherits(yac_result, "try-error") ||
        any(c("Deriv", "Integrate") %in% all.names(yac_result)))  {
      res <- bail_out_calc_op(operation, tilde, ...)
    } else {
      body(res) <- yac_result
      if (operation == "Integrate") res <- add_C(res) # constant of integration
    }
  } else {
    res <- bail_out_calc_op(operation, tilde, ...)

  }

  add_default_param_values(res, ...)
}

bail_out_calc_op <- function(operation, tilde, ...) {
  if (operation == "Integrate")
    res <- mosaicCalc::antiD(tilde)
  else if (operation == "D")
    res <- mosaicCalc::D(tilde)

  add_default_param_values(res, ...)
}

# need  a way to get a more complete list of allowed functions.

is_mathematical_function <- function(body) {
  operations <- setdiff(all.names(body), all.vars(body))
  all(operations %in%
          c("+", "*", "-", "^", "/", "(", "exp", "cos", "sin", "sqrt",
            "tan", "atan", "acos", "asin", "log"))
}

# create a function with formals defined according to a tile
# expression and defaults in ...

## NEED TO EXTEND THIS SO THAT IT HANDLES AN EXPRESSION????
mcalc_formal <- function(tilde, ...) {
  specials <- c("pi")
  if (length(tilde) != 3)
    stop("Tilde expression needs both left and right sides, e.g. cos(x^2) ~ x")
  left <- tilde[[2]]
  right <- tilde[[3]]
  explicit <- all.vars(right)
  explicit <- explicit[!duplicated(explicit)]
  implicit <- all.vars(left)
  implicit <- implicit[!duplicated(implicit)]
  # keep the ones defined in the tilde RHS at the front
  implicit <- setdiff(implicit, explicit)
  arguments <- setdiff(c(explicit, implicit), specials)
  res <- function(){}
  args <- eval(parse(text =
    paste("alist(", paste(paste(arguments, "="), collapse = ","), ")")))

  formals(res) <- args


  add_default_param_values(res, ...)
}

# MAKE A from_to(fun) that instead of adding a constant of integration, gives a function
# with <from> and <to> arguments.



# add a constant of integration, default 0, for the output of antidifferentiation
# @param fun the function to which the constant of integration is to be added.
add_C <- function(fun) {
  if (is_mathematical_function(body(fun))) {
    args <- formals(fun)
    # add a formal arg for the offset in an antiD.
    possibilities <- c("C", "const", paste0(c("C", "constant", "offset"), 1:100))
    possibilities <- possibilities[!possibilities %in% names(args)]
    if (length(possibilities) == 0) stop("Ran out of names for constant. Sorry.")
    C_name <- possibilities[1]
    args[[C_name]] <- 0
    formals(fun) <- args
    newbody <- parse(text = paste(deparse(body(fun)), "+", C_name))
    body(fun) <- newbody
  }

  fun

}

# add default values for parameters if specified in ...
add_default_param_values <- function(fun, ...) {
  specials <- c("pi")
  param_defaults <- list(...)
  args <- formals(fun)
  # handle special case of primitives or functions with no arguments
  if (is.null(args)) args <- alist()
  for (default in names(param_defaults)) {
    if (!default %in% specials)
      args[[default]] <- param_defaults[[default]]
  }

  formals(fun) <- args

  fun
}

