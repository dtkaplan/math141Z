#' Rules for differentiation
#'
#' Each `_rule` function checks the input expression for a particular kind of structure.
#' If that structure is found, the function returns an expression for the **derivative**.
#' But if the pattern sought is not found, the function returns FALSE.
#'
#' @name differentiation_rules
#'
#' @details
#' Currently, these differentiate with respect to `x`
#'
#'
#' @param X an R expression such as `a*x + b`
#'
#'
#' @examples
#' x_rule(x)
#' axb_rule(x)
#' axb_rule(a*x + 7)

library(rlang)


#' @export
x_rule <- function(X) {
  is_x(substitute(X))
}
#' @export
ax_rule <- function(X) {
  is_ax(substitute(X))
}
#' @export
axb_rule <- function(X) {
  is_axb(substitute(X))
}
#' @export
bmf_rule <- function(X){
  is_basic(substitute(X))
}
#' @export
chain_rule <- function(X) {
  is_fx(substitute(X))
}
#' @export
sum_rule <- function(X) {
  is_sum(substitute(X))
}
#' @export
product_rule <- function(X) {
  is_prod(substitute(X))
}
#' @export
power_rule <- function(X) {
  is_pow(substitute(X))
}

yes <- function(X) {
  if (is.null(X)) return(FALSE)
  if (is.logical(X)) return(X)
  if (is.list(X)) return(TRUE)
  is.numeric(X) || is.name(X) || is.call(X)
}
no  <- function(X) !yes(X)
is_number <- function(X) {
  length(all.vars(X)) == 0
}
as_number <- function(X) {
  if (is_number(X)) return(eval(X))
  else FALSE
}
is_zero <- function(X) {
  yes(as_number(X)) && X==0
}
has_x <- function(X) {
  "x" %in% all.vars(X)
}
negation <- function(X) {
  res <- quote(- y)
  res[[2]] <- X

  return(res)
}
log_prime <- function(X) {
  form <- quote(1/x)
  form[[3]] <- X
  return(form)
}
log10_prime <- function(X){
  form <- quote(1/(log(10)*x))
  form[[3]][[2]][[3]] <- X
  return(form)
}
log2_prime <- function(X){
  form <- quote(1/(log(2)*x))
  form[[3]][[2]][[3]] <- X
  return(form)
}


ddnorm <- function (x, m=0, s=1) {
  -(exp(-((x - m)^2)/(2 * s^2)) * (2 * (x - m)/(2 * s^2)))/sqrt(2*pi*s^2)
}
tan_prime <- function(X) {
  form <- quote(1/cos(a)^2)
  form[[3]][[2]][[2]] <- X
  form
}
sec <- function(x) {
  1/cos(x)
}

sqrt_prime <- function(X) {
  form <- quote(0.5/sqrt(X))
  form[[3]][[2]] <- X
  form
}

sec_prime <- function(X) {
  form <- quote(tan(a)/cos(b))
  form[[3]][[2]] <- X
  form[[2]][[2]] <- X
  form
}
cos_prime <- function(X) {
  form <- quote(-sin(x))
  form[[2]][[2]] <- X
  return(form)
}
tanh_prime <- function(X) {
  form <- quote(1-tanh(x)^2)
  form[[3]][[2]][[2]] <- X
  return(form)
}

is_x <- function(X) {
  if(is.name(X) && "x" == X) return(1)
  else return(FALSE)
}
is_ax <- function(X) {
  # remove parens
  if (length(X) == 2 && X[[1]] == "(") X <- X[[2]]
  # handles length-1 patterns
  if (!has_x(X)) return(FALSE)
  pat <- is_x(X)
  if (pat) return(pat)

  # Unary + and -
  if (length(X) == 2) {
    pat <- is_x(X[[2]])
    if (yes(pat)) {
      if (X[[1]] == "-") return(-1)
      if (X[[1]] == "+") return(1)
      return(FALSE) # shouldn't get here
    }
    else return(FALSE)
  }

  # handles length-3 patterns
  if (X[[1]] == "*") {
    if (is_x(X[[2]]) && !is_x(X[[3]])) return(do_arith(X[[3]]))
    if (!is_x(X[[2]]) && is_x(X[[3]])) return(do_arith(X[[2]]))
    return(FALSE)
  }
  # if (X[[1]] == "+") {
  #   pat <- is_sum(X)
  #   if (no(has_x(pat))) return(pat)
  # }
  return(FALSE)

}
is_axb <- function(X) {
  # remove parens
  if (length(X) == 2 && X[[1]] == "(") X <- X[[2]]
  if (no(has_x(X))) return(0)
  pat <- is_x(X)
  if (yes(pat)) return(pat)
  pat <- is_ax(X)
  if (yes(pat)) return(pat)
  pat1 <- is_ax(X[[2]])
  if (length(X) == 2) {
    if (X[[1]] == "+") return(pat1)
    if (X[[1]] == "-") {
      form <- quote(- a)
      form[[2]] <- pat1
      return(form)
    }
    return(FALSE)
  }
  pat2 <- is_ax(X[[3]])
  if (X[[1]] == "+") {
    if (yes(pat1)) {
      if (no(pat2)) return(pat1)
      else return(FALSE)
    } else if (yes(pat2)) {
      if (no(pat1)) return(pat2)
      else return(FALSE)
    } else {
      return(FALSE)
    }
  } else if (X[[1]] == "-") {
    return(negation(X[[2]]))
  } else {
    return(FALSE)
  }
}

# a special case, for the basic modeling functions
is_basic <- function(X) {
  if (length(X) == 1) {
    if(no(has_x(X))) return(0)
    else return(is_x(X))
  }
  if (length(X) >= 3) {
    pat2 <- is_axb(X[[2]])
    if (no(pat2)) return(FALSE)
    pat3 <- has_x(X[[3]])
    if (yes(pat3)) return(FALSE)
    pat4 <- is_pow(X)
    if (yes(pat4)) return(pat4)
    return(is_fx(X))
  }
  # for length 2
  pat <- is_axb(X[[2]])
  if (no(pat)) return(FALSE)
  return(is_fx(X))
}

# negate <- function(x) -x
unknown <- function(x) {warning("Unknown function.")}
ncos <- function(x) - cos(x)


## Special cases

is_pow <- function(X) {
  if (length(X) == 3 && as.character(X[[1]]) %in% c("pow", "^")) {
    if (no(has_x(X[[2]])) && yes(has_x(X[[3]]))) { # it's an exponential!
      form <- quote(exp(log(a) * x))
      form[[2]][[2]][[2]] <- X[[2]]
      form[[2]][[3]] <- X[[3]]
      return(is_fx(form))
    }
    if (has_x(X[[3]])) {
      return(FALSE) # for now
      # in the form x^x
    }
    p <- do_arith(X[[3]])
    pm1 <- quote(x - 1)
    pm1[[2]] <- X[[3]]
    pm1 <- do_arith(pm1)
    if (is_zero(pm1)) return(do_D(X[[2]]))
    if (is_number(pm1) && (pm1 == -1)) return(0)
    if (is_number(pm1) && pm1 == 1) {
      form <- quote(2*x)
      form[[3]] <- X[[2]]
    } else {
      form = quote(a*x^p)
      form[[2]] <- p
      form[[3]][[2]] <- X[[2]]
      form[[3]][[3]] <- pm1
    }

    if (is_x(X[[2]])) return(form)

    form2 <- quote(a * b)
    gprime <- do_arith(do_D(X[[2]]))
    if (is_number(gprime)) {
      if (gprime == 1) return(form)
      if (gprime == 0) return(0)
    } else {
      form2[[2]] <- form
      form2[[3]] <- gprime
    }

    return(form2)
  }
  return(FALSE)
}


is_fx <- function(X) {
  rules <- list(sin = quote(cos),
                ncos = quote(sin),
                exp=quote(exp),
                pnorm=quote(dnorm),
                dnorm=quote(ddnorm),
                sinh=quote(cosh),
                cosh=quote(sinh),
                # provide support for basic example functions
                f=quote(dx_f),
                g=quote(dx_g),
                h=quote(dx_h),
                dx_f=quote(dxx_f),
                dx_g=quote(dxx_g),
                dx_h=quote(dxx_h),
                dxx_f=quote(dxxx_f),
                dxx_g=quote(dxxx_g),
                dxx_h=quote(dxxx_h)
  )
  composed <- list(
    tan=tan_prime,
    cos=cos_prime,
    sec=sec_prime,
    tanh=tanh_prime,
    log=log_prime,
    log10=log10_prime,
    log2=log2_prime,
    sqrt=sqrt_prime
  )

  pat <- is_axb(X)
  if (yes(pat)) return(pat)

  ## Handle exponentiation separately
  pat <- is_pow(X)
  if (yes(pat)) return(pat)

  fun <- X[[1]] # it has to be a function, since it's not is_axb().
  first <- X[[2]]
  if (length(X) > 2) {
    rest <- lapply(X[-c(1,2)], has_x)
    if (any(unlist(lapply(rest, yes)))) return(FALSE)
  }
  if (!is.name(fun) || !has_x(first)) {
    warning("I think we shouldn't ever get here")
    return(FALSE)
  }



  newX <- X
  fname <- as.character(fun)
  if (fname %in% names(rules)) {
    newf <- rules[[fname]]
    newX[[1]] <- newf
  } else if (fname %in% names(composed)) {
    pattern_fun <- composed[[fname]]
    newX <- pattern_fun(X[[2]])
  } else {
    return(FALSE)
  }



  # Now the chain rule part of things
  if (no(has_x(first))) return(0) #shouldn't get here but just in case
  if (yes(is_x(first))) return(newX)
  pat <- is_axb(first)
  if (is_number(pat) && pat == "1") return(newX)
  if (no(pat)) pat <- do_D(first)

  form = quote(a * b)
  form[[2]] = pat
  form[[3]] = newX

  return(form)
}

do_arith <- function(X) {
  # remove parens
  if (length(X) == 2 && X[[1]] == "(") X <- X[[2]]
  if (is_number(X)) return(eval(X))
  if (length(X) == 2 && X[[1]] == "+")
    return(X[[2]]) # monatic +
  if (length(X) == 3) {
    if(X[[1]] == "+") {
      if (X[[3]] == 0) return(X[[2]])
      else if (X[[2]] == 0) return(X[[3]])
    }
    if(X[[1]] == "*") {
      if (X[[3]] == 1) return(X[[2]])
      else if (X[[2]] == 1) return(X[[3]])
    }
    if (X[[1]] == "/") {
      if (is_zero(X[[2]])) return(0)
      if (is_number(X[[2]]) && X[[2]] == 1) {
        form = quote(a ^ (-1))
        form[[2]] <- X[[3]]
        return(form)
      }
      if (is_number(X[[2]])) {
        form = quote(a * x^(-1))
        form[[3]][[2]] <- X[[3]]
        form[[2]] <- do_arith(X[[2]])
        return(form)
      }
    }
  }

  return(X)
}


do_D <- function(X) {
  # clean up parentheses and numbers
  X <- do_arith(X)
  if (!has_x(X)) return(0)

  pat <- is_axb(X)
  if (yes(pat)) return(pat)
  pat <- is_fx(X)
  if (yes(pat)) return(pat)
  pat <- is_sum(X)
  if (yes(pat)) return(pat)
  pat <- is_prod(X)
  if (yes(pat)) return(pat)
  pat <- is_pow(X)
  if (yes(pat)) return(pat)


  form <- quote(unknown(X))
  form[[2]] <- X
  return(form)
}

is_sum <- function(X) {
  val <- do_arith(X)
  if (is.numeric(val)) return(0)
  if (length(X) != 3 || (X[[1]] != "+" && X[[1]] != "-")) return(FALSE)
  form <- quote(a + b)
  if (X[[1]]=="-") form[[1]] = `-`

  pat1 <- do_D(X[[2]])
  pat2 <- do_D(X[[3]])
  if (no(pat1)) {
    if (no(pat2)) return(0)
    else return(do_arith(pat2))
  } else {
    if (no(pat2)) return(do_arith(pat1))
    else {
      form[[2]] = pat1
      form[[3]] = pat2
      return(do_arith(form))
    }
  }
  warning("Shouldn't get here.")
  return(FALSE)
}

is_prod <- function(X) {
  val <- do_arith(X)
  if (is.numeric(val)) return(val)
  if (length(X) != 3 || X[[1]] != "*") return(FALSE)
  first <- do_arith(X[[2]])
  second <- do_arith(X[[3]])
  if (is_zero(first) || is_zero(second)) return(0)
  pata <- has_x(first)
  patb <- has_x(second)
  pat1 <- do_D(first)
  pat2 <- do_D(second)
  form <- quote(a * b)
  if (no(pata)) {
    form[[2]] <- first
    form[[3]] <- pat2
    return(form)
  }
  if (no(patb)) {
    form[[2]] <- second
    form[[3]] <- pat1
    return(form)
  }
  form <- quote(A + B)
  form3 <- form2 <- quote(a * b)
  form2[[2]] <- second
  form2[[3]] <- pat1
  form[[2]] <-  do_arith(form2)
  form3[[2]] <- first
  form3[[3]] <- pat2
  form[[3]] <- do_arith(form3)

  return(do_arith(form))
}
