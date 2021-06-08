#' Draw parallel axes with a linear transform
#'
#' Only handles linear transformations: r*(x-x0)
#' See examples
#' @export
scale_shift <- function(min, max, r, x0=0, nticks=10) {
  tick_height = (max-min)/5
  nudge = (max-min)/15
  Orig <- tibble::tibble(
    horiz = pretty(min:max,
                   n=nticks,
                   min.n=ceiling(.7*nticks)),
    vert = min,
    end = vert + tick_height/2,
  )

  New <- tibble::tibble(
    yvals = pretty(c(r*(min-x0), r*(max-x0)),
                   n=nticks,
                   min.n=ceiling(.7*nticks)),
    horiz = yvals/r + x0,
    vert = max,
    end = vert - tick_height/2,

  )

  gf_line(vert ~ horiz, data = Orig) %>%
    gf_text(vert ~ horiz, label = ~ as.character(horiz), vjust=1, nudge_y = -nudge) %>%
    gf_errorbar(end + vert ~ horiz, width=0) %>%
    gf_line(vert ~ horiz, data = New, color="red") %>%
    gf_text(vert ~ horiz, label = ~ as.character(yvals),
            data = New, vjust=0, nudge_y = nudge,
            color="red") %>%
    gf_errorbar(end + vert ~ horiz, width=0, data=New,
                color="red") %>%
    gf_theme(theme_void()) %>%
    gf_refine(coord_fixed(ratio = 0.1)) %>%
    gf_lims(
      y= extendrange(
        r=c(min-tick_height, max+tick_height),
        1))
}


#' @export
add_scale <- function(tilde, domain, r, x0, nticks=10) {
  F <- makeFun(tilde)
  Pts <- tibble::tibble(
    xpts = seq(domain[[1]][1], domain[[1]][2], length=500),
    ypts = F(xpts)
  )
  new_range <- range(Pts$ypts)
  yplace <- new_range[1] + 0.02*diff(new_range)
  tick_height <- 0.05*diff(new_range)
  nudge <- 1*tick_height
  New <- tibble::tibble(
    yvals = pretty(r*(range(Pts$xpts)-x0),
                   n=nticks,
                   min.n=ceiling(.7*nticks)),
    horiz = yvals/r + x0,
    vert = yplace,
    end = vert + tick_height/2,
  )

  gf_line(vert ~ horiz, data = New, color="red") %>%
    gf_text(vert ~ horiz, label = ~ as.character(yvals),
            data = New, vjust=0, nudge_y = nudge,
            color="red") %>%
    gf_errorbar(end + vert ~ horiz, width=0, data=New,
                color="red") %>%
    slice_plot(tilde, domain)
}
#' @examples
#' scale_shift(-40, 100, 9/5, 32, 10 )
#' add_scale(exp(x) ~ x, domain(x=c(-2,2)), 2.4, 1)
