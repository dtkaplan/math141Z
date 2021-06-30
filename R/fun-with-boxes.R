#' Draw a graph with labeled boxes
#'
#' @export
graph_with_boxes <- function(fn,
                             intervals = tibble::tibble(x=-1:1, xend=x+0.6),
                             domain = c(-3,3), my_letters = LETTERS, ...) {
  domain <- list(x=domain)
  intervals <- intervals %>% mutate(y=0, ytop=1)
  for (k in 1:nrow(intervals)) {
    yvals <- fn(seq(intervals$x[k], intervals$xend[k], length=10))
    ylims <- extendrange(range(yvals), f=0.2)
    intervals$y[k] <- ylims[1]
    intervals$ytop[k] <- ylims[2]
  }
  intervals <- intervals %>% mutate(color = rainbow(nrow(.), start=0.6),
                                    label = my_letters[1:nrow(.)])
  slice_plot(fn(x) ~ x, domain=domain, ...) %>%
    gf_rect(y + ytop ~ x + xend, data=intervals,
            color = ~color,
            alpha = 0.2, inherit=FALSE) %>%
    gf_text(ytop ~ I(x+xend)/2, data = intervals, label=~label, color=~color,
            vjust = -.3, fontface="bold") %>%
    gf_refine(scale_color_identity())
}

#' @export
graph_with_tangents <- function(fn,
                             touches = c(-2, 0, 1.5),
                             offsets = c(.9, 1, 1.1),
                             domain = c(-3,3), ...) {
  if (length(touches) != length(offsets)) stop("offsets and touches must be the same length.")
  width <- diff(domain)/(3*length(touches))
  domain <- list(x=domain)
  dfn <- mosaicCalc::D(fn(x) ~ x)
  slopes <- dfn(touches)
  intervals <- tibble(
    xmid = touches,
    xlow = touches - width,
    xhigh = touches + width,
    ymid = fn(touches),
    ylow = ymid - width*offsets*slopes,
    yhigh= ymid + width*offsets*slopes
  ) %>%
    mutate(color = rainbow(nrow(.), start=0.6),
           label = LETTERS[1:nrow(.)])

  slice_plot(fn(x) ~ x, domain=domain, ...) %>%
    gf_segment(ylow + yhigh ~ xlow + xhigh, data=intervals,
            color = ~color, size=3,
            alpha = 0.4, inherit=FALSE) %>%
    gf_text(ymid ~ xmid, data = intervals, label=~label, color=~color,
            vjust = -.8, fontface="bold") %>%
    gf_point(ymid ~ xmid, data = intervals, alpha = 0.5) %>%
    gf_refine(scale_color_identity())
}

