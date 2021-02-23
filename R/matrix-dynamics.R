#' Draw diagram of the flow induced by a matrix
#'
#' @export
show_matrix_dynamics <- function(A) {
  Segs <- matrix_on_grid(1.5*A, npts=15) %>%
    mutate(xend = x + 0.1* (xend - x),
           yend = y + 0.1* (yend - x))
  gf_segment(y + yend ~ x + xend, data = Segs) %>%
    gf_point(yend ~ xend, alpha=0.3) %>%
    gf_refine(coord_fixed(xlim=c(-1,1), ylim=c(-1, 1), clip = "on"))
}
# helper function
matrix_on_grid <- function(A, domain = c(-1, 1), npts=11) {
  if (!identical(dim(A), c(2L,2L))) stop("Matrix A must be 2x2.")
  if (length(domain) != 2) stop("domain should be a range vector with two elements.")

  pts <- seq(domain[1], domain[2], length=npts)
  Start <- expand.grid(pts, pts) %>% tibble() %>%
    rename(x = "Var1", y = "Var2")
  End <- t(A %*% t(Start)) %>% as_tibble()
  names(End) <- c("xend", "yend")

  bind_cols(Start, End)
}


