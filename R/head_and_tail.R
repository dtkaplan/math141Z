#' Display head and tail of a long data frame
#'
#' Works just like `head()` or `tail` but shows both the first and last rows
#'
#' @param df  A data frame or data-frame-like object
#' @param n A number specifying the number of rows to show at the top and bottom.
#' @param ntop Overrides `n` to specify the number of rows to show at the top
#' @param nbottom Overrides `n` to specify the number of rows at the bottom
#' @param nmiddle Integer. Whether to show  intermediate row to indicate
#' that there are elided rows
#'
#' @examples
#' head_and_tail(mtcars)
#' mtcars %>% head_and_tail(ntop=2, nbottom=4)
#' mtcars %>% head_and_tail(nmiddle = 3)
#'
#' @export
head_and_tail <- function(df, n=4L, ntop=n, nbottom=n, nmiddle=0L) {
  if ((ntop + nbottom) + ifelse(nmiddle>0, nmiddle, 0) >= nrow(df)) return(df)
  if (nmiddle >= 0) {
    middle = df[1,]
    row.names(middle) <- "..."
    middle[] <- NA
    middle2 <- middle
    row.names(middle2) <- "... " # to avoid duplicate names
    if (nmiddle > 0) {
      start <- round(nrow(df) / 2)
      contents <- df[seq(start, start+nmiddle),]
      middle <- rbind(middle, contents, middle2)
    }
  } else {
    middle = NULL
  }


  top <- head(df, ntop)
  bottom <- tail(df, nbottom)
  rbind(top, middle, bottom)
}
