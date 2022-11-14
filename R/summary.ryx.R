#'@title summary.ryx
#'
#'@description
#'This \code{summary.ryx} function can summarize the ryx objects for you.
#'
#'This is wonderful.
#'@export
#'#'@examples
#'\dontrun{
#'summary.ryx(myryx, digits = 3)
#'}

summary.ryx <- function(x, digits = 3){
  if(!inherits(x, "ryx")) {
    stop("This function requires an object type 'ryx'")
  }
  text <- c(
    "Correlating",
    x$y,
    "with",
    x$x
  )
  text2 <- c(
    "The median absolute correlation was",
    round(median(abs(x$df$r)), digits),
    "with a range from",
    round(min(x$df$r),digits),
    "to",
    round(max(x$df$r),digits)
  )
  text3 <- c(
    nrow(x$df[x$df$p<0.05,]),
    "out of",
    nrow(x$df),
    "variables where significant at the p < 0.05 level."
  )

  cat(text,
      "\n",
      text2,
      "\n",
      text3)
}
