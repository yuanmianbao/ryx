#'@title print.ryx
#'
#'@description
#'This \code{print.ryx} function can print the ryx objects.
#'
#'This is wonderful.
#'@export
#'@examples
#'\dontrun{
#'print.ryx(myryx, digits = 3)
#'}


print.ryx <- function(x, digits = 3){
  if(!inherits(x, "ryx")) {
    stop("This function requires an object type 'ryx'")
  }
  x$df$r <- round(x$df$r, digits)
  x$df$p <- format.pval(x$df$p, digits)
  cat("Correlations of", x$y, "with \n")
  print(x$df, row.names = FALSE)
}
