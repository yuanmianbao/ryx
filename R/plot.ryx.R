#'@title plot.ryx
#'
#'@description
#'This \code{plot.ryx} function can plot the ryx objects.
#'
#'This is wonderful.
#'@export


plot.ryx <- function(x){
  if(!inherits(x, "ryx")) {
    stop("This function requires an object type 'ryx'")
  }

  library(ggplot2)
  x$df$type <- ifelse(x$df$r>0, "positive", "negative")
  ggplot(x$df,
         aes(x = abs(x$df$r),
             y = reorder(x$df$variable, abs(x$df$r)),
             colour = x$df$type))+
    geom_point(size =3)+
    scale_colour_manual(values = c(positive = "blue", negative = "red"))+
    labs(title = paste("Correlation with",
                       x$y),
         colour = "Direction")+
    xlab("Correlation (absolute value)")+
    ylab("Variables")+
    scale_x_continuous(breaks=seq(0,1, by=0.1))+
    theme_light()+
    theme(panel.grid.major.y = element_line(linetype = 0),
          panel.grid.major.x = element_line(linetype = 2))+
    geom_segment(x=0,
                 y = x$df$variable,
                 xend = abs(x$df$r),
                 yend = x$df$variable,
                 color = "light grey",
                 size = 0.3)

}
