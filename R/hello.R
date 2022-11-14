
ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}

library(MASS)
x <- ryx(Boston, y="medv")

print.ryx <- function(x, digits = 3){
  if(!inherits(x, "ryx")) {
    stop("This function requires an object type 'ryx'")
  }
  x$df$r <- round(x$df$r, digits)
  x$df$p <- format.pval(x$df$p, digits)
  cat("Correlations of", x$y, "with \n")
  print(x$df, row.names = FALSE)
}

print.ryx(x)

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

summary.ryx(x)

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
#to-do: adjust the scale of the x axis
plot(x)

