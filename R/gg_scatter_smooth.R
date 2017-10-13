#' @title scatter plot with smoother
#' @description scatter plot with smoother
#' @param df data frame
#' @param x x variable
#' @param y y variable
#' @param smooth_method method for smoother, Default: 'loess'
#' @return ggplot object
#' @rdname gg_scatter_smooth
#' @export 
#' @importFrom rlang quo
#' @import ggplot2
gg_scatter_smooth <- function(df, x, y,
                              smooth_method="loess") {
  #needed?
  x <- enexpr(x)
  y <- enexpr(y)
  
  p <- rlang::quo(
    ggplot(df, aes(x=!!x, y=!!y)) + 
    geom_point() + 
    geom_smooth(method=!!smooth_method)
  )
  return(eval_tidy(p))
}
