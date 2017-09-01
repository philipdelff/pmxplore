#' @title box plot with x-axis spaced as continous
#' @description box plot with the x-axis spaced out as continous instead of categorical
#' @param df data frame
#' @param x x variable (continuous)
#' @param y y variable
#' @param fill fill variable passed to aes, Default: NULL
#' @param dodge position of text, Default: 0
#' @return ggplot object
#' @details x-axis is continous and then grouped, useful for dose-linearity graphics
#' @rdname gg_continuous_box
#' @export 
#' @importFrom rlang quo
#' @import ggplot2
gg_continuous_box <- function(df, x, y, 
                              fill=NULL, dodge=0){
  x <- enexpr(x)
  y <- enexpr(y)
  fill <- enexpr(fill)
  
  x_col <- df[[expr_text(x)]]
  x_breaks <- sort(unique(x_col))
  
  fill_col <- df[[expr_text(fill)]]
  
  if(!is.numeric(x_col)){
    stop(paste(expr_text(x),"is not numeric"))
  }
  
  get_n <- function(x){
    return(c(y = median(x, na.rm=T), label = length(x)))
  }
  
  if(is.null(fill_col)){
    p <- rlang::quo(
      ggplot(data=df, aes(x = !!x, y=!!y)) + 
        geom_boxplot(aes(group= !!x)) +
        stat_summary(aes(y=!!y, x=!!x), fun.data = get_n, 
                     geom = "label", inherit.aes=F,
                     position = position_dodge(!!dodge)) +
        scale_x_continuous(breaks = !!x_breaks) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())
    )
  } else {
    p <- rlang::quo(
      ggplot(data=df, aes(x=!!x, y=!!y)) + 
        geom_boxplot(aes(group=interaction(!!x, !!fill), fill=!!fill)) +
        stat_summary(aes(y=!!y, x=!!x, group=interaction(!!x, !!fill)), 
                     fun.data = get_n, 
                     geom = "label", inherit.aes=F,
                     position = position_dodge(!!dodge)) +
        scale_x_continuous(breaks = !!x_breaks) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())
    )
  }
  return(eval_tidy(p))
}
