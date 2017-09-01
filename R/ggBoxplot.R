#' @title boxplot
#' @description boxplot
#' @param df dataframe
#' @param x x variable
#' @param y y variable
#' @param angle angle passed to theme axis.text.x, Default: 40
#' @param hjust hjust passed to theme axis.text.x, Default: 1
#' @param x.grid panel grid on x, Default: F
#' @return ggplot object
#' @export 
#' @importFrom rlang quo enexpr expr_text
gg_box_plot <- function(df, x, y, 
                        angle=40, 
                        hjust=1, 
                        x.grid=F){
  #needed?
  x <- enexpr(x)
  y <- enexpr(y)
  
  x_col <- df[[expr_text(x)]]
  
  if(!is.factor(x_col)){
    stop(paste(expr_text(x),"is not a factor"))
  }
  
  p <- rlang::quo(
    ggplot(data=df, aes(x = !!x, y=!!y)) + 
      geom_boxplot() +
      theme(axis.text.x = element_text(angle=!!angle, hjust=!!hjust)))
  
  p <- eval_tidy(p)
  
  if(!x.grid){
    p <- p + 
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  }

  return(eval_tidy(p))
}
