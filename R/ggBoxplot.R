#' @title boxplot
#' @description boxplot
#' @param df dataframe
#' @param x x variable
#' @param y y variable
#' @return ggplot object
#' @export 
#' @importFrom rlang quo enexpr expr_text
gg_box_plot <- function(df, x, y){
  
  x <- enexpr(x)
  y <- enexpr(y)
  
  xCol <- df[[expr_text(x)]]
  
  if(!is.factor(xCol)){
    stop(paste(expr_text(x),"is not a factor"))
  }
  
  p <- rlang::quo(
    ggplot(data=df, aes(x = !!x, y=!!y)) + 
      geom_boxplot() +
      theme(axis.text.x = element_text(angle=40, hjust=1),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  )
  # consider adding correlation
  return(eval_tidy(p))
}
