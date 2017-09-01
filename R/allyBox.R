
#' @title ally_box
#' @description FUNCTION_DESCRIPTION
#' @param data data frame
#' @param mapping aes passed to ggplot
#' @param angle angle passed to theme axis.text.x, Default: 40
#' @param hjust hjust passed to theme axis.text.x, Default: 1
#' @param x.grid panel grid on x, Default: F
#' @return ggplot object (passed to ggally)
#' @details DETAILS
#' @rdname ally_box
#' @export 
#' @import ggplot2 

ally_box <- function(data, mapping, angle=40, hjust=1, x.grid=F){
  
  xCol <- data[[deparse(mapping$x)]]
  yCol <- data[[deparse(mapping$y)]]
  
  p <- ggplot(data, mapping) + 
    geom_boxplot() +
    theme(axis.text.x = element_text(angle=angle, hjust=hjust))
  
  if(!x.grid){
    p <- p +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  }
  return(p)
}