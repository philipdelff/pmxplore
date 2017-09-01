
#' @title ally_bar
#' @description barchart to be used with ggally
#' @param data data frame 
#' @param mapping aes passed to ggplot
#' @param numbers display count or not, Default: F
#' @param angle angle passed to theme axis.text.x, Default: 40
#' @param hjust hjust passed to theme axis.text.x, Default: 1
#' @return ggplot object (passed to ggally)
#' @rdname ally_bar
#' @export 
#' @import ggplot2 

ally_bar <- function(data, mapping, numbers=F, angle=40, hjust=1){
  
  p <- ggplot(data, mapping) + 
    geom_bar() +
    theme(axis.text.x = element_text(angle=angle, hjust=hjust),
          panel.grid = element_blank()) 
  
  if(numbers){
    # numbers does not work well in the scenario of many covariates
    p <- ggplot(data, mapping) + 
      geom_bar() +
      theme(axis.text.x = element_text(angle=angle, hjust=hjust),
            panel.grid = element_blank()) +
      geom_label(aes(label=..count.., y=(..count..)),
                 stat= "count", fontface="bold")
  }
  return(p)
}
