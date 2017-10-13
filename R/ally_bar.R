
#' @title barchart for ggally
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
  
  # mapping should be a x variable and should not contain y variable
  if("y" %in% names(mapping)){
    stop("stat_count() must not be used with a y aesthetic.")
  }
  if(!"x" %in% names(mapping)){
    stop("ally_bar requires an x variable")
  }
  
  # test for factor input
  x_col <- data[[deparse(mapping$x)]]
  if(!is.factor(x_col)){
    warning(paste(deparse(mapping$x), " is not a factor."))
  }

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
