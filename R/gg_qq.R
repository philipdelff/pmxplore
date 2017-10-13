#' @title qq plot
#' @description qq plot
#' @param df data frame
#' @param sample name of sample variable, Default: CWRES
#' @param stat theoretical distribution, Default: 'qq'
#' @return ggplot object
#' @rdname gg_qq_plot
#' @export 
#' @importFrom rlang enexpr quo
#' @import ggplot2
gg_qq_plot <- function(df, sample=CWRES, stat="qq"){
  
  sample <- rlang::enexpr(sample)
  
  p <- rlang::quo(ggplot(df, aes(sample=!!sample)) + 
                    geom_abline(slope=1, intercept=0) +  # line of identity
                    geom_point(stat=!!stat))
  
  return(eval_tidy(p))
}