#' @title residuals vs predictions or time
#' @description plot for residuals vs predictions or time
#' @param df data frame
#' @param y y variable, Default: CWRES
#' @param x x variable, Default: PRED
#' @param absolute use absolute values of residuals?, Default: F
#' @param smooth_method method for smoother, Default: 'loess'
#' @return ggplot object
#' @rdname gg_residuals
#' @export 
#' @importFrom rlang quo expr_text eval_tidy
#' @import ggplot2
gg_residuals <- function(df, y=CWRES, x=PRED,
                         absolute=F,
                         smooth_method="loess"){
  
  y <- enexpr(y)
  x <- enexpr(x)

  y_col <- df[[rlang::expr_text(y)]]

  # Settings for axes
  ymax <- max(abs(y_col), na.rm=T)
  
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x, y=!!y)) +
      geom_hline(aes(yintercept=0), colour="darkgray") + # line at y=0
      geom_hline(yintercept=c(-2, 2), 
                 linetype="dashed", colour="darkgray") + # line at 2 and -2
      geom_point() +
      geom_smooth(method=!!smooth_method) + 
      coord_cartesian(ylim=c(-!!ymax, !!ymax))
  )
  
  if(absolute){
    p <- rlang::quo(
      ggplot(data=df, aes(x=!!x, y=abs(!!y))) +
        geom_point() +
        geom_smooth(method=!!smooth_method) + 
        coord_cartesian(ylim=c(0, !!ymax))
    )
  }
  
  return(rlang::eval_tidy(p))
  
}