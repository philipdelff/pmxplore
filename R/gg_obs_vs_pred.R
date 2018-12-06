#' gg observation vs pred
#' @param df data frame
#' @param y observation variable
#' @param x predicted variable
#' @param grp group variable, Default: NULL
#' @param method statistical method for central tendancy, Default: 'loess'
#' @param labs x and y label names, passed as a list, Default: list(y = "Observed", x = "Predicted")
#' @return ggplot object
#' @rdname gg_obs_vs_pred
#' @export 
#' @import ggplot2
#' @importFrom rlang expr_text eval_tidy quo enexpr
gg_obs_vs_pred <- function(df, y, x, blq = NULL, 
                           smooth_method="loess"){
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  blq <- rlang::enexpr(blq)
  
  x_col <- df[[rlang::expr_text(x)]]
  y_col <- df[[rlang::expr_text(y)]]
  
  # Maxvalue for axes
  max_val <- max(c(x_col, y_col), na.rm=T)
  
  if(is.null(expr_text(blq))){
    p <- 
      rlang::quo(ggplot(data=df, aes(x=!!x, y=!!y)) + 
                   geom_abline(slope=1, intercept=0) +  # line of identity
                   geom_point() +
                   geom_smooth(method=!!smooth_method) + 
                   coord_cartesian(xlim=c(0, !!max_val), ylim=c(0, !!max_val))
      )
  } else {
    p <- 
      rlang::quo(ggplot(data=df, aes(x=!!x, y=!!y)) + 
                   geom_abline(slope=1, intercept=0) +  # line of identity
                   geom_point(aes(shape=!!blq)) +
                   guides(shape="none") +
                   geom_smooth(method=!!smooth_method) + 
                   coord_cartesian(xlim=c(0, !!max_val), ylim=c(0, !!max_val))
      )
  }
  return(rlang::eval_tidy(p))
}
