#' gg observation vs pred
#' @param df data frame
#' @param y observation variable
#' @param x predicted variable
#' @param grp group variable, Default: NULL
#' @param method statistical method for central tendancy, Default: 'loess'
#' @param labs x and y label names, passed as a list, Default: list(y = "Observed", x = "Predicted")
#' @return ggplot object
#' @export 
#' @importFrom dplyr summarize
gg_obs_vs_pred <- function(df, 
                        y, 
                        x, 
                        grp = NULL, 
                        method="loess", 
                        labs=list(y="Observed", x="Predicted")){
    x <- enexpr(x)
    y <- enexpr(y)
    grp <- enexpr(grp)
  
    # Maxvalue for axes
    maxXY <- rlang::quo(df %>% dplyr::summarize(maxVal=max(c(!!x,!!y), na.rm=T))) %>% 
      eval_tidy()
    maxVal <- maxXY[1,]
    
    p <- 
      rlang::quo(ggplot(data=df, aes(x=!!x, y=!!y)) + 
                   geom_abline(slope=1, intercept=0) +  # line of identity
                   geom_point(aes(shape=factor(BLQ))) +
                   scale_shape_manual(values=c(1, 4)) + guides(shape="none") +
                   geom_smooth(method=method) + 
                   coord_cartesian(xlim=c(0, maxVal), ylim=c(0, maxVal)) +
                   labs(y=labs[["y"]], x=labs[["x"]]))
    return(eval_tidy(p))
}