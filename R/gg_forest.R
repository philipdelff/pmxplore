#' @title forest plot for covariate-parameter impact
#' @description forest plot for covariate-parameter impact
#' @param df data frame
#' @param x x variable
#' @param y y variable
#' @param x_ci_u upper confidence interval for x variable
#' @param x_ci_l lower confidence interval for x variable
#' @param impact_area delimits shaded area, Default: c(0.8, 1.25)
#' @return ggplot object
#' @rdname gg_forest
#' @export 
#' @importFrom rlang enexpr quo
#' @import ggplot2
gg_forest <- function(df, x, y, x_ci_u, x_ci_l,
                      impact_area = c(0.8, 1.25), ...){
  # consider adding control of shape, height nad linetype.
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  x_ci_u <- rlang::enexpr(x_ci_u)
  x_ci_l <- rlang::enexpr(x_ci_l)
  facet <- rlang::enexpr(facet)
  
  p <- rlang::quo(
    ggplot(df, aes(x=!!x, y=!!y)) + 
    geom_rect(xmin = !!impact_area[1], xmax= !!impact_area[2],
              ymin = -Inf, ymax = Inf, 
              fill="lightgray") + 
      geom_point(size=3, shape=18) +
      geom_errorbarh(aes(xmin = !!x_ci_l, xmax=!!x_ci_u), height = 0.2) +
      geom_vline(xintercept=1, linetype = "longdash")
    )
  return(eval_tidy(p))
}
