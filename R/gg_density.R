#' @title density plot
#' @description density function with or without overlaying distribution
#' @param df data frame
#' @param x x variable
#' @param n_bins number of bins passed to geom_freqpoly, Default: 6
#' @param with_est PARAM_DESCRIPTION, Default: F
#' @param dist pdf function for overlaying distribution, Default: NULL
#' @param var estimated variance of the overlaying distribution, Default: NULL
#' @return ggplot object
#' @details DETAILS
#' @rdname gg_density
#' @export 
#' @importFrom rlang enexpr quo
#' @import ggplot2
gg_density <- function(df, x, n_bins=6,
                       with_est=F, dist=NULL, var=NULL){
  
  x <- rlang::enexpr(x)
  n_bins <- rlang::enexpr(n_bins)
  dist <- rlang::enexpr(dist)
  var <- rlang::enexpr(var)
  
  if(with_est){
    p <- rlang::quo(
      ggplot(df) + 
        # add distribution with estimated pars
        stat_function(fun=!!dist, args = list(sd=sqrt(!!var)), 
                      linetype="dashed", col="#898989") +
        geom_freqpoly(aes(!!x, ..density..), bins=!!n_bins)
    )
  } else {
    p <- rlang::quo(
      ggplot(df) + geom_freqpoly(aes(!!x, ..density..), bins=!!n_bins)
    )
  }

  return(eval_tidy(p))
}
