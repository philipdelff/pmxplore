
#' @title regression equation and r2 to 
#' @description function to add regression equation and R2 to plot
#' @param lm_fit model object from lm fit
#' @return parsable character string
#' @details copied and modified from:
# https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
#' @rdname print_lm_eq
#' @export 

print_lm_eq <- function(lm_fit){
  
  l <- list(a = format(coef(lm_fit)[1], digits = 2),
            b = format(abs(coef(lm_fit)[2]), digits = 2),
            r2 = format(summary(lm_fit)$r.squared, digits = 3));
  
  if (coef(lm_fit)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(R)^2~"="~r2,l)    
  }
  
  return(as.character(as.expression(eq)))
}