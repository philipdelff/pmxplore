
#' @title regression equation and r2 to 
#' @description function to add regression equation and R2 to plot
#' @param lm_fit model object from lm fit
#' @return parsable character string
#' @details copied and modified from:
# https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph
#' @rdname print_lm_eq
#' @export 
#' @import stringr
print_lm_eq <- function(lm_fit){
  
  # Testing for lm object input (there is no is.lm function)
  if(!is.list(lm_fit)){
    stop("Input is not a model object")
  }
  if(is.list(lm_fit) & (! "call" %in% names(lm_fit))){
    stop("Input is not a model object")
  }
  if(is.list(lm_fit) & "call" %in% names(lm_fit)){
    lm_str <- as.character(summary(lm_fit$call))
    if( ! any(str_detect(lm_str, "^lm")) ){
      stop("Input object is not a lm model object")
    }
  }
  
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