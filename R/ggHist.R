#' @title histogram
#' @param df dataframe
#' @param x x variable
#' @return ggplot object
#' @rdname gg_histogram
#' @export 
gg_histogram <- function(df, x){
  
  x <- enexpr(x)
  
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x)) + 
      geom_histogram()
  )
  
  return(eval_tidy(p))
}
