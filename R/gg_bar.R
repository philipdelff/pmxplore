
#' @title gg_bar
#' @param df data frame
#' @param x name of column to plot
#' @param numbers print count, Default: F
#' @param angle angle passed to theme axis.text.x, Default: 40
#' @param hjust hjust passed to theme axis.text.x, Default: 1
#' @return ggplot object
#' @export 
#' @rdname gg_bar
#' @import ggplot2 
gg_bar <- function(df, x, numbers=F, 
                   angle=40, hjust=1){
  # needed?
  x <- enexpr(x)
  
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x)) + 
      geom_bar() +
      theme(axis.text.x = element_text(angle=!!angle, hjust=!!hjust),
            panel.grid = element_blank()) 
  )
  
  if(numbers){
    p <- rlang::quo(
      ggplot(data=df, aes(x=!!x)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle=!!angle, hjust=!!hjust),
              panel.grid = element_blank()) +
        geom_label(aes(label=..count.., y=(..count..)),
                  stat= "count", fontface="bold"))
    # numbers does not work well in the scenario of many covariates
  }
  return(eval_tidy(p))
}

