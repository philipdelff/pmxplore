ggBarchart <- function(df, x, numbers=F, ...){
  
  x <- enexpr(x)
  
  p <- rlang::quo(
    ggplot(data=df, aes(x=!!x)) + 
      geom_bar() +
      theme(axis.text.x = element_text(angle=40, hjust=1),
            panel.grid = element_blank()) 
  )
  
  if(numbers){
    p <- rlang::quo(
      ggplot(data=df, aes(x=!!x)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle=40, hjust=1),
              panel.grid = element_blank()) +
        geom_label(aes(label=..count.., y=(..count..)),
                  stat= "count", fontface="bold"))
    # numbers does not work well in the scenario of many covariates
  }
  return(eval_tidy(p))
}
