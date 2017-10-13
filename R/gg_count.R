count_n <- function(x){
  return(c( y=unique(x), label=length(x)))
}

#' proably not working - needs debugging to det the rlang correct... 
#' @title create a counts plot
#' @description count plot
#' @param df dataframe
#' @param x x variable
#' @param y y variable
#' @param mapping aes passed to ggplot
#' @param counts absolute number or relative, see details, Default: 'rel'
#' @param angle angle passed to theme axis.text Default: 35
#' @param hjust_x hjust passed to theme axis.text.x, Default: 1
#' @param vjust_y vjust passed to theme axis.text.y, Default:0
#' @param ... PARAM_DESCRIPTION
#' @return ggplot object
#' @details counts: one of 'abs' or 'rel'. To be expanded.
#' @rdname gg_count
#' @export
#' @importFrom rlang enexpr quo eval_tidy
#' @import ggplot2

gg_count <- function(df, x, y, 
                     counts="abs", 
                     angle = 35, hjust_x=1, vjust_y=0, ...){
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  
  x_col <- df[[expr_text(x)]]
  y_col <- df[[expr_text(y)]]
  

  if(!is.factor(x_col) | !is.factor(y_col)){
    stop(paste("Plot requires categorical input.",
               expr_text(x),"or", expr_text(y), "is not a factor"))
  }
  
  if(counts=="abs"){
    
    # to be updated with only the number
    p <- rlang::quo(
      ggplot(data=df, aes(x=!!x, y=!!y)) +
        geom_count() + scale_size_area() + guides(size = "none") +
        stat_summary(fun.data=count_n, geom = "text", size=3) + 
        theme(axis.text.x = element_text(angle= !!angle, hjust=!!hjust_x),
              axis.text.y = element_text(angle= !!angle, vjust=!!vjust_y))
    )
  } 

  if(counts=="rel"){ 
    
    # update this section with rlang
    
    if(length(levels(x_col)) > 1 | length(levels(y_col))>1){
      
      # handling of NAs in plot
      if(any(is.na(x_col))){
        x_max <- length(levels(x_col))+1
      } else {
        x_max <- length(levels(x_col))
      }
      
      if(any(is.na(y_col))){
        y_max <- length(levels(y_col))+1
      }else {
        y_max <- length(levels(y_col))
      }
      
      # Get the contingency table
      contingency_tab <- table(y_col, x_col, useNA="ifany")
      contingency_tab <- as.data.frame(contingency_tab)
      
      # Summarize for x
      x_col_sum <- contingency_tab %>% 
        group_by(x_col) %>% 
        mutate(total = sum(Freq), 
               relative = ifelse(total!=0, 
                                 round(100*(Freq / total), digits=0), 
                                 0),
               y = ifelse(is.na(y_col),
                          y_max+0.35,
                          as.numeric(y_col)+0.35)) # need to round
      
      # Summarize for y
      y_col_sum <- contingency_tab %>% 
        group_by(y_col) %>% 
        mutate(total = sum(Freq), 
               relative = ifelse(total!=0, 
                                 round(100*(Freq / total), digits=0), 
                                 0),
               x=ifelse(is.na(x_col),
                        x_max-0.35,
                        as.numeric(x_col)-0.35)) # need to round
    } else {
      warning("All data in the same level.")
    }
    
    p <- rlang::quo(
      ggplot(data=df, aes(x=!!x, y=!!y)) +
      geom_point(alpha=0) + # invisible layer to set the axes properly
      
      # add "anchor points" 
      geom_point(data=!!x_col_sum, aes(y=!!y_col, x=!!x_col), colour="gray15",
                 shape=16, size=1.5) +
      
      # Add numbers relative y-axis
      geom_text(data=!!x_col_sum, aes(y=!!y, x=!!x_col, label=relative), 
                size=2.8, colour="gray15") + 
      
      # Add numbers relative x-axis
      geom_text(data=!!y_col_sum, aes(y=!!y_col, x=!!x, label=relative), 
                size=2.8, colour="gray15") +
      
      coord_cartesian(xlim=c(0.75, seq(1, !!x_max, by=1)), 
                      ylim=c(seq(1, !!y_max, by=1), !!y_max+0.25)) + 
      
      theme(axis.text.x = element_text(angle = !!angle, hjust=!!hjust_x),
            axis.text.y = element_text(angle = !!anlge, vjust=!!vjust_y))
      )
  } 
  return(eval_tidy(p))
}
