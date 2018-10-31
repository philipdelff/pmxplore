
#' @title linear regression with correlation threshold for ggally
#' @description scatter plot with linear regression and correlation threshold to be used with ggally
#' @param data data frame 
#' @param mapping aes passed to ggplot
#' @param cor_method method for correlation estimation, Default: 'pearson'
#' @param cor_use argument to cor, Default: 'pairwise.complete.obs'
#' @param cor_threshold threshold for color switch, Default: 0.4
#' @param cor_color colour for used at color switch, Default: 'red'
#' @param ... PARAM_DESCRIPTION
#' @return ggplot object (passed to ggally)
#' @rdname ally_scatter_lm_cor
#' @export 

ally_scatter_lm_cor <- 
  function(data, mapping, 
           cor_method = "pearson",
           cor_use = 'pairwise.complete.obs',
           cor_threshold=0.4,
           cor_color="red", ...) {
    
    x_col <- data[[deparse(mapping$x)]]
    y_col <- data[[deparse(mapping$y)]]
    
    # Calculate correlation only if there are values different from zero
    # (important for ETA corr plots)
    if(all(x_col==0) | all(y_col==0)){
      correlation <- 0
    } else if (length(unique(x_col))==1 | length(unique(y_col))==1) {
      correlation <- 0 
    } else {
      correlation <- cor(x_col, y_col, 
                         method=cor_method,
                         use=cor_use)
    }
    
    # Set to red colour if more or less than threshold
    if(correlation <= -cor_threshold | correlation >= cor_threshold) {
      point_color <- cor_color
    }else{
      point_color <- "black"
    }
    
    # Plot
    if(!all(x_col==0) & !all(y_col==0)){
      p <- 
        ggplot(data, mapping) + 
        geom_point(colour=point_color) + 
        geom_smooth(method="lm")
    }else{
      p <- ggplot(data, mapping) + 
        geom_point()
    }
    return(p)
  }