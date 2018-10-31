
#' @title correlation plot for ggally
#' @description correlation plot to be used with ggally
#' @param data data frame
#' @param mapping aes passed to ggplot
#' @param cor_method method for correlation estimation, Default: 'pearson'
#' @param cor_use argument to cor, Default: 'pairwise.complete.obs'
#' @param cor_threshold threshold for color switch, Default: 0.4
#' @param cor_color color used at color switch, Default: 'red'
#' @return ggplot object (passed to ggally)
#' @seealso 
#'  \code{\link[vcd]{assocstats}}
#' @rdname ally_cor
#' @export 
#' @importFrom vcd assocstats
#' @import ggplot2 

ally_cor <- function(data, mapping, 
                     cor_method='pearson',
                     cor_use = 'pairwise.complete.obs',
                     cor_threshold=0.4,
                     cor_color='red', ...) {
  
  x_col <- data[[deparse(mapping$x)]]
  y_col <- data[[deparse(mapping$y)]]
  
  ## If numeric columns 
  if(is.numeric(x_col) & is.numeric(y_col)){
    
    # Calculate correlation only if there are values different from zero
    # (important for ETA corr plots)
    if(length(unique(x_col))==1 | length(unique(y_col))==1){
      correlation <- 0
      cor_df <- data.frame(
        pos_x = (max(x_col, na.rm = TRUE)-min(x_col, na.rm = TRUE))/2,
        pos_y = (max(y_col, na.rm = TRUE)-min(y_col, na.rm = TRUE))/2,
        lab = "Corr:\nNA")
    }else{
      correlation <- cor(x_col, y_col, 
                         method=cor_method,
                         use=cor_use)
      cor_df <- data.frame(
        pos_x = (max(x_col, na.rm = TRUE)-min(x_col, na.rm = TRUE))/2,
        pos_y = (max(y_col, na.rm = TRUE)-min(y_col, na.rm = TRUE))/2,
        lab = paste0("Corr:\n", round(correlation, digits = 3)))
    }
  }
  
  ## If categorical columns 
  if(is.factor(x_col) & is.factor(y_col)){
    # Calculate correlation if more than 1 level
    if(length(levels(x_col)) > 1 & length(levels(y_col))>1){
      
      contingencyTab <- table(y_col, x_col)
      correlation <- vcd::assocstats(contingencyTab)$cramer
      
      # Correct position if vector includes NA
      if(any(is.na(x_col))){
        add_level_x <- 1 
      }else{
        add_level_x <- 0
      }
      if(any(is.na(yCol))){
        add_level_y <- 1 
      }else{
        add_level_y <- 0
      }
      cor_df <- data.frame(
        pos_x = (length(levels(x_col))+add_level_x)/2,
        pos_y = (length(levels(y_col))+add_level_y)/2,
        lab = paste0("Corr:\n", round(correlation, digits = 3)))
    }else{
      correlation <- 0
      cor_df <- data.frame(
        posX = (length(levels(x_col))+add_level_x)/2,
        posY = (length(levels(y_col))+add_level_y)/2,
        lab = "Corr:\n NA")
    }
  }
  
  # Set to red colour if more or less than threshold
  if(correlation <= -cor_threshold | correlation >= cor_threshold) {
    text_colour <- cor_color
  }else{
    text_colour <- "black"
  }
  
  # Plot
  p <- 
    ggplot(data) + 
    geom_text(data=cor_df, 
              aes(x=pos_x, y=pos_y, label=lab), 
              col=text_colour) + 
    theme(panel.grid = element_blank())
  
  return(p)
}
