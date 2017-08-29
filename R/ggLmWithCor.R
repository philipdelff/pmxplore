#' @title linear regression with correlation
#' @param df dataframe
#' @param x x variable
#' @param y y variable
#' @param corThreshold correlation threshold, Default: 0.4
#' @param corMethod correlation method, Default: 'pearson'
#' @return ggplot object
#' @rdname gg_lm_with_cor
#' @export 
gg_lm_with_cor <- function(df, x, y, 
                      corThreshold=0.4,
                      corMethod = "pearson", ...) {
  x <- enexpr(x)
  y <- enexpr(y)
  
  xCol <- df[[expr_text(x)]]
  yCol <- df[[expr_text(y)]]
  
  # Calculate correlation only if there are values different from zero
  # (important for ETA corr plots)
  if(all(xCol==0) | all(yCol==0)){
    cor <- 0
  }else{
    cor <- cor(xCol, yCol, 
               method=corMethod,
               use="pairwise.complete.obs")
    corDf <- data.frame(
      posX = min(xCol, na.rm = TRUE),
      posY = max(yCol, na.rm = TRUE),
      lab = round(cor, digits = 3))
  }
  # Set to red colour if more or less than threshold
  if(cor <= -corThreshold | cor >= corThreshold) {
    textColour <- "red" 
  }else{
    textColour <- "black"
  }
  # Plot
  if(!all(x==0) & !all(y==0)){
    p <- rlang::quo(
      ggplot(df, aes(x=!!x, y=!!y)) + 
        geom_point() + 
        geom_smooth(method="lm") + 
        geom_label(data=corDf, 
                   aes(x=posX, y=posY, label=lab), 
                   hjust = 0, vjust = 1, fontface = "bold", col=textColour)
    )
  }else{
    p <- rlang::quo(
      ggplot(df, aes(x=!!x, y=!!y)) + 
        geom_point()
    )
  }
  return(eval_tidy(p))
}
