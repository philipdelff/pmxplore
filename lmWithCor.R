lmWithCor <- function(data, mapping, corThreshold = 0.4, ...) {
  
  x <- data[[deparse(mapping$x)]]
  y <- data[[deparse(mapping$y)]]
  
  # calculate correlation only if there are values different from zero (important for ETA corr plots)
  if(all(x==0)){
    cor <- 0  }
  if(all(y==0)){
    cor <- 0  }
  if(!all(x==0) & !all(y==0)){
    cor <- cor(x, y, method = "pearson",
               use="pairwise.complete.obs")
  }
  
  if(cor <= -corThreshold | cor >= corThreshold) {
    pointColour <- "red" 
    textColour <- "red" 
  } else {
    pointColour <- "#737373"
    textColour <- "black" }
  
  if(!all(x==0) & !all(y==0)){
    ggally_smooth_lm(data, mapping, col=pointColour, shape = 1,...) +
      ggplot2::geom_label(
        data = data.frame(
          x = min(x, na.rm = TRUE),
          y = max(y, na.rm = TRUE),
          lab = round(cor, digits = 3)),
        mapping = ggplot2::aes(x = x, y = y, label = lab, color = NULL),
        hjust = 0, vjust = 1, fontface = "bold", col = textColour
      )
  } else {
    ggally_smooth_lm(data, mapping, col=pointColour, shape = 1,...) 
  }
}
