azHistogram <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_histogram(fill="#737373", colour="#525252")
  return(p)
}
