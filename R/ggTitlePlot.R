#' @title title page
#' @description Plot to serve as divider page for multiple plot pdfs
#' @param title test string to output in plot
#' @param text_size size of text, Default: 18
#' @return ggplot object
#' @rdname gg_title_plot
#' @export
#' @import ggplot2
gg_title_plot <- function(title, text_size=18){
  
  tmp <- data.frame(x=5, y=5, label=title)
  
  p <- ggplot(data=tmp) + 
    geom_text(aes(x=x, y=y, label=label), size=text_size) +
    coord_cartesian(xlim=c(0,10), ylim=c(0,10)) + 
    theme_bw() + 
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank()
    )
  return(p)
}