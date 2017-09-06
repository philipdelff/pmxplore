
#' @title For printing a list of grobs to pdf
#' @description forces a new page for grob objects to avoid printing all on the same page
#' @param x grob grids
#' @return "printed" grob object
#' @seealso 
#'  \code{\link[grid]{grid.newpage}},\code{\link[grid]{grid.draw}}
#' @rdname grob_draw
#' @export 
#' @importFrom grid grid.newpage grid.draw

grob_draw <- function(x){
  if(is.grob(x)){
    grid::grid.newpage()
    grid::grid.draw(x)
  } else {
    grid::grid.draw(x)
  }
}
