
#' @title pixiedust::dust with az specifications
#' @description  Wrapper for pixiedust::dust with az specifications
#' @param df data frame
#' @param caption string with caption for table, Default: NULL
#' @param label string with label for cross-reference. "tab:" is automatically added, Default: NULL
#' @param print_method "latex" or "html",  Default: "latex"
#' @param long use longtable environment (for latex-use only), Default: F
#' @return dust object
#' @rdname aztable
#' @export
#' @import pixiedust

aztable <- function(df, caption=NULL, label=NULL,
                    print_method="latex", long=F){
  # To fix:
  # Probably good idea to set a default for significant digits of numeric cols
  # Border thickness does for some reason not work:
  # Trying to update latex packages on cluster to see it that solves it

  if(!long & print_method!="latex") {
    long <- F
    warning("Longtable environment only supported for LaTeX. Argument ignored")
  }

  if(!is.null(label) & print_method!="latex") {
    label <- NULL
    warning("use of label only supported for LaTeX. Argument ignored")
  }

  tab <-
    pixiedust::dust(df,
         longtable=long,
         hhline=T,
         caption = caption,
         label = label) %>%
    pixiedust::sprinkle(part="head", bold=T,
             border=c("top", "bottom"),
             border_thickness=3, border_units="pt") %>%
    pixiedust::sprinkle(row=nrow(df), border=c("bottom"),
             border_thickness=3, border_units="pt") %>%
    pixiedust::sprinkle_print_method(print_method)

  return(tab)
}
