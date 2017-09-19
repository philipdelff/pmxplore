#' @title summarize a categorical variable
#' @param df dataframe
#' @param col_name column name
#' @param signif number of significant digits to output
#' @return summary data frame
#' @details DETAILS
#' @rdname summarize_cat
#' @export 
#' @importFrom rlang sym quo expr_text
#' @import dplyr

summarize_cat <- function(df, col_name, signif=3){
  # this function doesn't make sense with regards to the whole category thing 
  # H: ok, not sure what you are refering to but it takes the levels of the factor. Let's discuss.
  sym_col_name <- rlang::sym(col_name)

  # test for factor  
  column <- df[[expr_text(sym_col_name)]]
  if(!is.factor(column)){
    stop(paste(expr_text(sym_col_name), "is not a factor"))
  }
  
  tab <- rlang::quo(df %>% 
    group_by(Category = !!sym_col_name) %>% 
    summarise(N = n()) %>%
    mutate(Percent = 100*(N / sum(N)), 
           Characteristic = col_name, 
           Category = as.character(Category), 
           Category = ifelse(is.na(Category), "Missing", Category)) %>% 
    select(Characteristic, Category, N, Percent))

  tab <- eval_tidy(tab)

  # Return the percent with the specified significant digits
  rounding <- c(which(str_detect(names(tab), "Percent")))
  tab[, rounding] <- apply(tab[,rounding], 2, signif, digits=signif)
    
  return(tab)
}