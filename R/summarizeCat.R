#' @title summarize a categorical variable
#' @param df dataframe
#' @param colName column name
#' @return summary data frame
#' @details DETAILS
#' @rdname summarizeCat
#' @export 
#' @importFrom rlang sym quo
summarizeCat <- function(df, colName){
  # this function doesn't make sense with regards to the whole category thing 
  symColName <- rlang::sym(colName)
  
  tab <- rlang::quo(df %>% 
    group_by(Category = !!symColName) %>% 
    summarise(N = n()) %>%
    mutate(Percent = 100*(N / sum(N)), 
           Characteristic = colName, 
           Category = as.character(Category), 
           Category = ifelse(is.na(Category), "Missing", Category)) %>% 
    select(Characteristic, Category, N, Percent))

  tab <- eval_tidy(tab)
  
  return(tab)
}
