#' @title summarize covariates
#' @description summarize covariates either of type continuous or categorical
#' @param df data frame
#' @param cov_vector vector of column names
#' @param type continous or categorical variable, Default: 'Cont'
#' @param ... additional details to pass to summary_fun
#' @return summary dataframe
#' @rdname summarize_cov
#' @export 
summarize_cov <- function(df, cov_vector, type = "Cont", ...){
  
  if(type == "Cont"){
    # Accept numeric or integers
    cov_subset <- df %>% dplyr::select(cov_vector)
    numerics <- sapply(cov_subset, is.numeric)
    integers <- sapply(cov_subset, is.integer)
    if(!all(numerics | integers)){ 
      stop("type 'Cont' is only applicable to numeric/integer columns")
    }
  }
  if(type == "Cat"){
    # Accept factor
    cov_subset <- df %>% dplyr::select(cov_vector)
    factors <- sapply(cov_subset, is.factor)
    if(!all(factors)){ 
      stop("type 'Cat' is only applicable to factors")
    }
  }
  for(i in unique(cov_vector)) {
    if(type == "Cont"){
      tmp <- summarize_cont(df, i, ...)
    }
    if(type == "Cat"){
      tmp <- summarize_cat(df, i, ...)
    }
    if(i == cov_vector[1]){
      out <- tmp
    }else{
      out <- rbind(out, tmp)
    }
  }
  return(out)
}
