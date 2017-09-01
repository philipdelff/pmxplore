#' @title summarize covariates
#' @description summarize covariates either of type continuous or categorical
#' @param df data frame
#' @param cov_vector vector of column names
#' @param type continous or categorical variable, Default: 'Cont'
#' @param ... additional details to pass to summaryFun
#' @return summary dataframe
#' @rdname summarize_cov
#' @export 
summarize_cov <- function(df, cov_cector, type = "Cont", ...){
  
  if(type == "Cont"){
    summary_fun <- summarize_cont
  }
  if(type == "Cat"){
    summary_fun <- summarize_cat
  }
  for(i in unique(cov_vector)) {
    tmp <- summary_fun(df, i, ...)
    if(i == cov_vector[1]){
      out <- tmp
    }else{
      out <- rbind(out, tmp)
    }
  }
  return(out)
}
