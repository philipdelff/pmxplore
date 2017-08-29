#' @title summarize covariates
#' @description summarize covariates either of type continuous or categorical
#' @param df data frame
#' @param covVector vector of column names
#' @param type continous or categorical variable, Default: 'Cont'
#' @param ... additional details to pass to summaryFun
#' @return summary dataframe
#' @rdname summarizeCov
#' @export 
summarizeCov <- function(df, covVector, type = "Cont", ...){
  
  if(type == "Cont"){
    summaryFun <- summarizeCont
  }
  if(type == "Cat"){
    summaryFun <- summarizeCat
  }
  for(i in unique(covVector)) {
    tmp <- summaryFun(df, i, ...)
    if(i == covVector[1]){
      out <- tmp
    }else{
      out <- rbind(out, tmp)
    }
  }
  return(out)
}
