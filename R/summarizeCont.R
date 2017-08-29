#' @title summarize continuous variables
#' @param df dataframe
#' @param colName summarization column
#' @param p quantiles to summarize, Default: c(0.05, 0.95)
#' @rdname summarizeCont
#' @export 
#' @importFrom dplyr summarise
#' @importFrom rlang sym quo UQ
summarizeCont <- function(df, colName, p=c(0.05, 0.95)){
  symColName <- rlang::sym(colName)
    
  tab <- rlang::quo(df %>% dplyr::summarise(
    Characteristic = colName,
    # !! with quosures can have trouble with parsing order
    # so specifically use UQ
    N        = sum(!is.na(UQ(symColName)) & !!symColName!=-99),
    NMissing = sum(is.na(UQ(symColName)) | !!symColName==-99),
    Mean     = mean(!!symColName, na.rm = TRUE), 
    SD       = sd(!!symColName, na.rm = TRUE), 
    Min      = min(!!symColName, na.rm = TRUE),
    Median   = quantile(!!symColName, probs =0.50, na.rm = TRUE, names=FALSE), 
    Max      = max(!!symColName, na.rm = TRUE)))
  
  tab <- eval_tidy(tab) 
  
  for(i in 1:length(p)){
    nameP <- paste0("p",p[i])
    colP <- rlang::quo(df %>% summarize(
      !!nameP := quantile(!!symColName, probs=p[i], na.rm = T, names=F)))
    tab <- cbind(tab, eval_tidy(colP))
  }
  return(tab)
}
