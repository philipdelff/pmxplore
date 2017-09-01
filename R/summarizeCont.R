#' @title summarize a continuous variable
#' @param df PARAM_DESCRIPTION
#' @param col_name PARAM_DESCRIPTION
#' @param p quantiles to output, Default: c(0.05, 0.95)
#' @param signif number of significant digits to output, Default=3
#' @param missingFlag flag indicating missing variables, Default=-99
#' @return summary dataframe
#' @details DETAILS
#' @rdname summarize_cont
#' @export 
#' @importFrom dplyr summarise
#' @importFrom rlang sym quo UQ
summarize_cont <- function(df, col_name, p=c(0.05, 0.95), 
                           signif=3, missingFlag=-99){
  sym_col_name <- rlang::sym(col_name)
    
  tab <- rlang::quo(df %>% dplyr::summarise(
    Characteristic = col_name,
    # !! with quosures can have trouble with parsing order
    # so specifically use UQ
    N        = sum(!is.na(!!sym_col_name) & rlang::UQE(sym_col_name) != !!missingFlag), 
    NMissing = sum(is.na(!!sym_col_name) | rlang::UQE(sym_col_name) == !!missingFlag),
    PercentMissing = 100*(NMissing/(NMissing+N)),
    Mean     = mean(!!sym_col_name, na.rm = TRUE), 
    SD       = sd(!!sym_col_name, na.rm = TRUE), 
    Min      = min(!!sym_col_name, na.rm = TRUE),
    Median   = quantile(!!sym_col_name, probs =0.50, na.rm = TRUE, names=FALSE), 
    Max      = max(!!sym_col_name, na.rm = TRUE)))
  
  tab <- eval_tidy(tab) 
  
  for(i in 1:length(p)){
    nameP <- paste0("p",p[i])
    colP <- rlang::quo(df %>% summarize(
      !!nameP := quantile(!!sym_col_name, probs=p[i], na.rm = T, names=F)))
    tab <- cbind(tab, eval_tidy(colP))
  }
  
  # Return with the specified significant digits
  noRounding <- c(which(str_detect(names(tab), "Characteristic")),
                  which(str_detect(names(tab), "^N")))
  tab[,-noRounding] <- 
    apply(tab[,-noRounding], 2, signif, digits=signif)
  
  return(tab)
}
