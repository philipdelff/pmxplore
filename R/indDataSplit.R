#' @title split dataset into sub elements
#' @param df data frame
#' @param nPerPage number of groups per split, Default: 12
#' @param id PARAM_DESCRIPTION, Default: 'ID'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname ind_data_split
#' @importFrom zoo na.locf
#' @export 
ind_data_split <- function(df, nPerPage=12, id="ID"){
  # Generates a list of dataset subsets to get nSplit plots per page

  # Define number of splits
  nId <- length(unique(df[,id]))
  nSplit <- ceiling(nId/nPerPage)
  
  # Add splitting columns to dataset
  df$Splits[!duplicated(df[,id])] <-
    rep(1:nSplit, each=nPerPage, length.out=nId)
  df$Splits <- na.locf(df$Splits)
  
  # generate a list with nSplit datasets
  dataSplits <- split(df, df$Splits)
  
  return(dataSplits)
}
