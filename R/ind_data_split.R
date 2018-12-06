#' @title ind_data_split
#' @description splits data into list subsets for plotting
#' @param df data frame
#' @param n_per_page number of ids/key variable per page, Default: 12
#' @param id column name of id/key variable, Default: 'ID'
#' @return list with subsets of the data frame
#' @rdname ind_data_split
#' @export 
#' @importFrom zoo na.locf

# needs to be updated to handle tibbles

ind_data_split <- function(df, n_per_page=12, id="ID"){

  # Error message if dataset does not have any rows
  if(nrow(df)==0){
    stop('Cannot split an empty dataset')
  }
  
  # Define number of splits
  n_id <- length(unique(df[,id]))
  n_split <- ceiling(n_id/n_per_page)
  
  # Add splitting columns to dataset
  df$Splits[!duplicated(df[,id])] <-
    rep(1:n_split, each=n_per_page, length.out=n_id)
  df$Splits <- zoo::na.locf(df$Splits)
  
  # generate a list with nSplit datasets
  data_splits <- split(df, df$Splits)
  
  return(data_splits)
}
