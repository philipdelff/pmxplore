
#' @title summarizes nonmem run into data.frame
#' @description summarizes nonmem run into data.frame
#' @param mod_no string with model number
#' @param ref_mod_no string with referebne model number
#' @param listobject listobject for run generated with read.lst
#' @param record data frame with run-record
#' @param descr string with model description
#' @return data frame
#' @rdname summarize_run
#' @export 
summarize_run <- function(mod_no, ref_mod_no, listobject, record, descr){
  summary <- 
    data.frame(Run = as.character(mod_no),
               Minimization = listobject$term[1],
               OFV = as.numeric(as.character(listobject$ofv)),
               Reference = as.numeric(ref_mod_no),
               dOFV = ifelse(is.na(ref_mod_no), NA, 
                             as.numeric(as.character(listobject$ofv)) - 
                               record$OFV[record$Run==ref_mod_no]),
               Description = descr)
  return(summary)
}