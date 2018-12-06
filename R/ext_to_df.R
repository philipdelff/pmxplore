#' @title Generate parameter table suitable for reports
#' 
#' @description Loads NONMEM output files (lst, ext, and if covariance setp was run, the cov file) using 
#' nonmem2R::sumoR and compiles a data.frame with parameter estiamtes, RSE and Shrinkage suitable for report. 
#' The resulting data.frame suitable as input to kableExtra.
#' 
#' @param modelFile name of NONMEM output files *without* the .ext extension (including full or relative path to file)
#' @param digits number of significant digits, Default: 4
#' @return data frame
#' @details DETAILS
#' @seealso 
#'  \code{\link[nonmem2R]{sumoR}}
#' @rdname ext_to_df
#' @export 
#' @importFrom nonmem2R sumoR
#' @importFrom dplyr mutate bind_rows mutate_if
#' @importFrom stringr str_replace_all str_extract_all str_sub
#' @importFrom tibble rownames_to_column as.tibble

ext_to_df <- function(modelFile, digits=4){
  
  # -----------------------------------
  # Checks and messages
  # -----------------------------------
  lst <- paste0(modelFile, ".lst")
  ext <- paste0(modelFile, ".ext")
  cov <- paste0(modelFile, ".cov")
  
  if(!file.exists(lst)){
    stop("Cannot find .lst file related to: ", modelFile)
  }
  if(!file.exists(ext)){
    stop("Cannot find .ext file related to: ", modelFile)
  }
  if(!file.exists(cov)){
    warning("Cannot find .cov file related to: ", modelFile)
    # need to check what is in $theta.sd id cov did not go through
  }
  
  # -----------------------------------
  # use sumo to read in files
  # -----------------------------------
  sumo <- nonmem2R::sumoR(modelFile) # sumoR(modelFile, ...)
  ext <-  sumo[["Ext"]]
  
  
  # -----------------------------------
  # Thetas
  # -----------------------------------
  # check for fixed
  fixedThetas <- ext$fix[str_detect(ext$fix, "THETA")]
  
  thetas <- 
    data.frame(Estimate = ext$theta, 
               pRSE = 100*(ext$theta.sd / ext$theta), 
               Shrinkage = vector(mode = "numeric", length(ext$theta)) 
    ) %>% 
    tibble::rownames_to_column("Parameter") %>% 
    dplyr::mutate(fixed = ifelse(Parameter %in% fixedThetas, TRUE, FALSE))
  
  # -----------------------------------
  # Omegas
  # -----------------------------------
  # check for fixed
  fixedOmegas <- ext$fix[str_detect(ext$fix, "OMEGA")] %>% 
    # remove all dots
    stringr::str_replace_all(., "\\.", "") 
  
  # find diagonal elements (i.e. those with two identical numbers) and strip off second digit
  strLoc <- tibble::as.tibble(
    stringr::str_extract_all(fixedOmegas, "[0-9]", simplify = T)
  ) %>% 
    dplyr::mutate(diagonal = V1 == V2) 
  
  fixedOmegas[strLoc$diagonal] <- 
    stringr::str_sub(fixedOmegas[strLoc$diagonal], end = -2)
  
  # find diagonal values
  omega.diagonal    <- diag(ext$omega)
  omega.diagonal.sd <- diag(ext$omega.sd)
  omega.shrink      <- sumo[['etaShrink']]
  
  # To do: check for off diagonal values and add them
  omegas <- 
    data.frame(Estimate = omega.diagonal, 
               pRSE = 100*(omega.diagonal.sd / omega.diagonal / 2), 
               Shrinkage = omega.shrink 
    ) %>% 
    tibble::rownames_to_column("Parameter") %>% 
    dplyr::mutate(fixed = ifelse(Parameter %in% fixedOmegas, TRUE, FALSE))
  
  # -----------------------------------
  # Sigmas
  # -----------------------------------
  # check for fixed
  fixedSigmas <- ext$fix[str_detect(ext$fix, "SIGMA")]
  
  sigma.diagonal    <- diag(ext$sigma)
  sigma.diagonal.sd <- diag(ext$sigma.sd)
  sigma.shrink      <- sumo[['epsShrink']]
  
  # To do: check for off diagonal values and in that case add them 
  sigmas <- 
    data.frame(Estimate = sigma.diagonal, 
               pRSE = 100*(sigma.diagonal.sd / sigma.diagonal), 
               Shrinkage = vector(mode = "numeric", length(sigma.diagonal)) 
    ) %>% 
    tibble::rownames_to_column("Parameter") %>% 
    dplyr::mutate(fixed = ifelse(Parameter %in% fixedSigmas, TRUE, FALSE))
  
  # -----------------------------------
  # Merge and format
  # -----------------------------------
  parTab <- 
    dplyr::bind_rows(list(thetas, omegas, sigmas)) %>% 
    dplyr::mutate(pRSE = abs(pRSE), 
                  Shrinkage = ifelse(str_detect(Parameter, "THETA"), NA, Shrinkage), 
                  # remove values for fixed parameters
                  pRSE = ifelse(fixed, NA, pRSE),
                  Shrinkage = ifelse(fixed, NA, Shrinkage)) %>% 
    dplyr::mutate_if(is.numeric, signif, digits=digits)
  
  # make sure always data.frame
  parTab <- as.data.frame(parTab)
  
  parTab
}

