# ------------------------------------------------------------------
#  Functions for what to do with each column type
# ------------------------------------------------------------------
char_col <- function(colvector){
  if(!is.character(colvector)){
    colvector <- as.character(colvector)
  }
  return(colvector)
}
num_col <- function(colvector){# Numerical/Integer
  if(!is.numeric(colvector)){
    colvector <- as.numeric(as.character(colvector))
  }
  return(colvector)
}
cat_char_col <- function(colvector, levs, labs){# categorical w levels)
  if(is.factor(colvector)){
    colvector <- as.character(colvector)
  }
  colvector <- factor(colvector, 
                      levels = levs,
                      labels = labs)
  return(colvector)
}

nm_output_data <- function(.df, id="NMSEQSID", 
                           blq_flag="BLQ", occ_flag="OCC"){
  
  # check that .df contains EVID
  if(!"EVID" %in% names(.df)){
    stop("Dataset does not contain an EVID column which is needed for transformations.")
  }
  
  # Rename ID back to NMSEQSID
  if("ID" %in% names(.df)){
    names(.df)[names(.df)=="ID"] <- id
  }
  # Time as a numeric variable
  if("TIME" %in% names(.df)){
    .df$TIME  <- as.numeric(.df$TIME)
  }
  # Correct any empty cells that has been filled with zeros in NONMEM
  if("AMT" %in% names(.df)){
    # Remove any 0 AMT which are not a dosing event
    .df$AMT   <- as.numeric(.df$AMT)
    .df$AMT[.df$AMT==0 & .df$EVID==1] <- NA
  }
  if("DV" %in% names(.df)){
    # Remove any 0 DV which are not a observation event
    .df$DV   <- as.numeric(.df$DV)
    .df$DV[.df$DV==0 & .df$EVID==0] <- NA
  }
  if("LNDV" %in% names(.df)){
    # Remove any 0 LNDV which are not a observation event
    .df$LNDV   <- as.numeric(.df$LNDV)
    .df$LNDV[.df$LNDV==0 & .df$EVID==0] <- NA
  }
  if(blq_flag %in% names(.df)){
    # Remove any BLQ flag of 0 which are not at a observation event
    .df[[blq_flag]]   <- as.numeric(.df[[blq_flag]])
    .df[[blq_flag]][.df[[blq_flag]]==0 & .df$EVID==0] <- NA
  }
  if(occ_flag %in% names(.df)){
    # Remove all OCC=0 since not defined in data
    .df[[occ_flag]] <- as.numeric(.df[[occ_flag]])
    .df[[occ_flag]][.df[[occ_flag]]==0] <- NA
  }
  
  # Make sure all ETA columns (if any) are numeric
  if(any(stringr::str_detect(names(.df), "ETA"))){
    for(j in 1:sum(stringr::str_detect(names(.df), "ETA"))){
      .df[,paste0("ETA", j)] <- num_col(.df[,paste0("ETA", j)])
    }
  }
  return(.df)
}

#' @title Sets up the datastructure
#' @description Set up data from and object + specification file
#' @param .df data frame
#' @param data_spec full path to specification file, Default: './SourceData/dataVariablesSpecification.csv'
#' @param nm_output is the data frame a nonmem output data, Default: FALSE
#' @param ... additional arguments passed to nm_output_data
#' @description
#' The function reads in a .csv file with the data specifications 
#' (also accepts the same as a already loaded data frame)
#' The data spec will be delivered with the dataset if requested through QCP order form
#' and should include the columns: Variable.Name Label Code Decode Description R.Type
#' (in that order)
#' @return a data frame with structure according to data spec
#' @details DETAILS
#' @export 
#' @importFrom stringr str_detect
#' @rdname r_data_structure

r_data_structure <- function(.df, 
                             data_spec=file.path("SourceData",
                                                 "dataVariablesSpecification.csv"),
                             nm_output=FALSE, 
                             ...){
  # ------------------------------------------------------------------
  # Check that .df is a data.frame
  # ------------------------------------------------------------------
  if(!is.data.frame(.df)){
    message("Input dataset needs to be a data frame. /n as.data.frame has therefore been applied")
    .df <- as.data.frame(.df)
  }
    
  # ------------------------------------------------------------------
  # Checks for the specification file
  # ------------------------------------------------------------------
  if(!is.data.frame(data_spec)){
    if(!file.exists(data_spec)){
      stop("Data specification file does not exist")
    }
  } 
  
  # ------------------------------------------------------------------
  # Read in the specification file
  # ------------------------------------------------------------------
  if(!is.data.frame(data_spec)){
    data_spec <- read.csv(data_spec, stringsAsFactors = F)
  }
  
  if(!all(names(data_spec) == c("Variable.Name","Label",
                                "Code","Decode",
                                "Description","R.Type"))){
    stop(paste0("Column names of specification file not as expected.\n",
                "Required input is: Variable.Name,Label,Code,Decode,Description,R.Type"))
  }
  
  # ------------------------------------------------------------------
  # For available columns, match the specification
  # ------------------------------------------------------------------
  for(i in names(.df)){
    # Only set column if column is defined in data_spec
    if(i %in% unique(data_spec$Variable.Name)){
      
      # Extract corresponding variable from data_spec
      var <- data_spec[data_spec$Variable.Name==i,]
      
      # Set the structure depending on type of variable
      if(unique(var$R.Type) == "Character"){
        .df[,i] <- char_col(.df[,i])
      }
      if(unique(var$R.Type) == "Numeric" | unique(var$R.Type) == "Integer"){
        .df[,i] <- num_col(.df[,i])
      }
      if(unique(var$R.Type) == "Categorical"){
        # Extract levels and labels
        code <- var$Code
        descr <- var$Description
        
        if(all(is.na(code))){
          # "numeric" column which should be considered categorical
          # but where we extract the levels from the data (not the spec)
          # e.g. ID and dose levels
          code <- unique(.df[!is.na(.df[,i]),i])
          descr <- code
        }
        .df[,i] <- cat_char_col(.df[,i], 
                                levs = code, 
                                labs = descr)
      }
    }
  }
  
  if(nm_output){
    .df <- nm_output_data(.df, ...)
  }
  
  # Issue warning for columns in dataset not defined in spec
  if(!nm_output & any(!(names(.df) %in% unique(data_spec$Variable.Name)))){
    whichNotDefined <- 
      names(.df)[(!(names(.df) %in% unique(data_spec$Variable.Name)))]
    warning(paste0("Columns: '", paste(whichNotDefined, collapse=" "), 
                   "' not defined in data specification file. ", 
                   "R structure not set for these column(s)."))
  }
  
  # Different warning if nm_output
  if(nm_output){
    set_in_nm_data <- c(names(.df)[stringr::str_detect(names(.df), "ETA")],
                        "TIME", "ID")
    all_set <- c(unique(data_spec$Variable.Name), set_in_nm_data)
    
    if(any(!(names(.df) %in% all_set))){
      whichNotDefined <- 
        names(.df)[!(names(.df) %in% all_set)]
      warning(paste0("Columns: '", paste(whichNotDefined, collapse=" "), 
                     "' not defined in data specification file. ", 
                     "R structure not set for these column(s)."))
    }
  }
  return(.df)
}
