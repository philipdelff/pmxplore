#' create a datastructure?
#' @description set up data from and object + specification file
#' @param .df data frame
#' @param dataSpec specification file, Default: 'dataSpecificationFile.csv'
#' @param dataSpecDir PARAM_DESCRIPTION, Default: './Scripts'
#' @param NMOutput PARAM_DESCRIPTION, Default: FALSE
#' @description
#' The function reads in a .csv file with the data specifications 
#' (also accepts the same as a already loaded data frame)
#' The data spec will be delivered with the dataset if requested through QCP order form
#' and should include the columns: Variable, Code, DeCode, Description, R Variable Type. 
#' The description column may be changed if a different description is preferred
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export 
#' @importFrom ggplot ggplot aes
#' @rdname rDataStructure
rDataStructure <- function(.df, 
                           dataSpec="dataSpecificationFile.csv",
                           dataSpecDir = "./Scripts", 
                           NMOutput=FALSE){
  # Function which loops through the columns of the data frame,
  # identifies matching column in the specification and does the appropriate setting
  
  # ------------------------------------------------------------------
  #  Functions for what to do with each column type
  # ------------------------------------------------------------------
  charCol <- function(colvector){
    if(!is.character(colvector)){
      colvector <- as.character(colvector)
    }
    return(colvector)
  }
  numCol <- function(colvector){# Numerical/Integer
    if(!is.numeric(colvector)){
      colvector <- as.numeric(as.character(colvector))
    }
    return(colvector)
  }
  catCharCol <- function(colvector, levs, labs){# categorical w levels)
    if(is.factor(colvector)){
      colvector <- as.character(colvector)
    }
    colvector <- factor(colvector, 
                        levels = levs,
                        labels = labs)
    return(colvector)
  }
  
  # ------------------------------------------------------------------
  # Changes if the dataset is a NONMEM output file
  # ------------------------------------------------------------------
  # Rename ID back to NMSEQSID
  if(NMOutput & any(names(.df)=="ID")){
    names(.df)[names(.df)=="ID"] <- "NMSEQSID"
  }
  # Time as a numeric variable
  if(NMOutput & any(names(.df)=="TIME")){
    .df$TIME  <- as.numeric(.df$TIME)
  }
  # Correct any empty cells that has been filled with zeros in NONMEM
  if(NMOutput){
    if(any(names(.df)=="AMT")){
      # Remove any 0 AMT which are not a dosing event
      .df$AMT   <- as.numeric(.df$AMT)
      .df$AMT[.df$AMT==0 & .df$EVID==1] <- NA
    }
    if(any(names(.df)=="DV")){
      # Remove any 0 DV which are not a observation event
      .df$DV   <- as.numeric(.df$DV)
      .df$DV[.df$DV==0 & .df$EVID==0] <- NA
    }
    if(any(names(.df)=="LNDV")){
      # Remove any 0 LNDV which are not a observation event
      .df$LNDV   <- as.numeric(.df$LNDV)
      .df$LNDV[.df$LNDV==0 & .df$EVID==0] <- NA
    }
    if(any(names(.df)=="BLQ")){
      # Remove any BLQ flag of 0 which are not at a observation event
      .df$BLQ   <- as.numeric(.df$BLQ)
      .df$BLQ[.df$BLQ==0 & .df$EVID==0] <- NA
    }
    if(any(names(.df)=="OCC")){
      # Remove all OCC=0 since not defined in data
      .df$OCC   <- as.numeric(.df$OCC)
      .df$OCC[.df$OCC==0] <- NA
    }
    
    # Make sure all ETA columns are numeric
    for(j in 1:sum(str_detect(names(.df), "ETA"))){
      .df[,paste0("ETA", j)] <- 
        as.numeric(as.character(.df[,paste0("ETA", j)]))
    }
  }
  
  # ------------------------------------------------------------------
  # Read in the specification file
  # ------------------------------------------------------------------
  if(!is.data.frame(dataSpec)){
    dataSpec <- read.csv(paste(dataSpecDir,dataSpec, sep="/"), 
                         stringsAsFactors = F)
    names(dataSpec) <- c("Variable","VariableDescription",
                         "Code","DeCode","Description",
                         "Type")
	# This needs to be fixed into something more flexible and less error-prone... 
  }
  
  # ------------------------------------------------------------------
  # For available columns, match the specification
  # ------------------------------------------------------------------
  for(i in names(.df)){
    # Only set column if column is defined in dataSpec
    if(i %in% unique(dataSpec$Variable)){
      
      # Extract corresponding variable from dataSpec
      var <- dataSpec[dataSpec$Variable==i,]
      
      # Set the structure depending on type of variable
      if(unique(var$Type) == "Character"){
        .df[,i] <- charCol(.df[,i])
      }
      if(unique(var$Type) == "Numeric" | unique(var$Type) == "Integer"){
        .df[,i] <- numCol(.df[,i])
      }
      if(unique(var$Type) == "Categorical"){
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
        .df[,i] <- catCharCol(.df[,i], 
                                    levs = code, 
                                    labs = descr)
      }
    }
  }# end of loop
  
  # Issue warning for columns in dataset not defined in spec
  if(any(!(names(.df) %in% unique(dataSpec$Variable)))){
    whichNotDefined <- 
      names(.df)[(!(names(.df) %in% unique(dataSpec$Variable)))]
    warning(paste0("Columns: '", paste(whichNotDefined, collapse=" "), 
                   "' not defined in data specification file. ", 
                   "R structure not set for these column(s)."))
  }
  return(.df)
}
