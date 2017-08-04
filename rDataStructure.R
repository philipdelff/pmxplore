###################################################
# rDataStructure.R
# 
# Author: Helena Edlund
# Created on: 2017-08-01
# Modified on:
# Purpose: Function to set the R data frame structure
# Dependencies: a file/data frame with the dataSpec
###################################################

# Description:
# The function reads in a .csv file with the data specifications 
# (also accepts the same as a already loaded data frame)
# The data spec will be delivered with the dataset if requested through QCP order form
# and should include the columns: Variable, Code, DeCode, Description, R Variable Type. 
# The description column may be changed if a different description is preferred

rDataStructure <- function(dataFrame, 
                           dataSpec="dataSpecificationFile.csv",
                           dataSpecDir = "./Scripts", 
                           NMOutput=F){
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
  if(NMOutput==T & any(names(dataFrame)=="ID")){
    names(dataFrame)[names(dataFrame)=="ID"] <- "NMSEQSID"
  }
  # Time as a numeric variable
  if(NMOutput==T & any(names(dataFrame)=="TIME")){
    dataFrame$TIME  <- as.numeric(dataFrame$TIME)
  }
  # Correct any empty cells that has been filled with zeros in NONMEM
  if(NMOutput==T){
    if(any(names(dataFrame)=="AMT")){
      # Remove any 0 AMT which are not a dosing event
      dataFrame$AMT   <- as.numeric(dataFrame$AMT)
      dataFrame$AMT[dataFrame$AMT==0 & dataFrame$EVID==1] <- NA
    }
    if(any(names(dataFrame)=="DV")){
      # Remove any 0 DV which are not a observation event
      dataFrame$DV   <- as.numeric(dataFrame$DV)
      dataFrame$DV[dataFrame$DV==0 & dataFrame$EVID==0] <- NA
    }
    if(any(names(dataFrame)=="LNDV")){
      # Remove any 0 LNDV which are not a observation event
      dataFrame$LNDV   <- as.numeric(dataFrame$LNDV)
      dataFrame$LNDV[dataFrame$LNDV==0 & dataFrame$EVID==0] <- NA
    }
    if(any(names(dataFrame)=="BLQ")){
      # Remove any BLQ flag of 0 which are not at a observation event
      dataFrame$BLQ   <- as.numeric(dataFrame$BLQ)
      dataFrame$BLQ[dataFrame$BLQ==0 & dataFrame$EVID==0] <- NA
    }
    if(any(names(dataFrame)=="OCC")){
      # Remove all OCC=0 since not defined in data
      dataFrame$OCC   <- as.numeric(dataFrame$OCC)
      dataFrame$OCC[dataFrame$OCC==0] <- NA
    }
    
    # Make sure all ETA columns are numeric
    for(j in 1:sum(str_detect(names(dataFrame), "ETA"))){
      dataFrame[,paste0("ETA", j)] <- 
        as.numeric(as.character(dataFrame[,paste0("ETA", j)]))
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
  for(i in names(dataFrame)){
    # Only set column if column is defined in dataSpec
    if(i %in% unique(dataSpec$Variable)){
      
      # Extract corresponding variable from dataSpec
      var <- dataSpec[dataSpec$Variable==i,]
      
      # Set the structure depending on type of variable
      if(unique(var$Type) == "Character"){
        dataFrame[,i] <- charCol(dataFrame[,i])
      }
      if(unique(var$Type) == "Numeric" | unique(var$Type) == "Integer"){
        dataFrame[,i] <- numCol(dataFrame[,i])
      }
      if(unique(var$Type) == "Categorical"){
        # Extract levels and labels
        code <- var$Code
        descr <- var$Description
        		
        if(all(is.na(code))){
		# "numeric" column which should be considered categorical
		# but where we extract the levels from the data (not the spec)
        # e.g. ID and dose levels
          code <- unique(dataFrame[!is.na(dataFrame[,i]),i])
          descr <- code
        }
        dataFrame[,i] <- catCharCol(dataFrame[,i], 
                                    levs = code, 
                                    labs = descr)
      }
    }
  }# end of loop
  
  # Issue warning for columns in dataset not defined in spec
  if(any(!(names(dataFrame) %in% unique(dataSpec$Variable)))){
    whichNotDefined <- 
      names(dataFrame)[(!(names(dataFrame) %in% unique(dataSpec$Variable)))]
    warning(paste0("Columns: '", paste(whichNotDefined, collapse=" "), 
                   "' not defined in data specification file. ", 
                   "R structure not set for these column(s)."))
  }
  return(dataFrame)
}
