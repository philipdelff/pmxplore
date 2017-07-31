rDataStructure <- function(dataFrame, 
                           dataSpec="dataSpecificationFile.csv",
                           dataSpecDir = "./Scripts", 
                           NMOutput=F){
  # Function which loopes through the columns of the dataframe,
  # idenifies matching column in the spec and does the appropriate setting
  
  # ------------------------------------------------------------------
  #  Functions for what to do with each column type
  # ------------------------------------------------------------------
  charCol <- function(colvector){# Pure character vector (e.g. Comments or units)
    if(!is.character(colvector)){
      colvector <- as.character(colvector)
    }
    return(colvector)
  }
  numCol <- function(colvector){# Numerical / integer
    if(!is.numeric(colvector)){
      colvector <- as.numeric(as.character(colvector))
    }
    return(colvector)
  }
  catCharCol <- function(colvector, levs, labs){# catgorical (w specified levels)
    if(is.factor(colvector)){
      colvector <- as.character(colvector)
    }
    colvector <- factor(colvector, 
                        levels = levs,
                        labels = labs)
    return(colvector)
  }
  
  # ------------------------------------------------------------------
  # Changes if the dataset is a nonmem output file
  # ------------------------------------------------------------------
  # Rename ID back to NMSEQSID
  if(NMOutput==T & any(names(dataFrame)=="ID")){
    names(dataFrame)[names(dataFrame)=="ID"] <- "NMSEQSID"
  }
  # Time as a numeric varaible
  if(NMOutput==T & any(names(dataFrame)=="TIME")){
    dataFrame$TIME  <- as.numeric(dataFrame$TIME)
  }
  # Correct any empty cells that has been filled with zeros in nonmem
  if(NMOutput==T){
    if(any(names(dataFrame)=="AMT")){
      # remove only 0 amt which are not a dosing event
      dataFrame$AMT   <- as.numeric(dataFrame$AMT)
      dataFrame$AMT[dataFrame$AMT==0 & dataFrame$EVID==1] <- NA
    }
    if(any(names(dataFrame)=="DV")){
      # remove only 0 which are not a observation event
      dataFrame$DV   <- as.numeric(dataFrame$DV)
      dataFrame$DV[dataFrame$DV==0 & dataFrame$EVID==0] <- NA
    }
    if(any(names(dataFrame)=="LNDV")){
      # remove only 0 which are not a observation event
      dataFrame$LNDV   <- as.numeric(dataFrame$LNDV)
      dataFrame$LNDV[dataFrame$LNDV==0 & dataFrame$EVID==0] <- NA
    }
    if(any(names(dataFrame)=="BLQ")){
      # remove only 0 which are at a dose event
      dataFrame$BLQ   <- as.numeric(dataFrame$BLQ)
      dataFrame$BLQ[dataFrame$BLQ==0 & dataFrame$EVID==0] <- NA
    }
    if(any(names(dataFrame)=="OCC")){
      # remove all 0 since no 0 occ in data
      dataFrame$OCC   <- as.numeric(dataFrame$OCC)
      dataFrame$OCC[dataFrame$OCC==0] <- NA
    }
    
    # Make sure all etas are numeric
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
  }
  
  # ------------------------------------------------------------------
  # For available columns, match the spec
  # ------------------------------------------------------------------
  for(i in names(dataFrame)){
    # only set column if column is defined in dataSpec
    if(i %in% unique(dataSpec$Variable)){
      
      # Find corresponding variable in dataSpec
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
        
        if(all(is.na(code))){# "numeric" column which should be considered categorical
        # extract the unique values and use as lables
          code <- sort(unique(dataFrame[!is.na(dataFrame[,i]),i])) 
          descr <- code
        }
        dataFrame[,i] <- catCharCol(dataFrame[,i], 
                                    levs = code, 
                                    labs = descr)
        
      }
    }
  }# end of loop
  
  # Issue warning if non-defined columns in dataset
  if(any(!(names(dataFrame) %in% unique(dataSpec$Variable)))){
    whichNotDefined <- 
      names(dataFrame)[(!(names(dataFrame) %in% unique(dataSpec$Variable)))]
    warning(paste0("Columns: '", paste(whichNotDefined, collapse=" "), 
                   "' not defined in data specification file. ", 
                   "R structure not set for these column(s)."))
  }
  return(dataFrame)
}
