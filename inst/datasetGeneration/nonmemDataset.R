# R script to generate nonmem dataset included in the package: 
# Uncomment the code below to re-generate/add data
# Last modified: 2018-11-27

library(magrittr)
library(ggplot2)

# ------------------------------------------------------------------
#  Generate dataset to simulate from
# ------------------------------------------------------------------

# Baseline data
nIDs <- 50

covariates <- dplyr::tibble(NMSEQSID = 1:nIDs) %>% 
  dplyr::mutate(
    # randomly generate demographics with some correlations
    SEXM = round(runif(nIDs, min = 0, max = 1)), 
    RACE = round(runif(nIDs, min = 1, max = 3)),
    BWT  = ifelse(SEXM==1 & RACE == 1, rnorm(1, mean = 80, sd = 5),  NA),  # males
    BWT  = ifelse(SEXM==1 & RACE == 2, rnorm(1, mean = 85, sd = 5),  BWT),
    BWT  = ifelse(SEXM==1 & RACE == 3, rnorm(1, mean = 75, sd = 5),  BWT),
    BWT  = ifelse(SEXM==0 & RACE == 1, rnorm(1, mean = 65, sd = 5),  BWT),  # females 
    BWT  = ifelse(SEXM==0 & RACE == 2, rnorm(1, mean = 70, sd = 5),  BWT),
    BWT  = ifelse(SEXM==0 & RACE == 3, rnorm(1, mean = 60, sd = 5),  BWT),
    BCREA = ifelse(SEXM==1, rnorm(50, mean = 0.9, sd = 0.12), # males in mg/dL
                   rnorm(1, mean = 0.8, sd = 0.10)),          # females
    AGE = rnorm(nIDs, mean = 40, sd = 8), 
    BCRCL = (140-AGE)*(BWT) / (72*BCREA), 
    BCRCL = ifelse(SEXM==0, 0.85*BCRCL, BCRCL),
    # randomly generate study related variables
    STUDYID = round(runif(nIDs, min = 1, max = 2)), 
    DOSE = 200) %>% 
  dplyr::select(-BCREA) %>% 
  dplyr::mutate_at(.vars = c("AGE","BWT", "BCRCL"), .funs = signif, digits=3)

# Times: profiles at Day 1 and Day 15
times <- c(0,  # for dose record
           c(0.5, 1, 2, 3, 5, 10, 23.9),              # Day 1
           336 + c(-0.2, 0.5, 1, 2, 3, 5, 10, 23.9))  # Day 15

sampleTimes <- 
  dplyr::tibble(NMSEQSID = rep(1:nIDs, each = length(times))) %>% 
  dplyr::mutate(TIME = rep(times, nIDs), 
         DAY = rep(rep(c(1, 15), each = length(times)/2), nIDs), 
         OCC = rep(c(NA, rep(1, length(times)/2-1), NA, rep(2, length(times)/2-1)), nIDs), 
         TAPD = ifelse(DAY==1, TIME, TIME-336), 
         TAPD = ifelse(TAPD <0, 23.9, TAPD))  

simData <- dplyr::full_join(covariates, sampleTimes)
rm(times, sampleTimes) 

# -------------
# Add needed nonmem columns  
# -------------
simData <- simData %>% 
  dplyr::mutate(
    C = vector("character", length = n()),
    # doses
    AMT  = ifelse(TIME == 0, DOSE, 0),
    ADDL = ifelse(TIME == 0, 20, "."), 
    II   =  ifelse(TIME == 0, 24, "."),   # QD dosing
    EVID = ifelse(AMT==0, 0, 1), 
    CMT  = ifelse(AMT!=0, 1, 2),         # oral absorption
    DV   = ".", 
    BLQ  = 0) %>% 
  dplyr::arrange(NMSEQSID, TIME) %>% 
  dplyr::select(C, NMSEQSID, TIME, TAPD, DV, AMT, EVID, CMT, OCC, BLQ, DOSE, ADDL, II,
         BWT, AGE, BCRCL, SEXM, RACE, STUDYID, DAY)

# -------------
#  Output dataset
# -------------
# Dataset name and path to location
outFile <- file.path("inst","datasetGeneration","simData.csv")
# Write dataset
write.csv(simData, file=outFile, row.names=F, quote=F, na = ".")


# ------------------------------------------------------------------
# Execute nonmem to generate DV data: run001.mod
# ------------------------------------------------------------------
# paste(names(simData), collapse= " ")
# qpsn -t 20 -r 1500 -- execute run001.mod





# ------------------------------------------------------------------
#  Read in the simulated data and tweak to a plausible example
# ------------------------------------------------------------------
inFile <- file.path("inst","datasetGeneration","simtab001")
simDataIn <- read.csv(file=inFile, skip=1, header = T, sep = "", as.is = T)

# Get rid of outputted zeros
simDataIn <- simDataIn %>% 
  dplyr::mutate(C = "", 
         DV = ifelse(EVID==1, NA, DV), 
         BLQ = ifelse(EVID==1, NA, BLQ),
         AMT = ifelse(EVID==0, ".", AMT), 
         OCC = ifelse(OCC==0, NA, OCC)) 

# ggplot2::ggplot(simDataIn, ggplot2::aes(y=DV, x=TIME, group=NMSEQSID)) +
# ggplot2::geom_line() + coord_cartesian(xlim = c(0,40))
# summary(simDataIn$DV, na.rm = T)

# introduce a lloq: 
LLOQ <- 0.15
simDataIn <- simDataIn %>% 
  dplyr::mutate(BLQ = ifelse(!is.na(DV) & DV < LLOQ & EVID==0, 1, 0), 
         DV  = ifelse(!is.na(DV) & DV < LLOQ & EVID==0, NA, DV), 
         DV = signif(DV, digits = 4)) 

# Include with a pre-first dose record
preDose <- simDataIn %>% 
  dplyr::filter(!duplicated(NMSEQSID)) %>% 
  dplyr::mutate(TIME = -0.2, 
         TAPD = NA, 
         AMT = ".")

simDataIn <- simDataIn %>% 
  rbind(preDose) %>% 
  dplyr::arrange(NMSEQSID, TIME)

# Introduce a couple of missing covs
simDataIn <- simDataIn %>% 
  dplyr::mutate(AGE = ifelse(NMSEQSID %in% c(5,25,50), -99, AGE), 
         RACE = ifelse(NMSEQSID %in% c(8,36), -99, RACE))


# Comments out some rows 
simDataIn <- simDataIn %>% 
  dplyr::mutate(C = ifelse(TIME < 0 , "C", C), 
         # add a comment field
         COMMENT = ifelse(TIME < 0, "Pre first dose sample", " "))

# Randomly select a few more rows to comments out
commentRow <- round(runif(10, min=1, max=nrow(simDataIn)))

simDataIn <- simDataIn %>% 
  dplyr::mutate(
    # only comment out samples & not already commented out
    C = ifelse(rownames(.) %in% commentRow & EVID == 0 & C !="C", "C", C), 
    # add a comment field
    COMMENT = ifelse(rownames(.) %in% commentRow & EVID == 0 & C =="C", 
                     "Randomly selected for illustration", COMMENT))

pkData <- simDataIn 

# ------------------------------------------------------------------
#  Add as available dataset in package
# ------------------------------------------------------------------
# Write to file
write.csv(pkData, file="simPKdata.csv", quote = F, row.names = F, na = ".")

use_data(pkData, pkg="../pmxplore")
