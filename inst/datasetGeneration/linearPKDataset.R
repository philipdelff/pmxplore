# R script to generate nca dataset included in the package: 

# Generate a random set of clearances
idPerDose  <- 10 
DOSES <- c(2.5, 5, 10, 20, 40, 60)
nIDs <- length(DOSES)*idPerDose

# For AUC vs DOSE data
ncaData <- dplyr::tibble(ID = 1:nIDs, 
                          DOSE = rep(DOSES, each=idPerDose), 
                          CL = rlnorm(nIDs, log(15), 0.1)) %>% 
  dplyr::mutate(
    CL = signif(CL, digits = 3),
    AUC = signif(DOSE/CL, digits = 4))

# Possibly redo this and use PKNCA to create a NCA dataset aligning to the simulated PK data

# ------------------------------------------------------------------
#  Add as available dataset in package
# ------------------------------------------------------------------
# Write to file
outFile <- file.path("inst","datasetGeneration", "simNcaData.csv")
write.csv(ncaData, file=outFile, quote = F, row.names = F, na = ".")

outFile <- file.path("inst","datasetGeneration", "simNcaData.Rdata")
save(ncaData, file=outFile)

use_data(ncaData, pkg="../pmxplore")
