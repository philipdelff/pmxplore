# R script to generate datasets included in the package: 
# Uncomment the code below to re-generate/add data

# 1. theoph dataset in NONMEM format
theoph <- Theoph

theoph <- theoph %>% 
  dplyr::mutate(Subject = as.character(Subject), 
         Subject = factor(Subject, 
                          levels = sort(as.numeric(unique(Subject))))) %>% 
  dplyr::arrange(Subject, Time)

theophConc <- theoph %>% 
  dplyr::mutate(AMT = as.numeric(NA), 
         EVID = as.numeric(0), 
         MDV = as.numeric(0))

theophDose <- theophConc %>% 
  dplyr::filter(!duplicated(Subject)) %>% 
  dplyr::mutate(AMT = Dose, 
                EVID = 1, 
                MDV = 1)

theophNM <- dplyr::full_join(theophDose, theophConc) %>% 
  dplyr::group_by(Subject) %>% 
  dplyr::arrange(Time, .by_group = TRUE) %>% 
  # rename to KM conventions
  dplyr::rename(NMSEQSID = Subject, 
                TAFD = Time, 
                BWT = Wt, 
                DOSE = Dose, 
                DV = conc) %>% 
  dplyr::select(NMSEQSID, TAFD, AMT, DV, EVID, MDV, DOSE, BWT)

rm(theoph, theophConc, theophDose)

# ------------------------------------------------------------------
#  Do not add as available dataset in package but use for testing
# ------------------------------------------------------------------
# Write to file
outFile <- file.path("inst","datasetGeneration","theophDataModified.RData")
save(theophNM, file = outFile)

# theohDataSpecFile <- "theoph_dataSpec.csv"

# write.csv(theophNM, file=outFile, quote = F, row.names = F, na = ".")
