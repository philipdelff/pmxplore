library(tidyverse)

## Load datasets to be used for testing ##

# 1. theoph
theoph <- Theoph
theoph$Subject <- as.character(theoph$Subject)
theoph$Subject <- factor(theoph$Subject,
                         levels = sort(as.numeric(unique(theoph$Subject))))
# to nonmem structure
theophConc <- theoph %>% 
  mutate(AMT = as.numeric(NA),
         EVID = as.numeric(0),
         MDV = as.numeric(0))
theophDose <- theophConc %>% 
  filter(!duplicated(Subject)) %>% 
  mutate(AMT = Dose, 
         EVID = 1, 
         MDV = 1)
theophNM <- dplyr::full_join(theophDose, theophConc) %>% 
  group_by(Subject) %>% 
  arrange(Time, .by_group = TRUE)

rm(theoph, theophConc, theophDose)

theopDataspecFile <- "theoph_dataSpec.csv"

# 2. Simulated dataset
simPK <- read.csv(file=file.path("./tests/testthat/test_datasets", "sim1.csv"),
                  stringsAsFactors=F)
simPKDataspecFile <- "sim1_dataSpec.csv"
