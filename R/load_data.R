library(dplyr)

theoph <- Theoph

theophNM <- theoph %>% 
  mutate(AMT = vector("numeric", nrow()),
    EVID = vector("numeric", nrow()), 
         MDV = vector("numeric", nrow()))


# ID    DOSE    TIME      CP      WT    EVID     MDV