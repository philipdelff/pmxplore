library(devtools)
library(sinew)
library(roxygen2)

install.packages("root")
library(root)

makeOxygen()

devtools::document()
