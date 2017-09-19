library(azrdatareview)
library(stringr)

context("ggally")

dat <- data("mtcars")
continuous <- c("mpg", "wt")
categorical <- c("cyl","gear")

# test_that("ggplot object output", {
#   ally_bar()
#   ally_box()
#   ally_cor()
#   ally_count()
#   ally_scatter_lm_cor()
#   }
# )
# 

# ally_bar
test_that("ally_bar", {
  # input is factor
  
  # errors/warnings as expected
  
  # 
})
