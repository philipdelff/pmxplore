library(azrdatareview)
library(stringr)

test_that("expecting a lm object", {
  dat <- data.frame(x=c(1,2,3,4,5,6,7,8,9,10),
                    y=c(1,2,3,4,5,6,7,8,9,10))
  lm_fit <- lm(x~y, data=dat)
  glm_fit <- glm(x~y, data=dat)
  not_lm <- "This_is_a_string"
  
  # expecting a character string if passing lm object
  expect_true(is.character(print_lm_eq(lm_fit)))
  
  # fail if glm model
  expect_error(print_lm_eq(glm_fit), "Input is not a model object")
  
  # fail if string input
  expect_error(print_lm_eq(not_lm), "Input is not a model object")
  }
)
