library(azrdatareview)
library(stringr)

test_that("expecting a lm object input", {

  lm_fit <- lm(mpg ~ wt, data = mtcars)
  glm_fit <- glm(mpg ~ wt, data = mtcars)
  not_lm <- "This_is_a_string"
  
  # expecting a character string if passing lm object
  expect_true(is.character(print_lm_eq(lm_fit)))
  
  # should pass error if glm model or string input
  expect_error(print_lm_eq(glm_fit), "Input is not a lm model object")
  expect_error(print_lm_eq(not_lm), "Input is not a model object")
  }
)

test_that("string output", {
  
  lm_fit <- lm(mpg ~ wt, data = mtcars)

  # expected output
  expect_identical(
    print_lm_eq(lm_fit), 
    "italic(y) == \"37\" - \"5.3\" %.% italic(x) * \",\" ~ ~italic(R)^2 ~ \"=\" ~ \"0.753\"")
}
)
