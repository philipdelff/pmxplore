source("./tests/testthat/test_datasets/test-load_data.R")

test_that("data spec file", {
  # expecting error not finding file 
  expect_error(r_data_structure(.df=theophNM, data_spec=theopDataspecFile), 
               "Data specification file does not exist")
  
  # expecting error column names not correct
  spec <- read.csv(file.path("./tests/testthat/test_datasets/", theopDataspecFile),
                   stringsAsFactors = F)
  names(spec) <- c("Vars","Labs","Code","Decode",
                   "Description","R.Type")
  expect_error(r_data_structure(.df=theophNM, data_spec=spec), 
               paste0("Column names of specification file not as expected.\n",
               "Required input is: Variable.Name,Label,Code,Decode,Description,R.Type"))

  # expecting message column not defined
  theophNMadded <- theophNM %>% 
    mutate(EXTRA = as.numeric(1)) %>% 
    as.data.frame()
  
  expect_message(
    r_data_structure(.df=theophNMadded,
                     data_spec=file.path("./tests/testthat/test_datasets/", theopDataspecFile)), 
    "Columns: 'EXTRA' not defined in data specification file. R structure not set for these column(s).")
}
)

test_that("correct output", {
  # expecting column type with certain levels
  simPKset <- 
    r_data_structure(.df=simPK,
                     data_spec=file.path("./tests/testthat/test_datasets/", simPKDataspecFile))
  
  # Test a couple of columns
  expect_true(is.factor(simPKset$RACE))
  expect_identical(levels(simPKset$SEXM), c("Female","Male"))
  
  expect_true(is.numeric(simPKset$AGE))
  
  # Need to figure out why this happened:
  # Warning messages:
  #   1: In num_col(.df[, i]) : NAs introduced by coercion
  # 2: In num_col(.df[, i]) : NAs introduced by coercion
  # 3: In num_col(.df[, i]) : NAs introduced by coercion
  # 4: In num_col(.df[, i]) : NAs introduced by coercion
}
)

test_that("nm output", {
  # test the nm_data_output works separately
  
    # test the renaming of ID 
    # test that ETA columns are numeric
  
  # test that it works within the r_data_structure
  # r_data_structure(.df=simPK,
  #                  data_spec=file.path("./tests/testthat/test_datasets/", simPKDataspecFile),
  #                  nm_output = T)
  
  # expecting different message if new columns introduced

}
)




