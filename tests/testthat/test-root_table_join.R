### Unit tests for root_table_join function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testMass <- readRDS(testthat::test_path("testdata", "root_table_join-valid-mass.RDS"))
testPool <- readRDS(testthat::test_path("testdata", "root_table_join-valid-pool.RDS"))
testChem <- readRDS(testthat::test_path("testdata", "root_table_join-valid-chem.RDS"))



### Test expected output type
testthat::test_that(desc = "Output type", {
  
  temp <- root_table_join(input_mass = testMass,
                          input_pool = testPool,
                          input_chem = testChem)
  
  testthat::expect_type(object = temp,
                        type = "list")
  
})



### Test expected errors for issues with input_mass table
# Test when input_mass lacks required column
testthat::test_that(desc = "Table input_mass missing column", {
  
  tempMass <- testMass %>%
    dplyr::select(-dryMass)
  
  testthat::expect_error(object = root_table_join(input_mass = tempMass,
                                                  input_pool = testPool,
                                                  input_chem = testChem),
                         regexp = "Required columns missing from input_mass: dryMass")
})

#   Test when input_mass has no data
testthat::test_that(desc = "Table input_mass missing data", {
  
  tempMass <- testMass %>%
    dplyr::filter(uid == "coconut")
  
  testthat::expect_error(object = root_table_join(input_mass = tempMass,
                                                  input_pool = testPool,
                                                  input_chem = testChem),
                         regexp = "Table input_mass has no data.")
})



### Test expected errors for issues with input_pool table
#   --> begin again here

