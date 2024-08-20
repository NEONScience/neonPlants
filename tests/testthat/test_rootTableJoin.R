### Unit tests for rootTableJoin function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testMass <- readRDS(testthat::test_path("testdata", "rootTableJoin-valid-mass.RDS"))
testPool <- readRDS(testthat::test_path("testdata", "rootTableJoin-valid-pool.RDS"))
testChem <- readRDS(testthat::test_path("testdata", "rootTableJoin-valid-chem.RDS"))



### Test expected output type
testthat::test_that(desc = "Output type", {
  
  temp <- rootTableJoin(inputMass = testMass,
                        inputPool = testPool,
                        inputChem = testChem)
  
  testthat::expect_type(object = temp,
                        type = "list")
  
})



### Test expected errors for issues with inputMass table
# Test when inputMass lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {
  
  tempMass <- testMass %>%
    dplyr::select(-dryMass)
  
  testthat::expect_error(object = rootTableJoin(inputMass = tempMass,
                                                inputPool = testPool,
                                                inputChem = testChem),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when inputMass has no data
testthat::test_that(desc = "Table 'inputMass' missing data", {
  
  tempMass <- testMass %>%
    dplyr::filter(uid == "coconut")
  
  testthat::expect_error(object = rootTableJoin(inputMass = tempMass,
                                                inputPool = testPool,
                                                inputChem = testChem),
                         regexp = "Table 'inputMass' has no data.")
})



### Test expected errors for issues with inputPool table
#   --> begin again here

