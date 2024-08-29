### Unit tests for rootChemJoin function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testMass <- readRDS(testthat::test_path("testdata", "valid-rootmass-201807.RDS"))
testPool <- readRDS(testthat::test_path("testdata", "valid-rootpool-201807.RDS"))
testChem <- readRDS(testthat::test_path("testdata", "valid-rootchem-201807.RDS"))



### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  
  testthat::expect_type(object = rootChemJoin(inputMass = testMass,
                                               inputPool = testPool,
                                               inputChem = testChem),
                        type = "list")
  
})



### Test: Function generates expected output class
testthat::test_that(desc = "Output class", {
  
  testthat::expect_s3_class(object = rootChemJoin(inputMass = testMass,
                                                   inputPool = testPool,
                                                   inputChem = testChem),
                            class = "data.frame")
  
})



### Test: Generate expected errors for issues with inputMass table
# Test when inputMass lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {
  
  testthat::expect_error(object = rootChemJoin(inputMass = testMass %>%
                                                  dplyr::select(-dryMass),
                                                inputPool = testPool,
                                                inputChem = testChem),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when inputMass has no data
testthat::test_that(desc = "Table 'inputMass' missing data", {
  
  testthat::expect_error(object = rootChemJoin(inputMass = testMass %>%
                                                  dplyr::filter(uid == "coconut"),
                                                inputPool = testPool,
                                                inputChem = testChem),
                         regexp = "Table 'inputMass' has no data.")
})



### Test: Generate expected errors for issues with inputPool table
#   Test when inputPool lacks required column
testthat::test_that(desc = "Table 'inputPool' missing column", {
  
  testthat::expect_error(object = rootChemJoin(inputMass = testMass,
                                                inputPool = testPool %>%
                                                  dplyr::select(-cnSampleID),
                                                inputChem = testChem),
                         regexp = "Required columns missing from 'inputPool': cnSampleID")
  
})

#   Test when inputPool has no data
testthat::test_that(desc = "Table 'inputPool' missing data", {
  
  testthat::expect_error(object = rootChemJoin(inputMass = testMass,
                                                inputPool = testPool %>%
                                                  dplyr::filter(uid == "doppelganger"),
                                                inputChem = testChem),
                         regexp = "Table 'inputPool' has no data.")
  
})



### Test: Generate expected errors for issues with inputChem table 
#   Test when inputChem lacks required column
testthat::test_that(desc = "Table 'inputChem' missing column", {
  
  testthat::expect_error(object = rootChemJoin(inputMass = testMass,
                                                inputPool = testPool,
                                                inputChem = testChem %>%
                                                  dplyr::select(-d15N)),
                         regexp = "Required columns missing from 'inputChem': d15N")
  
})

#   Test when inputChem has no data
testthat::test_that(desc = "Table 'inputChem' missing data", {
  
  testthat::expect_error(object = rootChemJoin(inputMass = testMass,
                                                inputPool = testPool,
                                                inputChem = testChem %>%
                                                  dplyr::filter(uid == "ministry")),
                         regexp = "Table 'inputChem' has no data.")
  
})

