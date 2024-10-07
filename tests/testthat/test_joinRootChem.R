### Unit tests for joinRootChem function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "valid-rootdatalist-201807.RDS"))
testMass <- testList$bbc_rootmass
testPool <- testList$bbc_chemistryPooling
testChem <- testList$bbc_rootChemistry



### Test: Function generates expected output type with list input
testthat::test_that(desc = "Output type", {

  testthat::expect_type(object = joinRootChem(inputRootList = testList),
                        type = "list")

})



### Test: Function generates expected output class with list input
testthat::test_that(desc = "Output class", {

  testthat::expect_s3_class(object = joinRootChem(inputRootList = testList),
                            class = "data.frame")

})



### Test: Function generates data frame with expected dimensions using test data
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {

  testthat::expect_identical(object = nrow(joinRootChem(inputRootList = testList)),
                             expected = as.integer(320))
})


#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {

  testthat::expect_identical(object = ncol(joinRootChem(inputRootList = testList)),
                             expected = as.integer(35))
})



### Tests: Generate expected errors for 'inputRootList' ####
#   Test 'inputRootList' is a list
testthat::test_that(desc = "Argument 'inputRootList' is list object", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = testMass),
                         regexp = "Argument 'inputRootList' must be a list object")
})

#   Test 'inputRootList' contains required tables
testthat::test_that(desc = "Required tables present in 'inputRootList' input", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = testList[1:3]),
                         regexp = "Required tables missing from 'inputRootList'")
})

#   Test table inputs are NA if 'inputRootList' supplied
testthat::test_that(desc = "Table inputs NA when required", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = testList,
                                               inputMass = testMass),
                         regexp = "When 'inputRootList' is supplied all table input arguments must be NA")
})



### Tests: Generate expected errors with table inputs ####
testthat::test_that(desc = "Table inputs are data frames when required", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = NA,
                                               inputMass = testList,
                                               inputPool = testPool,
                                               inputChem = testChem),
                         regexp = "Data frames must be supplied for all table inputs if 'inputRootList' is NA")
})



### Test: Generate expected errors for issues with rootMass table (works for inputRootList or inputMass source)
# Test when inputMass lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {

  testthat::expect_error(object = joinRootChem(inputRootList = NA,
                                               inputMass = testMass %>%
                                                 dplyr::select(-dryMass),
                                               inputPool = testPool,
                                               inputChem = testChem),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when inputMass has no data
testthat::test_that(desc = "Table 'inputMass' missing data", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = NA,
                                               inputMass = testMass %>%
                                                 dplyr::filter(uid == "coconut"),
                                               inputPool = testPool,
                                               inputChem = testChem),
                         regexp = "Table 'inputMass' has no data.")
})



### Test: Generate expected errors for issues with inputPool table (works for inputRootList or inputPool source)
#   Test when inputPool lacks required column
testthat::test_that(desc = "Table 'inputPool' missing column", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = NA,
                                               inputMass = testMass,
                                               inputPool = testPool %>%
                                                 dplyr::select(-cnSampleID),
                                               inputChem = testChem),
                         regexp = "Required columns missing from 'inputPool': cnSampleID")
  
})

#   Test when inputPool has no data
testthat::test_that(desc = "Table 'inputPool' missing data", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = NA,
                                               inputMass = testMass,
                                               inputPool = testPool %>%
                                                 dplyr::filter(uid == "doppelganger"),
                                               inputChem = testChem),
                         regexp = "Table 'inputPool' has no data.")
  
})



### Test: Generate expected errors for issues with inputChem table
#   Test when inputChem lacks required column
testthat::test_that(desc = "Table 'inputChem' missing column", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = NA,
                                               inputMass = testMass,
                                               inputPool = testPool,
                                               inputChem = testChem %>%
                                                 dplyr::select(-d15N)),
                         regexp = "Required columns missing from 'inputChem': d15N")
  
})

#   Test when inputChem has no data
testthat::test_that(desc = "Table 'inputChem' missing data", {
  
  testthat::expect_error(object = joinRootChem(inputRootList = NA,
                                               inputMass = testMass,
                                               inputPool = testPool,
                                               inputChem = testChem %>%
                                                 dplyr::filter(uid == "ministry")),
                         regexp = "Table 'inputChem' has no data.")
  
})

