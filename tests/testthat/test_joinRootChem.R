### Unit tests for joinRootChem function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "rootdatalist-201807.RDS"))
testMass <- testList$bbc_rootmass
testPool <- testList$bbc_chemistryPooling
testChem <- testList$bbc_rootChemistry



### Test: Function generates expected output type
#   Test list input
testthat::test_that(desc = "Output type list input", {

  testthat::expect_type(object = joinRootChem(inputDataList = testList),
                        type = "list")

})

#   Test table input
testthat::test_that(desc = "Output type table input", {
  
  testthat::expect_type(object = joinRootChem(inputMass = testMass,
                                              inputPool = testPool,
                                              inputChem = testChem),
                        type = "list")
})



### Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list input", {

  testthat::expect_s3_class(object = joinRootChem(inputDataList = testList),
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  testthat::expect_s3_class(object = joinRootChem(inputMass = testMass,
                                                  inputPool = testPool,
                                                  inputChem = testChem),
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
##  Test list input
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number list input", {

  testthat::expect_identical(object = nrow(joinRootChem(inputDataList = testList)),
                             expected = as.integer(477))
})


#   Check expected column number of output
testthat::test_that(desc = "Output data frame column number list input", {

  testthat::expect_identical(object = ncol(joinRootChem(inputDataList = testList)),
                             expected = as.integer(35))
})


##  Test table inputs
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = nrow(joinRootChem(inputMass = testMass,
                                                        inputPool = testPool,
                                                        inputChem = testChem)),
                             expected = as.integer(477))
})

#   Check expected column number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = ncol(joinRootChem(inputMass = testMass,
                                                        inputPool = testPool,
                                                        inputChem = testChem)),
                             expected = as.integer(35))
})



### Tests: Generate expected errors for 'inputDataList' ####
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  
  testthat::expect_error(object = joinRootChem(inputDataList = testMass),
                         regexp = "Argument 'inputDataList' must be a list object")
})

#   Test 'inputDataList' contains required tables
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  
  testthat::expect_error(object = joinRootChem(inputDataList = testList[1:3]),
                         regexp = "Required tables missing from 'inputDataList'")
})

#   Test table inputs are NA if 'inputDataList' supplied
testthat::test_that(desc = "Table inputs NA when required", {
  
  testthat::expect_error(object = joinRootChem(inputDataList = testList,
                                               inputMass = testMass),
                         regexp = "When 'inputDataList' is supplied all table input arguments must be NA")
})



### Tests: Generate expected errors with table inputs ####
testthat::test_that(desc = "Table inputs are data frames when required", {
  
  testthat::expect_error(object = joinRootChem(inputMass = testList,
                                               inputPool = testPool,
                                               inputChem = testChem),
                         regexp = "Data frames must be supplied for all table inputs if 'inputDataList' is missing")
})



### Test: Generate expected errors for issues with rootMass table (works for inputDataList or inputMass source)
# Test when inputMass lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {

  testthat::expect_error(object = joinRootChem(inputMass = testMass %>%
                                                 dplyr::select(-dryMass),
                                               inputPool = testPool,
                                               inputChem = testChem),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when inputMass has no data
testthat::test_that(desc = "Table 'inputMass' missing data", {
  
  testthat::expect_error(object = joinRootChem(inputMass = testMass %>%
                                                 dplyr::filter(uid == "coconut"),
                                               inputPool = testPool,
                                               inputChem = testChem),
                         regexp = "Table 'inputMass' has no data.")
})



### Test: Generate expected errors for issues with inputPool table (works for inputDataList or inputPool source)
#   Test when inputPool lacks required column
testthat::test_that(desc = "Table 'inputPool' missing column", {
  
  testthat::expect_error(object = joinRootChem(inputMass = testMass,
                                               inputPool = testPool %>%
                                                 dplyr::select(-cnSampleID),
                                               inputChem = testChem),
                         regexp = "Required columns missing from 'inputPool': cnSampleID")
  
})

#   Test when inputPool has no data
testthat::test_that(desc = "Table 'inputPool' missing data", {
  
  testthat::expect_error(object = joinRootChem(inputMass = testMass,
                                               inputPool = testPool %>%
                                                 dplyr::filter(uid == "doppelganger"),
                                               inputChem = testChem),
                         regexp = "Table 'inputPool' has no data.")
  
})



### Test: Generate expected errors for issues with inputChem table
#   Test when inputChem lacks required column
testthat::test_that(desc = "Table 'inputChem' missing column", {
  
  testthat::expect_error(object = joinRootChem(inputMass = testMass,
                                               inputPool = testPool,
                                               inputChem = testChem %>%
                                                 dplyr::select(-d15N)),
                         regexp = "Required columns missing from 'inputChem': d15N")
  
})

#   Test when inputChem has no data
testthat::test_that(desc = "Table 'inputChem' missing data", {
  
  testthat::expect_error(object = joinRootChem(inputMass = testMass,
                                               inputPool = testPool,
                                               inputChem = testChem %>%
                                                 dplyr::filter(uid == "ministry")),
                         regexp = "Table 'inputChem' has no data.")
  
})

