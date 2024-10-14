### Unit tests for standardizeRootMass function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "rootdatalist-201807.RDS"))
testMass <- testList$bbc_rootmass



### Test: Function generates expected output type
#   Test list input
testthat::test_that(desc = "Output type list input", {

  testthat::expect_type(object = standardizeRootMass(inputRootList = testList),
                        type = "list")

})

#   Test table input
testthat::test_that(desc = "Output type table input", {
  
  testthat::expect_type(object = standardizeRootMass(inputRootList = NA,
                                                     inputMass = testMass),
                        type = "list")
})



### Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list input", {

  testthat::expect_s3_class(object = standardizeRootMass(inputRootList = testList),
                            class = "data.frame")

})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  testthat::expect_s3_class(object = standardizeRootMass(inputRootList = NA,
                                                         inputMass = testMass),
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {

  testthat::expect_identical(object = nrow(standardizeRootMass(inputRootList = testList)),
                             expected = as.integer(159))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {

  testthat::expect_identical(object = ncol(standardizeRootMass(inputRootList = testList)),
                             expected = as.integer(10))
})



### Test: Generate expected errors for 'inputRootList'
#   Test 'inputRootList' is a list
testthat::test_that(desc = "Argument 'inputRootList' is list object", {
  
  testthat::expect_error(object = standardizeRootMass(inputRootList = testMass),
                         regexp = "Argument 'inputRootList' must be a list object")
})

#   Test 'inputRootList' contains required tables
testthat::test_that(desc = "Required tables present in 'inputRootList' input", {
  
  testthat::expect_error(object = standardizeRootMass(inputRootList = testList[3:5]),
                         regexp = "Required tables missing from 'inputRootList'")
})

#   Test 'inputMass' is NA if 'inputRootList' supplied
testthat::test_that(desc = "Table inputs NA when required", {
  
  testthat::expect_error(object = standardizeRootMass(inputRootList = testList,
                                                      inputMass = testMass),
                         regexp = "When 'inputRootList' is supplied the 'inputMass' argument must be NA")
})



### Test: Generate expected errors for 'inputMass'
#   Test 'inputMass' is data frame if 'inputRootList' is NA
testthat::test_that(desc = "Table input is data frame when required", {
  
  testthat::expect_error(object = standardizeRootMass(inputRootList = NA,
                                                      inputMass = testList),
                         regexp = "A data frame must be supplied for 'inputMass' if 'inputRootList' is NA")
})



### Tests: Generate expected errors with table inputs 
# Test when inputMass lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {
  
  testthat::expect_error(object = standardizeRootMass(inputRootList = NA,
                                                      inputMass = testMass %>%
                                                        dplyr::select(-dryMass)),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when inputMass has no data
testthat::test_that(desc = "Table 'inputMass' missing data", {
  
  testthat::expect_error(object = standardizeRootMass(inputRootList = NA,
                                                      inputMass = testMass %>%
                                                        dplyr::filter(uid == "mangrove")),
                         regexp = "Table 'inputMass' has no data.")
})

