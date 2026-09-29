### Unit tests for joinRootChem function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "bbc_testDat.rds"))

testMass <- testList$bbc_rootmass
testPool <- testList$bbc_chemistryPooling
testChem <- testList$bbc_rootChemistry

rctest <- joinRootChem(inputDataList = testList)
rctestti <- joinRootChem(inputMass = testMass,
                         inputPool = testPool,
                         inputChem = testChem)

### Test: Function generates expected output types
testthat::test_that(desc = "Output type list input", {

  #   Test list input
  testthat::expect_type(object = rctest,
                        type = "list")

  testthat::expect_type(object = rctestti,
                        type = "list")
})



### Test: Function generates expected output class
testthat::test_that(desc = "Output class list input", {

  #   Test list input
  testthat::expect_s3_class(object = rctest$bbc_joinedMassChem,
                            class = "data.frame")

  #   Test table input
  testthat::expect_s3_class(object = rctestti$bbc_joinedMassChem,
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
#   Check expected dimensions of output
testthat::test_that(desc = "Output data frame dimensions", {

  testthat::expect_identical(object = nrow(rctest$bbc_joinedMassChem),
                             expected = as.integer(477))

  testthat::expect_identical(object = ncol(rctest$bbc_joinedMassChem),
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

#   Test that there are no chemistry data for any dead roots
testthat::test_that(desc = "Test that there are no chemistry data for dead roots", {

  testthat::expect_true(object = all(is.na(rctest$bbc_joinedMassChem$carbonPercent[which(rctest$bbc_joinedMassChem$rootStatus == "dead")])))

  testthat::expect_true(object = all(is.na(rctest$bbc_joinedMassChem$d15N[which(rctest$bbc_joinedMassChem$rootStatus == "dead")])))
})

#   Test that all samples are accounted for
testthat::test_that(desc = "Test that all samples from original chemistry data are in output", {

  testthat::expect_true(object = all(testChem$cnSampleID %in% rctest$bbc_joinedMassChem$cnSampleID))
})
