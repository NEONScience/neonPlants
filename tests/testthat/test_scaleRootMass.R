### Unit tests for rootMassScale function #########
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "rootdatalist-201807.RDS"))
testCore <- testList$bbc_percore
testMass <- testList$bbc_rootmass
testDilution <- testList$bbc_dilution



### Test: Function generates expected output type
#   Test list input
testthat::test_that(desc = "Output type list input", {

  testthat::expect_type(object = scaleRootMass(inputRootList = testList),
                        type = "list")
})

#   Test table input
testthat::test_that(desc = "Output type table input", {
  
  testthat::expect_type(object = scaleRootMass(inputCore = testCore,
                                               inputMass = testMass,
                                               inputDilution = testDilution),
                        type = "list")
})



### Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list input", {

  testthat::expect_s3_class(object = scaleRootMass(inputRootList = testList),
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  testthat::expect_s3_class(object = scaleRootMass(inputCore = testCore,
                                                   inputMass = testMass,
                                                   inputDilution = testDilution),
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
##  Tests with includeDilution == TRUE (default) and includeFragInTotal == FALSE (default)
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number with defaults", {
  
  testthat::expect_identical(object = nrow(scaleRootMass(inputRootList = testList,
                                                         includeDilution = TRUE,
                                                         includeFragInTotal = FALSE)),
                             expected = as.integer(53))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number with defaults", {
  
  testthat::expect_identical(object = ncol(scaleRootMass(inputRootList = testList,
                                                         includeDilution = TRUE,
                                                         includeFragInTotal = FALSE)),
                             expected = as.integer(50))
})


##  Tests with includeDilution == FALSE
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number includeDilution FALSE", {

  testthat::expect_identical(object = nrow(scaleRootMass(inputRootList = testList,
                                                         includeDilution = FALSE)),
                             expected = as.integer(53))

})

#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number includeDilution FALSE", {

  testthat::expect_identical(object = ncol(scaleRootMass(inputRootList = testList,
                                                         includeDilution = FALSE)),
                             expected = as.integer(49))

})



### Test: Function includeFragInTotal correctly handles fragment mass
#   Check totalDryMass does not contain fragment mass with function defaults; row 3 has frag mass data
testthat::test_that(desc = "Output totalDryMass with includeFragInTotal FALSE", {
  
  test <- scaleRootMass(inputRootList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_equal(object = test$totalDryMass[3],
                         expected = 8.1535)
  
})

#   Check totalDryMass includes fragment mass where appropriate when includeFragInTotal is TRUE
testthat::test_that(desc = "Output totalDryMass with includeFragInTotal TRUE", {
  
  test <- scaleRootMass(inputRootList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = TRUE)
  
  testthat::expect_equal(object = test$totalDryMass[3],
                         expected = 9.1953)
  
})



### Test: Generate expected error when 'inputRootList' is not a list
testthat::test_that(desc = "Arg 'inputRootList' is list object", {
  
  testthat::expect_error(object = scaleRootMass(inputRootList = "wish-i-were-a-list"),
                         regexp = "Argument 'inputRootList' must be a list object from neonUtilities")
})



### Test: Generate expected error when inputRootList is missing required table
#   Check when includeDilution is TRUE (default)
testthat::test_that(desc = "Input list missing table includeDilution TRUE", {
  
  testthat::expect_error(object = scaleRootMass(inputRootList = testList[1:2],
                                                includeDilution = TRUE),
                         regexp = "Required tables missing from 'inputRootList'")
})

#   Check when includeDilution is FALSE
testthat::test_that(desc = "Input list missing table includeDilution FALSE", {
  
  testthat::expect_error(object = scaleRootMass(inputRootList = testList[2:3],
                                                includeDilution = FALSE),
                         regexp = "Required tables missing from 'inputRootList'")
})



### Test: Arguments are LOGICAL when required
#   Check 'includeDilution' is logical with list input
testthat::test_that(desc = "Argument includeDilution is logical list input", {
  
  testthat::expect_error(object = scaleRootMass(inputRootList = testList,
                                                includeDilution = "toast"),
                         regexp = "Argument 'includeDilution' must be type logical")
})

#   Check 'includeDilution' is logical with table input
testthat::test_that(desc = "Argument includeDilution is logical table input", {
  
  testthat::expect_error(object = scaleRootMass(includeDilution = "bacon",
                                                inputCore = testCore,
                                                inputMass = testMass,
                                                inputDilution = testDilution),
                         regexp = "Argument 'includeDilution' must be type logical")
})

#   Check 'includeFragInTotal' is logical with list input
testthat::test_that(desc = "Argument includeFragInTotal is logical list input", {
  
  testthat::expect_error(object = scaleRootMass(inputRootList = testList,
                                                includeFragInTotal = as.integer(1)),
                         regexp = "Argument 'includeFragInTotal' must be type logical")
})

#   Check 'includeFragInTotal' is logical with table input
testthat::test_that(desc = "Argument includeFragInTotal is logical table input", {
  
  testthat::expect_error(object = scaleRootMass(inputCore = testCore,
                                                inputMass = testMass,
                                                inputDilution = testDilution,
                                                includeFragInTotal = data.frame()),
                         regexp = "Argument 'includeFragInTotal' must be type logical")
})



### Test: Generate expected error when input list AND tables supplied
testthat::test_that(desc = "Both 'inputRootList' and input tables supplied", {
  
  testthat::expect_error(object = scaleRootMass(inputRootList = testList,
                                                inputCore = testCore),
                         regexp = "When 'inputRootList' is supplied all table input arguments must be NA")
})



### Test: Generate error when 'inputRootList' missing and required tables not supplied
testthat::test_that(desc = "List missing and 'inputCore' missing", {
  
  testthat::expect_error(object = scaleRootMass(inputMass = testMass,
                                                inputDilution = testDilution),
                         regexp = "Data frames must be supplied for all table inputs if 'inputRootList' is not provided")
})



### Test: Generate error when 'inputRootList' missing and 'inputDilution' is not a data frame
testthat::test_that(desc = "List missing and 'inputDilution' missing", {
  
  testthat::expect_error(object = scaleRootMass(includeDilution = TRUE,
                                                inputCore = testCore,
                                                inputMass = testMass,
                                                inputDilution = "chariot"),
                         regexp = "A data frame must be supplied to 'inputDilution' when 'inputRootList' is not provided")
})



### Test: Generate error when 'includeDilution' is FALSE and 'includeFragInTotal' is TRUE
testthat::test_that(desc = "Args 'includeDilution' FALSE and 'includeFragInTotal' TRUE", {
  
  testthat::expect_error(object = scaleRootMass(includeDilution = FALSE,
                                                inputCore = testCore,
                                                inputMass = testMass,
                                                inputDilution = testDilution,
                                                includeFragInTotal = TRUE),
                         regexp = "Valid dilution sampling data must be provided and 'includeDilution' must be TRUE")
})



### Test: Generate expected errors for issues with inputCore table
#   Test when inputCore lacks required column
testthat::test_that(desc = "Table 'inputCore' missing column", {

  testthat::expect_error(object = scaleRootMass(inputCore = testCore %>%
                                                  dplyr::select(-rootSampleArea),
                                                inputMass = testMass,
                                                includeDilution = FALSE),
                         regexp = "Required columns missing from 'inputCore': rootSampleArea")
})

#   Test when inputCore has no data
testthat::test_that(desc = "Table 'inputCore' has no data", {

  testthat::expect_error(object = scaleRootMass(inputCore = testCore %>%
                                                  dplyr::filter(rootSamplingMethod == "spade"),
                                                inputMass = testMass,
                                                includeDilution = FALSE),
                         regexp = "Table 'inputCore' has no data.")
})



### Test: Generate expected errors for issues with inputMass table
#   Test when inputMass lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {

  testthat::expect_error(object = scaleRootMass(inputCore = testCore,
                                                inputMass = testMass %>%
                                                  dplyr::select(-dryMass),
                                                includeDilution = FALSE),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when inputMass has no data
testthat::test_that(desc = "Table 'inputMass' has no data", {

  testthat::expect_error(object = scaleRootMass(inputCore = testCore,
                                                inputMass = testMass %>%
                                                  dplyr::filter(sizeCategory == "pickle"),
                                                includeDilution = FALSE),
                         regexp = "Table 'inputMass' has no data.")
})



### Test: Generate expected errors for issues with inputDilution table
#   Test when inputDilution lacks required column
testthat::test_that(desc = "Table 'inputDilution' missing column", {

  testthat::expect_error(object = scaleRootMass(inputCore = testCore,
                                                inputMass = testMass,
                                                inputDilution = testDilution %>%
                                                  dplyr::select(-sampleVolume)),
                         regexp = "Required columns missing from 'inputDilution': sampleVolume")
})

#   Test when inputDilution has no data
testthat::test_that(desc = "Table 'inputDilution' has no data", {

  testthat::expect_error(object = scaleRootMass(inputCore = testCore,
                                                inputMass = testMass,
                                                inputDilution = testDilution %>%
                                                  dplyr::filter(sampleID == "zappa")),
                         regexp = "Table 'inputDilution' has no data.")
})
