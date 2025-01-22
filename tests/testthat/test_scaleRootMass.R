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

  testthat::expect_type(object = scaleRootMass(inputDataList = testList),
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
  
  temp <- scaleRootMass(inputDataList = testList)

  testthat::expect_s3_class(object = temp$coreRootMass,
                            class = "data.frame")
  testthat::expect_s3_class(object = temp$plotRootMass,
                            class = "data.frame")
  testthat::expect_s3_class(object = temp$siteRootMass,
                            class = "data.frame")
  
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  temp <- scaleRootMass(inputCore = testCore,
                        inputMass = testMass,
                        inputDilution = testDilution)
  
  testthat::expect_s3_class(object = temp$coreRootMass,
                            class = "data.frame")
  testthat::expect_s3_class(object = temp$plotRootMass,
                            class = "data.frame")
  testthat::expect_s3_class(object = temp$siteRootMass,
                            class = "data.frame")
  
})



### Test: Function generates "coreRootMass" data frame with expected dimensions using test data
##  Tests with includeDilution == TRUE (default) and includeFragInTotal == FALSE (default)
#   Check expected row number of data frame
testthat::test_that(desc = "Output 'coreRootMass' row number with defaults", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_identical(object = nrow(temp$coreRootMass),
                             expected = as.integer(53))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output 'coreRootMass' column number with defaults", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_identical(object = ncol(temp$coreRootMass),
                             expected = as.integer(51))
})


##  Tests with includeDilution == FALSE
#   Check expected row number of data frame
testthat::test_that(desc = "Output 'coreRootMass' row number includeDilution FALSE", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = FALSE)

  testthat::expect_identical(object = nrow(temp$coreRootMass),
                             expected = as.integer(53))

})

#   Check expected column number of data frame
testthat::test_that(desc = "Output 'coreRootMass' column number includeDilution FALSE", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = FALSE)

  testthat::expect_identical(object = ncol(temp$coreRootMass),
                             expected = as.integer(50))

})



### Test: Function generates "plotRootMass" data frame with expected dimensions using test data
##  Tests with includeDilution == TRUE (default) and includeFragInTotal == FALSE (default)
#   Check expected row number of data frame
testthat::test_that(desc = "Output 'plotRootMass' row number with defaults", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_identical(object = nrow(temp$plotRootMass),
                             expected = as.integer(20))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output 'plotRootMass' column number with defaults", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_identical(object = ncol(temp$plotRootMass),
                             expected = as.integer(16))
  
})



### Test: Function generates "siteRootMass" data frame with expected dimensions using test data
##  Tests with includeDilution == TRUE (default) and includeFragInTotal == FALSE (default)
#   Check expected row number of data frame
testthat::test_that(desc = "Output 'siteRootMass' row number with defaults", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_identical(object = nrow(temp$siteRootMass),
                             expected = as.integer(1))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output 'siteRootMass' column number with defaults", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_identical(object = ncol(temp$siteRootMass),
                             expected = as.integer(12))
  
})



### Test: Function includeFragInTotal correctly handles fragment mass
#   Check totalDryMass does not contain fragment mass with function defaults; row 3 has frag mass data
testthat::test_that(desc = "Output 'coreRootMass.totalDryMass' with includeFragInTotal FALSE", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE)
  
  testthat::expect_equal(object = temp$coreRootMass$totalDryMass[3],
                         expected = 8.1535)
  
})

#   Check totalDryMass includes fragment mass where appropriate when includeFragInTotal is TRUE
testthat::test_that(desc = "Output 'coreRootMass.totalDryMass' with includeFragInTotal TRUE", {
  
  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = TRUE)
  
  testthat::expect_equal(object = temp$coreRootMass$totalDryMass[3],
                         expected = 9.1953)
  
})



### Test: Generate expected error when 'inputDataList' is not a list
testthat::test_that(desc = "Arg 'inputDataList' is list object", {
  
  testthat::expect_error(object = scaleRootMass(inputDataList = "wish-i-were-a-list"),
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities")
})



### Test: Generate expected error when inputDataList is missing required table
#   Check when includeDilution is TRUE (default)
testthat::test_that(desc = "Input list missing table includeDilution TRUE", {
  
  testthat::expect_error(object = scaleRootMass(inputDataList = testList[1:2],
                                                includeDilution = TRUE),
                         regexp = "Required tables missing from 'inputDataList'")
})

#   Check when includeDilution is FALSE
testthat::test_that(desc = "Input list missing table includeDilution FALSE", {
  
  testthat::expect_error(object = scaleRootMass(inputDataList = testList[2:3],
                                                includeDilution = FALSE),
                         regexp = "Required tables missing from 'inputDataList'")
})



### Test: Arguments are LOGICAL when required
#   Check 'includeDilution' is logical with list input
testthat::test_that(desc = "Argument includeDilution is logical list input", {
  
  testthat::expect_error(object = scaleRootMass(inputDataList = testList,
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
  
  testthat::expect_error(object = scaleRootMass(inputDataList = testList,
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
testthat::test_that(desc = "Both 'inputDataList' and input tables supplied", {
  
  testthat::expect_error(object = scaleRootMass(inputDataList = testList,
                                                inputCore = testCore),
                         regexp = "When 'inputDataList' is supplied all table input arguments must be NA")
})



### Test: Generate error when 'inputDataList' missing and required tables not supplied
testthat::test_that(desc = "List missing and 'inputCore' missing", {
  
  testthat::expect_error(object = scaleRootMass(inputMass = testMass,
                                                inputDilution = testDilution),
                         regexp = "Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
})



### Test: Generate error when 'inputDataList' missing and 'inputDilution' is not a data frame
testthat::test_that(desc = "List missing and 'inputDilution' missing", {
  
  testthat::expect_error(object = scaleRootMass(includeDilution = TRUE,
                                                inputCore = testCore,
                                                inputMass = testMass,
                                                inputDilution = "chariot"),
                         regexp = "A data frame must be supplied to 'inputDilution' when 'inputDataList' is not provided")
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
