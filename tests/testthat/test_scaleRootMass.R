### Unit tests for rootMassScale function #########
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "bbc_testDat.rds"))

testCore <- testList$bbc_percore
testMass <- testList$bbc_rootmass
testDilution <- testList$bbc_dilution

rmtest <- scaleRootMass(testList)
rmtestti <- scaleRootMass(inputCore = testCore,
                          inputMass = testMass,
                          inputDilution = testDilution)


### Test: Function generates expected output type
#   Test list input
testthat::test_that(desc = "Output type list input", {

  testthat::expect_type(object = rmtest,
                        type = "list")
})

#   Test table input
testthat::test_that(desc = "Output type table input", {

  testthat::expect_type(object = rmtestti,
                        type = "list")
})



### Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list input", {

  testthat::expect_s3_class(object = rmtest$bbc_core,
                            class = "data.frame")
  testthat::expect_s3_class(object = rmtest$bbc_plot,
                            class = "data.frame")
  testthat::expect_s3_class(object = rmtest$bbc_site,
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {

  testthat::expect_s3_class(object = rmtestti$bbc_core,
                            class = "data.frame")
  testthat::expect_s3_class(object = rmtestti$bbc_plot,
                            class = "data.frame")
  testthat::expect_s3_class(object = rmtestti$bbc_site,
                            class = "data.frame")
})



### Test: Function generates 'bbc_core' data frame with expected dimensions using test data
##  Tests with includeDilution == TRUE (default) and includeFragInTotal == FALSE (default)
#   Check expected row number of data frame
testthat::test_that(desc = "Output 'bbc_core' row number with defaults", {

  testthat::expect_identical(object = nrow(rmtest$bbc_core),
                             expected = as.integer(53))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output 'bbc_core' column number with defaults", {

  testthat::expect_identical(object = ncol(rmtest$bbc_core),
                             expected = as.integer(51))
})


##  Tests with includeDilution == FALSE
#   Check expected row and column numbers of data frame
testthat::test_that(desc = "Output 'bbc_core' row and col number includeDilution FALSE", {

  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = FALSE)

  testthat::expect_identical(object = nrow(temp$bbc_core),
                             expected = as.integer(53))

  testthat::expect_identical(object = ncol(temp$bbc_core),
                             expected = as.integer(50))
})



### Test: Function generates 'bbc_plot' data frame with expected dimensions using test data
##  Tests with includeDilution == TRUE (default) and includeFragInTotal == FALSE (default)
#   Check expected row and column numbers of data frame
testthat::test_that(desc = "Output 'bbc_plot' row and col number with defaults", {

  testthat::expect_identical(object = nrow(rmtest$bbc_plot),
                             expected = as.integer(20))

  testthat::expect_identical(object = ncol(rmtest$bbc_plot),
                             expected = as.integer(16))
})



### Test: Function generates 'bbc_site' data frame with expected dimensions using test data
##  Tests with includeDilution == TRUE (default) and includeFragInTotal == FALSE (default)
#   Check expected row and column numbers of data frame
testthat::test_that(desc = "Output 'bbc_site' row and col numbers with defaults", {

  testthat::expect_identical(object = nrow(rmtest$bbc_site),
                             expected = as.integer(1))

  testthat::expect_identical(object = ncol(rmtest$bbc_site),
                             expected = as.integer(13))
})



### Test: Function includeFragInTotal correctly handles fragment mass
#   Check totalDryMass does not contain fragment mass with function defaults; row 3 has frag mass data
testthat::test_that(desc = "Output 'bbc_core.totalDryMass' with includeFragInTotal FALSE", {

  testthat::expect_equal(object = rmtest$bbc_core$totalDryMass[3],
                         expected = 8.1535)
})

#   Check totalDryMass includes fragment mass where appropriate when includeFragInTotal is TRUE
testthat::test_that(desc = "Output 'bbc_core.totalDryMass' with includeFragInTotal TRUE", {

  temp <- scaleRootMass(inputDataList = testList,
                        includeDilution = TRUE,
                        includeFragInTotal = TRUE)

  testthat::expect_equal(object = temp$bbc_core$totalDryMass[3],
                         expected = 9.1953)
})

#   Test that plot mass is close to mean of core mass
testthat::test_that(desc = "Test that plot mass is close to mean of core mass", {
  testthat::expect_equal(object = mean(rmtest$bbc_core$totalMass_gm3
                                      [which(rmtest$bbc_core$plotID=="DEJU_047")]),
                         expected = mean(rmtest$bbc_plot$totalMass_gm3
                                         [which(rmtest$bbc_plot$plotID=="DEJU_047")]),
                         tolerance = 0.5)
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





### Output value tests ####

### Test: Verify unique sampleIDs in input data match those in 'bbc_core' output table
testthat::test_that(desc = "Output 'sampleIDs' match input 'sampleIDs'", {

  ##  Derive expected 'sampleIDs' from input data set
  inputSample <- testCore %>%
    dplyr::distinct(.data$sampleID) %>%
    dplyr::arrange(.data$sampleID)


  ##  Derive expected 'sampleIDs' from output data set
  outputSample <- rmtest$bbc_core %>%
    dplyr::distinct(.data$sampleID) %>%
    dplyr::arrange(.data$sampleID)


  ##  Conduct identical sampleID test
  testthat::expect_identical(object = outputSample$sampleID,
                             expected = inputSample$sampleID)
})



### Test: Verify 'plot-event' combos in input data match those in 'bbc_plot' output table
testthat::test_that(desc = "Output 'plot-events' match input 'plot-events'", {

  ##  Derive expected 'plot-events' from input data set
  inputPlotEvent <- testCore %>%
    dplyr::filter(.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical)) %>%
    dplyr::mutate(plotEvent = paste(.data$plotID, .data$eventID, sep = "-")) %>%
    dplyr::distinct(.data$plotEvent) %>%
    dplyr::arrange(.data$plotEvent)


  ##  Derive expected 'plot-events' from output data set
  outputPlotEvent <- rmtest$bbc_plot %>%
    dplyr::mutate(plotEvent = paste(.data$plotID, .data$eventID, sep = "-")) %>%
    dplyr::distinct(.data$plotEvent) %>%
    dplyr::arrange(.data$plotEvent)


  ##  Conduct identical plot-event test
  testthat::expect_identical(object = outputPlotEvent$plotEvent,
                             expected = inputPlotEvent$plotEvent)
})



### Test: Verify 'site-event' combos in input data match those in 'bbc_site' output table
testthat::test_that(desc = "Output 'site-events' match input 'site-events'", {

  ##  Derive expected 'site-events' from input data set
  inputSiteEvent <- testCore %>%
    dplyr::filter(.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical)) %>%
    dplyr::mutate(siteEvent = paste(.data$siteID, .data$eventID, sep = "-")) %>%
    dplyr::distinct(.data$siteEvent) %>%
    dplyr::arrange(.data$siteEvent)


  ##  Derive expected 'site-events' from output data set
  outputSiteEvent <- rmtest$bbc_site %>%
    dplyr::mutate(siteEvent = paste(.data$siteID, .data$eventID, sep = "-")) %>%
    dplyr::distinct(.data$siteEvent) %>%
    dplyr::arrange(.data$siteEvent)


  ##  Conduct identical plot-event test
  testthat::expect_identical(object = outputSiteEvent$siteEvent,
                             expected = inputSiteEvent$siteEvent)
})
