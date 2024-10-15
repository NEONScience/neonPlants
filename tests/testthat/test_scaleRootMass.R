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
  
  testthat::expect_type(object = scaleRootMass(inputRootList = NA,
                                               inputCore = testCore,
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
  
  testthat::expect_s3_class(object = scaleRootMass(inputRootList = NA,
                                                   inputCore = testCore,
                                                   inputMass = testMass,
                                                   inputDilution = testDilution),
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
##  Tests with no inputDilution argument
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {

  testthat::expect_identical(object = nrow(scaleRootMass(inputRootList = testList,
                                                         includeDilution = FALSE)),
                             expected = as.integer(53))

})

#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {

  testthat::expect_identical(object = ncol(scaleRootMass(inputRootList = testList,
                                                         includeDilution = FALSE)),
                             expected = as.integer(49))

})


# ##  Tests with inputDilution argument supplied and includeFragments == FALSE (default)
# #   Check expected row number of data frame
# testthat::test_that(desc = "Output row number with inputDilution arg", {
#   
#   testthat::expect_identical(object = nrow(rootMassScale(inputCore = testCore,
#                                                          inputMass = testMass,
#                                                          inputDilution = testDilution)),
#                              expected = as.integer(455))
#   
# })
# 
# #   Check expected column number of data frame
# testthat::test_that(desc = "Output column number with inputDilution arg", {
#   
#   testthat::expect_identical(object = ncol(rootMassScale(inputCore = testCore,
#                                                          inputMass = testMass,
#                                                          inputDilution = testDilution)),
#                              expected = as.integer(50))
#   
# })
# 
# #   Check expected totalDryMass does not contain fragment mass when latter is present
# testthat::test_that(desc = "Output totalDryMass with includeFragments FALSE", {
#   
#   #   Row 3 of output data has fragment mass present but not included in totalDryMass
#   test <- rootMassScale(inputCore = testCore,
#                         inputMass = testMass,
#                         inputDilution = testDilution,
#                         includeFragments = FALSE)
#   
#   testthat::expect_equal(object = test$totalDryMass[3],
#                          expected = 5.5857)
#   
# })
# 
# 
# ##  Tests with inputDilution argument supplied and includeFragments == TRUE
# #   Check expected totalDryMass includes fragment mass for record where fragment mass is present
# testthat::test_that(desc = "Output totalDryMass with includeFragments TRUE", {
#   
#   #   Row 3 of output data has fragment mass present and included in totalDryMass
#   test <- rootMassScale(inputCore = testCore,
#                         inputMass = testMass,
#                         inputDilution = testDilution,
#                         includeFragments = TRUE)
#   
#   testthat::expect_equal(object = test$totalDryMass[3],
#                          expected = 7.4667)
#   
# })
# 
# 
# 
# ### Test: Generate expected errors for issues with inputCore table
# #   Test when inputCore lacks required column
# testthat::test_that(desc = "Table 'inputCore' missing column", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore %>%
#                                                   dplyr::select(-rootSampleArea),
#                                                 inputMass = testMass),
#                          regexp = "Required columns missing from 'inputCore': rootSampleArea")
# })
# 
# #   Test when inputCore has no data
# testthat::test_that(desc = "Table 'inputCore' has no data", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore %>%
#                                                   dplyr::filter(rootSamplingMethod == "spade"),
#                                                 inputMass = testMass),
#                          regexp = "Table 'inputCore' has no data.")
# })
# 
# 
# 
# ### Test: Generate expected errors for issues with inputMass table
# #   Test when inputMass lacks required column
# testthat::test_that(desc = "Table 'inputMass' missing column", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore,
#                                                 inputMass = testMass %>%
#                                                   dplyr::select(-dryMass)),
#                          regexp = "Required columns missing from 'inputMass': dryMass")
# })
# 
# #   Test when inputMass has no data
# testthat::test_that(desc = "Table 'inputMass' has no data", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore,
#                                                 inputMass = testMass %>%
#                                                   dplyr::filter(sizeCategory == "pickle")),
#                          regexp = "Table 'inputMass' has no data.")
# })
# 
# 
# 
# ### Test: Generate expected errors for issues with inputDilution table
# #   Test when inputDilution lacks required column
# testthat::test_that(desc = "Table 'inputDilution' missing column", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore,
#                                                 inputMass = testMass,
#                                                 inputDilution = testDilution %>%
#                                                   dplyr::select(-sampleVolume)),
#                          regexp = "Required columns missing from 'inputDilution': sampleVolume")
# })
# 
# #   Test when inputDilution has no data
# testthat::test_that(desc = "Table 'inputDilution' has no data", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore,
#                                                 inputMass = testMass,
#                                                 inputDilution = testDilution %>%
#                                                   dplyr::filter(sampleID == "zappa")),
#                          regexp = "Table 'inputDilution' has no data.")
# })
# 
# 
# 
# ### Test: Argument includeFragments is not LOGICAL
# testthat::test_that(desc = "Argument includeFragments is not logical", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore,
#                                                 inputMass = testMass,
#                                                 inputDilution = testDilution,
#                                                 includeFragments = "TOAST"),
#                          regexp = "The 'inputFragments' argument must be of type logical.")
# })
# 
# 
# 
# ### Test: inputDilution provided when includeFragments TRUE
# testthat::test_that(desc = "Data 'inputDilution' present when includeFragments TRUE", {
#   
#   testthat::expect_error(object = rootMassScale(inputCore = testCore,
#                                                 inputMass = testMass,
#                                                 includeFragments = TRUE),
#                          regexp = "A valid 'inputDilution' data frame must be provided when 'includeFragments' is TRUE.")
# })
