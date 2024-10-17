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



### Test: Generate expected errors for issues with inputCore table
#   Test when inputCore lacks required column
testthat::test_that(desc = "Table 'inputCore' missing column", {

  testthat::expect_error(object = scaleRootMass(inputRootList = NA,
                                                inputCore = testCore %>%
                                                  dplyr::select(-rootSampleArea),
                                                inputMass = testMass,
                                                includeDilution = FALSE),
                         regexp = "Required columns missing from 'inputCore': rootSampleArea")
})

#   Test when inputCore has no data
testthat::test_that(desc = "Table 'inputCore' has no data", {

  testthat::expect_error(object = scaleRootMass(inputRootList = NA,
                                                inputCore = testCore %>%
                                                  dplyr::filter(rootSamplingMethod == "spade"),
                                                inputMass = testMass,
                                                includeDilution = FALSE),
                         regexp = "Table 'inputCore' has no data.")
})



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
