### Unit tests for rootMassScale function #########
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testCore <- readRDS(testthat::test_path("testdata", "valid-rootcore-201807.RDS"))
testMass <- readRDS(testthat::test_path("testdata", "valid-rootmass-201807.RDS"))
testDilution <- readRDS(testthat::test_path("testdata", "valid-rootdilution-201807.RDS"))



### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  
  testthat::expect_type(object = rootMassScale(inputCore = testCore,
                                               inputMass = testMass),
                        type = "list")
  
})



### Test: Function generates expected output class
testthat::test_that(desc = "Output class", {
  
  testthat::expect_s3_class(object = rootMassScale(inputCore = testCore,
                                                   inputMass = testMass),
                            class = "data.frame")
  
})



### Test: Function generates data frame with expected dimensions using test data
##  Tests with no inputDilution argument
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {
  
  testthat::expect_identical(object = nrow(rootMassScale(inputCore = testCore,
                                                         inputMass = testMass)),
                             expected = as.integer(455))
  
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {
  
  testthat::expect_identical(object = ncol(rootMassScale(inputCore = testCore,
                                                         inputMass = testMass)),
                             expected = as.integer(49))
  
})


##  Tests with inputDilution argument supplied and includeFragments == FALSE (default)
#   Check expected row number of data frame
testthat::test_that(desc = "Output row number with inputDilution arg", {
  
  testthat::expect_identical(object = nrow(rootMassScale(inputCore = testCore,
                                                         inputMass = testMass,
                                                         inputDilution = testDilution)),
                             expected = as.integer(455))
  
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output column number with inputDilution arg", {
  
  testthat::expect_identical(object = ncol(rootMassScale(inputCore = testCore,
                                                         inputMass = testMass,
                                                         inputDilution = testDilution)),
                             expected = as.integer(50))
  
})

#   Check expected totalDryMass does not contain fragment mass when latter is present
testthat::test_that(desc = "Output totalDryMass with includeFragments FALSE", {
  
  #   Row 3 of output data has fragment mass present
  
  
})


