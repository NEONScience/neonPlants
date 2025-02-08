### Unit tests for estimatePheTransByTag function ####
### POC: Katie Jones, kjones@BattelleEcology.org



### Read in test data ####
testList <- readRDS(testthat::test_path("testdata", "phe_testDat_GRSM.rds"))
testStatus <- testList$phe_statusintensity
testTags <- testList$phe_perindividual



##  Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list output", {
  
  testthat::expect_s3_class(object = estimatePheDurationByTag(inputDataList = testList),
                            class = "data.frame")
  
})

#   Test table input
testthat::test_that(desc = "Output class table output", {
  
  testthat::expect_s3_class(object = estimatePheDurationByTag(inputStatus = testStatus,
                                                              inputTags = testTags),
                            class = "data.frame")
  
})


##  Check expected column number of output
#   Test list input
testthat::test_that(desc = "Output data frame column number list input", {
  
  testthat::expect_identical(object = ncol(estimatePheDurationByTag(inputDataList = testList)),
                             expected = as.integer(14))
  
})

#   Test table input
testthat::test_that(desc = "Output data frame column number table input", {
  
  testthat::expect_identical(object = ncol(estimatePheDurationByTag(inputStatus = testStatus,
                                                                    inputTags = testTags)),
                             expected = as.integer(14))
  
})


##  Check expected row number of output
#   Test list input
testthat::test_that(desc = "Output data frame row number list input", {
  
  testthat::expect_identical(object = nrow(estimatePheDurationByTag(inputDataList = testList)),
                             expected = as.integer(22))
  
})

#   Test table input
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = nrow(estimatePheDurationByTag(inputStatus = testStatus,
                                                                    inputTags = testTags)),
                             expected = as.integer(22))
  
})
