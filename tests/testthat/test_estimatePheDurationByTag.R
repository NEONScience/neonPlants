### Unit tests for estimatePheTransByTag function ####
### Developed by: Katie Jones, joneseyk@gmail.com
### POC: Courtney Meier, cmeier@BattelleEcology.org



### Read in test data ####
testList <- readRDS(testthat::test_path("testdata", "phe_testDat_GRSM.rds"))
testStatus <- testList$phe_statusintensity
testTags <- testList$phe_perindividual

testdur1 <- estimatePheDurationByTag(inputDataList = testList)
testdurti <- estimatePheDurationByTag(inputStatus = testStatus,
                                      inputTags = testTags)

#   Second set of test data - these are out of order and include phenophases that cross years, and multiple onsets within a year
testList2 <- readRDS(testthat::test_path("testdata", "phe_testDat_GUAN_SRER.rds"))
testdur <- estimatePheDurationByTag(inputDataList = testList2)



### Tests: Output class tests

##  Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list output", {

  testthat::expect_s3_class(object = testdur1$phe_duration_tag,
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table output", {

  testthat::expect_s3_class(object = testdurti$phe_duration_tag,
                            class = "data.frame")

})


##  Check expected column number of output
testthat::test_that(desc = "Output data frame column number list input", {

  #   Test list input
  testthat::expect_identical(object = ncol(testdur1$phe_duration_tag),
                             expected = as.integer(14))

  #   Test table input
  testthat::expect_identical(object = ncol(testdurti$phe_duration_tag),
                             expected = as.integer(14))
})


##  Check expected row number of output
testthat::test_that(desc = "Output data frame row number list input", {

  #   Test list input
  testthat::expect_identical(object = nrow(testdur1$phe_duration_tag),
                             expected = as.integer(22))

  #   Test table input
  testthat::expect_identical(object = nrow(testdurti$phe_duration_tag),
                             expected = as.integer(22))
})

#   Test transition count
testthat::test_that(desc = "Number of transitions matches expectation", {

  testthat::expect_identical(object = testdur$phe_duration_tag$nthTransition[which(testdur$phe_duration_tag$taxonID == 'PRVE' &
                                                                                     testdur$phe_duration_tag$yearPhenophaseBegan == 2021 &
                                                                                     testdur$phe_duration_tag$phenophaseName == 'Colored leaves')],
                             expected = 1:2)
})

#   Test duration
testthat::test_that(desc = "Duration of select phenophases matches expectation", {

  testthat::expect_identical(object = testdur$phe_duration_tag$duration[which(testdur$phe_duration_tag$taxonID == 'LELE10' &
                                                             testdur$phe_duration_tag$phenophaseName == 'Young leaves')],
                             expected = c(7,13,56,189,27,189,90,28,21,84,49,35,60,30,49,28,56,21))
})




