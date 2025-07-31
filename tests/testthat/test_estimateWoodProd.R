# estimateWoodProd function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org

### Read in test data

VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))

estimateWoodProdOutputs <- estimateWoodProd(inputDataList = VstDat, plotSubset = "towerAnnualSubset", siteID = "WREF")

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateWoodProd(inputDataList = VstDat, plotSubset = "towerAnnualSubset", siteID = "WREF"),
                        type = "list")
})


### Test: Function generates expected output class

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateWoodProdOutputs$vst_ANPP_plot,
                            class = "data.frame")
})

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateWoodProdOutputs$vst_ANPP_site,
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
#   Check expected column number of data frame


testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$vst_ANPP_plot),
                             expected = as.integer(13))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$vst_ANPP_site),
                             expected = as.integer(6))
})



#   Check expected row number of data frame

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$vst_ANPP_plot),
                             expected = as.integer(1))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$vst_ANPP_site),
                             expected = as.integer(1))
})


### Test: Generate error if output vst_ANPP_plot value not as expected
testthat::test_that(desc = "Output vst_ANPP_plot value as expected", {
  test <- estimateWoodProd(inputDataList = VstDat, plotSubset = "towerAnnualSubset", siteID = "WREF")
  testthat::expect_equal(object = test$vst_ANPP_plot$woodANPP_Mghayr[1],
                         expected = 2.73)
})

### Test: Generate error if output vst_ANPP_site value not as expected
testthat::test_that(desc = "Output vst_ANPP_site value as expected", {
  test <- estimateWoodProd(inputDataList = VstDat, plotSubset = "towerAnnualSubset", siteID = "WREF")
  testthat::expect_equal(object = test$vst_ANPP_site$woodANPPMean_Mghayr[1],
                         expected = 2.73)
})

