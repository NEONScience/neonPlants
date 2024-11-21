# estimateBiomass function tests
# Samuel M Simkin (2024-10-02)  ssimkin@battelleecology.org

### Read in test data

#VstHbpData <- getBiomassInputs(site="HARV", start = 2021, end = 2022, dataProducts = "Vst")

VstHbpData <- readRDS(testthat::test_path("testdata", "VstHbpData.rds"))
VstDat <- VstHbpData$Vst
HbpDat <- VstHbpData$Hbp

#estimateBiomassOutputs <- estimateBiomass(inputDataListVst = VstDat, inputDataListHbp = HbpDat)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateBiomass(inputDataListVst = VstDat, inputDataListHbp = HbpDat),
                        type = "list")
})


### Test: Function generates expected output class
testthat::test_that(desc = "Output class vst_agb_per_ha", {
  testthat::expect_s3_class(object = estimateBiomassOutputs$vst_agb_per_ha,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class vst_plot_w_0s", {
  testthat::expect_s3_class(object = estimateBiomassOutputs$vst_plot_w_0s,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class vst_agb_zeros", {
  testthat::expect_s3_class(object = estimateBiomassOutputs$vst_agb_zeros,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class vst_site", {
  testthat::expect_s3_class(object = estimateBiomassOutputs$vst_site,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class hbp_agb_per_ha", {
  testthat::expect_s3_class(object = estimateBiomassOutputs$hbp_agb_per_ha,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class hbp_plot", {
  testthat::expect_s3_class(object = estimateBiomassOutputs$hbp_plot,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class VstHbp_site", {
  testthat::expect_s3_class(object = estimateBiomassOutputs$VstHbp_site,
                            class = "data.frame")
})  


### Test: Function generates data frame with expected dimensions using test data
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateBiomassOutputs$vst_agb_per_ha),
                             expected = as.integer(767))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateBiomassOutputs$vst_agb_per_ha),
                             expected = as.integer(11))
})

