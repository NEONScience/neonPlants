# estimateBiomass function tests
# Samuel M Simkin (2024-10-02)  ssimkin@battelleecology.org

### Read in test data

#VstHbpData <- getBiomassInputs(site="HARV", start = 2021, end = 2022, dataProducts = "Vst")

VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))
HbpDat <- readRDS(testthat::test_path("testdata", "HbpDat.rds"))

estimateBiomassOutputs <- estimateBiomass(inputVst = VstDat, inputHbp = HbpDat)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {

  testthat::expect_type(object = estimateBiomass(inputVst = VstDat, inputHbp = HbpDat),
                        type = "list")

})


### Test: Function generates expected output class
testthat::test_that(desc = "Output class", {
  
  testthat::expect_s3_class(object = estimateBiomassOutputs$vst_agb_per_ha,
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

