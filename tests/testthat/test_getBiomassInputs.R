# getBiomassInputs function tests
# Samuel M Simkin (2024-12-16)  ssimkin@battelleecology.org

### Read in test data

#VstHbpData <- readRDS(testthat::test_path("testdata", "VstHbpData.rds"))

VstHbpData <- getBiomassInputs(site="SJER", start = 2021, end = 2022, dataProducts = "VstHbp")
VstDat <- VstHbpData$Vst
HbpDat <- VstHbpData$Hbp

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = VstDat,
                        type = "list")
})


### Test: Function generates expected output class
testthat::test_that(desc = "Output vst_apparentindividual", {
  testthat::expect_s3_class(object = VstDat$vst_apparentindividual,
                            class = "data.frame")
})  


### Tests: Generate expected errors 
testthat::test_that(desc = "Argument 'dataProducts' is an allowable value", {
  testthat::expect_error(object = getBiomassInputs(site="SJER", start = 2021, end = 2022, dataProducts = "xyz"), 
                         regexp = "Currently the only valid dataProducts options are 'VstHbp' or 'Vst'")
})

testthat::test_that(desc = "Argument 'site' is an allowable value", {
  testthat::expect_error(object = getBiomassInputs(site = 111, start = 2021, end = 2022, dataProducts = "VstHbp"), 
                         regexp = "The site argument is expected to be either 'all', a four-letter NEON siteID, or set of NEON siteIDs")
})

testthat::test_that(desc = "Argument 'start' is an allowable value", {
  testthat::expect_error(object = getBiomassInputs(site="SJER", start = "now", end = 2022, dataProducts = "VstHbp"), 
                         regexp = "The start argument is expected to be numeric")
})

testthat::test_that(desc = "Argument 'end' is an allowable value", {
  testthat::expect_error(object = getBiomassInputs(site="SJER", start = 2021, end = "later", dataProducts = "VstHbp"), 
                         regexp = "The end argument is expected to be numeric")
})

