# estimateProductivity function tests
# Samuel M Simkin (2024-10-02)  ssimkin@battelleecology.org

### Read in test data

#VstHbpData <- getBiomassInputs(site="HARV", start = 2021, end = 2022, dataProducts = "VstHbp")

VstHbpData <- readRDS(testthat::test_path("testdata", "VstHbpData.rds"))
VstDat <- VstHbpData$Vst
HbpDat <- VstHbpData$Hbp

estimateBiomassOutputs <- estimateBiomass(inputDataListVst = VstDat, inputDataListHbp = HbpDat)
estimateProductivityOutputs <- estimateProductivity(input = estimateBiomassOutputs)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {

  testthat::expect_type(object = estimateProductivity(input = estimateProductivityOutputs),
                        type = "list")

})


### Test: Function generates expected output class
testthat::test_that(desc = "Output class", {
  
  testthat::expect_s3_class(object = estimateProductivityOutputs$vst_ANPP_plot_w_taxa,
                            class = "data.frame")
  
})  



### Test: Function generates data frame with expected dimensions using test data
#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {
  
  testthat::expect_identical(object = nrow(estimateProductivityOutputs$vst_ANPP_plot_w_taxa),
                             expected = as.integer(31))
})

#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {
  
  testthat::expect_identical(object = ncol(estimateProductivityOutputs$vst_ANPP_plot_w_taxa),
                             expected = as.integer(7))
})



### Test: Generate expected errors for issues with vst_plot_w_0s table
# Test when input vst_plot_w_0s lacks required column
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_plot_w_0s <- estimateBiomassOutputs_mod$vst_plot_w_0s %>% dplyr::select(-agb_Mg_per_ha__Live)
testthat::test_that(desc = "Table 'vst_plot_w_0s' missing column", {
  
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Required columns missing from 'vst_plot_w_0s': agb_Mg_per_ha__Live")
})


