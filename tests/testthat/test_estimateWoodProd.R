# estimateWoodProd function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org

### Read in test data

VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))

estimateWoodMassOutputs <- estimateWoodMass(inputDataList = VstDat, plotSubset = "towerAnnualSubset", growthForm = "tree")
estimateWoodProdOutputs <- estimateWoodProd(inputDataList = estimateWoodMassOutputs, plotSubset = "towerAnnualSubset")

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs),
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
                             expected = as.integer(9))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$vst_ANPP_site),
                             expected = as.integer(5))
})



#   Check expected row number of data frame

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$vst_ANPP_plot),
                             expected = as.integer(4))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$vst_ANPP_site),
                             expected = as.integer(3))
})



### Tests: Generate expected errors for inputDataList object ####
#   Test that inputDataList argument is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs$vst_agb_kg), # test whether function stops if supplied with a dataframe instead of list
                         regexp = "The 'inputDataList' argument is expected to be a list")
})

#   Test that inputDataList object contains required tables (expect 4 tables: vst_agb_kg", "vst_plot_w_0s", "vst_agb_zeros", "vst_site")
testthat::test_that(desc = "Required tables present in inputDataList object", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs[1:3]),
                         regexp = "Required tables missing from inputDataList list")
})



### Test: Generate expected errors for issues with vst_agb_kg table
# Test when input vst_agb_ka lacks required column
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_agb_kg <- estimateWoodMassOutputs_mod$vst_agb_kg %>% dplyr::select(-agb_kg)
testthat::test_that(desc = "Table 'vst_agb_kg' missing column", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs_mod),
                         regexp = "Required columns missing from 'vst_agb_kg': agb_kg")
})

#   Test when vst_agb_kg has no data
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_agb_kg <- estimateWoodMassOutputs_mod$vst_agb_kg %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_agb_kg' missing data", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs_mod),
                         regexp = "Table 'vst_agb_kg' has no data.")
})


### Test: Generate expected errors for issues with vst_agb_zeros table


### Test: Generate expected errors for issues with vst_plot_w_0s table
# Test when input vst_plot_w_0s lacks required column
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_plot_w_0s <- estimateWoodMassOutputs_mod$vst_plot_w_0s %>% dplyr::select(-Live_Mgha)
testthat::test_that(desc = "Table 'vst_plot_w_0s' missing column", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs_mod),
                         regexp = "Required columns missing from 'vst_plot_w_0s': Live_Mgha")
})

#   Test when vst_plot_w_0s has no data
#VstDat_mod <- filter_df_in_list(VstDat, "vst_plot_w_0s")
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_plot_w_0s <- estimateWoodMassOutputs_mod$vst_plot_w_0s %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_plot_w_0s' missing data", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs_mod),
                         regexp = "Table 'vst_plot_w_0s' has no data.")
})

### Test: Generate expected errors for issues with vst_site table
# Test when input vst_site lacks required column
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_site <- estimateWoodMassOutputs_mod$vst_site %>% dplyr::select(-woodLiveMassMean_Mgha)
testthat::test_that(desc = "Table 'vst_site' missing column", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs_mod),
                         regexp = "Required columns missing from 'vst_site': woodLiveMassMean_Mgha")
})

#   Test when vst_site has no data
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_site <- estimateWoodMassOutputs_mod$vst_site %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_site' missing data", {
  testthat::expect_error(object = estimateWoodProd(inputDataList = estimateWoodMassOutputs_mod),
                         regexp = "Table 'vst_site' has no data.")
})


### Test: Generate error if output vst_ANPP_plot value not as expected
testthat::test_that(desc = "Output vst_ANPP_plot value as expected", {
  test <- estimateWoodProd(inputDataList = estimateWoodMassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_plot$woodANPP_Mghayr[2],
                         expected = 3.334)
})

### Test: Generate error if output vst_ANPP_site value not as expected
testthat::test_that(desc = "Output vst_ANPP_site value as expected", {
  test <- estimateWoodProd(inputDataList = estimateWoodMassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_site$woodANPPMean_Mghayr[2],
                         expected = 2.2705)
})

