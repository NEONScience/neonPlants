# estimateWoodProd function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org

### Read in test data

VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))
VstDat <- VstDat

estimateWoodMassOutputs <- estimateWoodMass(inputDataList = VstDat)
estimateWoodProdOutputs <- estimateWoodProd(input = estimateWoodMassOutputs)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateWoodProd(input = estimateWoodMassOutputs),
                        type = "list")
})


### Test: Function generates expected output class
testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateWoodProdOutputs$increment_all,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateWoodProdOutputs$increment_outlier,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateWoodProdOutputs$vst_ANPP_plot_w_taxa,
                            class = "data.frame")
})  

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
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$increment_all),
                             expected = as.integer(87))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$increment_outlier),
                             expected = as.integer(8))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$vst_ANPP_plot_w_taxa),
                             expected = as.integer(7))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$vst_ANPP_plot),
                             expected = as.integer(7))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodProdOutputs$vst_ANPP_site),
                             expected = as.integer(6))
})



#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$increment_all),
                             expected = as.integer(28))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$increment_outlier),
                             expected = as.integer(0))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$vst_ANPP_plot_w_taxa),
                             expected = as.integer(7))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$vst_ANPP_plot),
                             expected = as.integer(2))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodProdOutputs$vst_ANPP_site),
                             expected = as.integer(1))
})



### Tests: Generate expected errors for input object ####
#   Test that input argument is a list
testthat::test_that(desc = "Argument 'input' is list object", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs$vst_agb_per_ha), # test whether function stops if supplied with a dataframe instead of list
                         regexp = "The input argument is expected to be a list")
})

#   Test that input object contains required tables (expect 4 tables: vst_agb_per_ha", "vst_plot_w_0s", "vst_agb_zeros", "vst_site")
testthat::test_that(desc = "Required tables present in input object", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs[1:3]),
                         regexp = "Required tables missing from input list")
})



### Test: Generate expected errors for issues with vst_agb_per_ha table
# Test when input vst_agb_per_ha lacks required column
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_agb_per_ha <- estimateWoodMassOutputs_mod$vst_agb_per_ha %>% dplyr::select(-agb_Mgha)
testthat::test_that(desc = "Table 'vst_agb_per_ha' missing column", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
                         regexp = "Required columns missing from 'vst_agb_per_ha': agb_Mgha")
})

#   Test when vst_agb_per_ha has no data
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_agb_per_ha <- estimateWoodMassOutputs_mod$vst_agb_per_ha %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_agb_per_ha' missing data", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
                         regexp = "Table 'vst_agb_per_ha' has no data.")
})


### Test: Generate expected errors for issues with vst_agb_zeros table
# Test when input vst_agb_zeros lacks required column
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_agb_zeros <- estimateWoodMassOutputs_mod$vst_agb_zeros %>% dplyr::select(-plotID)
testthat::test_that(desc = "Table 'vst_agb_zeros' missing column", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
                         regexp = "Required columns missing from 'vst_agb_zeros': plotID")
})

#   Test when vst_agb_zeros has no data
# estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
# estimateWoodMassOutputs_mod$vst_agb_zeros <- estimateWoodMassOutputs_mod$vst_agb_zeros %>% dplyr::filter(year == "notRealyear")
# testthat::test_that(desc = "Table 'vst_agb_zeros' missing data", {
#   testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
#                          regexp = "Table 'vst_agb_zeros' has no data.")
# })

### Test: Generate expected errors for issues with vst_plot_w_0s table
# Test when input vst_plot_w_0s lacks required column
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_plot_w_0s <- estimateWoodMassOutputs_mod$vst_plot_w_0s %>% dplyr::select(-agb_Mgha__Live)
testthat::test_that(desc = "Table 'vst_plot_w_0s' missing column", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
                         regexp = "Required columns missing from 'vst_plot_w_0s': agb_Mgha__Live")
})

#   Test when vst_plot_w_0s has no data
#VstDat_mod <- filter_df_in_list(VstDat, "vst_plot_w_0s")
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_plot_w_0s <- estimateWoodMassOutputs_mod$vst_plot_w_0s %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_plot_w_0s' missing data", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
                         regexp = "Table 'vst_plot_w_0s' has no data.")
})

### Test: Generate expected errors for issues with vst_site table
# Test when input vst_site lacks required column
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_site <- estimateWoodMassOutputs_mod$vst_site %>% dplyr::select(-woodLiveMassMean_Mgha)
testthat::test_that(desc = "Table 'vst_site' missing column", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
                         regexp = "Required columns missing from 'vst_site': woodLiveMassMean_Mgha")
})

#   Test when vst_site has no data
estimateWoodMassOutputs_mod <- estimateWoodMassOutputs
estimateWoodMassOutputs_mod$vst_site <- estimateWoodMassOutputs_mod$vst_site %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_site' missing data", {
  testthat::expect_error(object = estimateWoodProd(input = estimateWoodMassOutputs_mod),
                         regexp = "Table 'vst_site' has no data.")
})



### Test: Generate error if output increment_all value not as expected
testthat::test_that(desc = "Output increment_all value as expected", {
  test <- estimateWoodProd(input = estimateWoodMassOutputs)
  testthat::expect_equal(object = test$increment_all$Mgha_per_yr_inc[1],
                         expected = 0.533726)
})


### Test: Generate error if output vst_ANPP_plot_w_taxa value not as expected
testthat::test_that(desc = "Output vst_ANPP_plot_w_taxa value as expected", {
  test <- estimateWoodProd(input = estimateWoodMassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_plot_w_taxa$woodANPP_Mghayr[1],
                         expected = 0.984351)
})


### Test: Generate error if output vst_ANPP_plot value not as expected
testthat::test_that(desc = "Output vst_ANPP_plot value as expected", {
  test <- estimateWoodProd(input = estimateWoodMassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_plot$woodANPP_Mghayr[1],
                         expected = 5.063526)
})

### Test: Generate error if output vst_ANPP_site value not as expected
testthat::test_that(desc = "Output vst_ANPP_site value as expected", {
  test <- estimateWoodProd(input = estimateWoodMassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_site$woodANPPMean_Mghayr[1],
                         expected = 3.588)
})

