# estimateProductivity function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org

### Read in test data

VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))
HbpDat <- readRDS(testthat::test_path("testdata", "HbpDat.rds"))
VstDat <- VstDat
HbpDat <- HbpDat

estimateBiomassOutputs <- estimateBiomass(inputDataListVst = VstDat, inputDataListHbp = HbpDat)
estimateProductivityOutputs <- estimateProductivity(input = estimateBiomassOutputs)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateProductivity(input = estimateBiomassOutputs),
                        type = "list")
})


### Test: Function generates expected output class
testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateProductivityOutputs$increment_all,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateProductivityOutputs$increment_outlier,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateProductivityOutputs$vst_ANPP_plot_w_taxa,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateProductivityOutputs$vst_ANPP_plot,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateProductivityOutputs$vst_ANPP_site,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateProductivityOutputs$ANPP_site,
                            class = "data.frame")
})  



### Test: Function generates data frame with expected dimensions using test data
#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateProductivityOutputs$increment_all),
                             expected = as.integer(87))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateProductivityOutputs$increment_outlier),
                             expected = as.integer(8))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateProductivityOutputs$vst_ANPP_plot_w_taxa),
                             expected = as.integer(7))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateProductivityOutputs$vst_ANPP_plot),
                             expected = as.integer(7))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateProductivityOutputs$vst_ANPP_site),
                             expected = as.integer(9))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateProductivityOutputs$ANPP_site),
                             expected = as.integer(11))
})



#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateProductivityOutputs$increment_all),
                             expected = as.integer(25))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateProductivityOutputs$increment_outlier),
                             expected = as.integer(0))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateProductivityOutputs$vst_ANPP_plot_w_taxa),
                             expected = as.integer(5))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateProductivityOutputs$vst_ANPP_plot),
                             expected = as.integer(2))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateProductivityOutputs$vst_ANPP_site),
                             expected = as.integer(1))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateProductivityOutputs$ANPP_site),
                             expected = as.integer(1))
})


### Tests: Generate expected errors for input object ####
#   Test that input argument is a list
testthat::test_that(desc = "Argument 'input' is list object", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs$vst_agb_per_ha), # test whether function stops if supplied with a dataframe instead of list
                         regexp = "The input argument is expected to be a list")
})

#   Test that input object contains required tables (expect 7 if dataProducts = "VstHbp": vst_agb_per_ha", "vst_plot_w_0s", "vst_agb_zeros", "vst_site", "hbp_agb_per_ha", "hbp_plot", "VstHbp_site".
#                                                    expect only the first 4 if dataProducts = "Vst")
testthat::test_that(desc = "Required tables present in input object", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs[1:6], dataProducts ="VstHbp"),
                         regexp = "Required tables missing from input list")
})

testthat::test_that(desc = "Required tables present in input object", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs[1:3], dataProducts ="Vst"),
                         regexp = "Required tables missing from input list")
})



### Test: Generate expected errors for issues with vst_agb_per_ha table
# Test when input vst_agb_per_ha lacks required column
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_agb_per_ha <- estimateBiomassOutputs_mod$vst_agb_per_ha %>% dplyr::select(-agb_Mg_per_ha)
testthat::test_that(desc = "Table 'vst_agb_per_ha' missing column", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Required columns missing from 'vst_agb_per_ha': agb_Mg_per_ha")
})

#   Test when vst_agb_per_ha has no data
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_agb_per_ha <- estimateBiomassOutputs_mod$vst_agb_per_ha %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_agb_per_ha' missing data", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Table 'vst_agb_per_ha' has no data.")
})


### Test: Generate expected errors for issues with vst_agb_zeros table
# Test when input vst_agb_zeros lacks required column
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_agb_zeros <- estimateBiomassOutputs_mod$vst_agb_zeros %>% dplyr::select(-plotID)
testthat::test_that(desc = "Table 'vst_agb_zeros' missing column", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Required columns missing from 'vst_agb_zeros': plotID")
})

#   Test when vst_agb_zeros has no data
# estimateBiomassOutputs_mod <- estimateBiomassOutputs
# estimateBiomassOutputs_mod$vst_agb_zeros <- estimateBiomassOutputs_mod$vst_agb_zeros %>% dplyr::filter(year == "notRealyear")
# testthat::test_that(desc = "Table 'vst_agb_zeros' missing data", {
#   testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
#                          regexp = "Table 'vst_agb_zeros' has no data.")
# })

### Test: Generate expected errors for issues with vst_plot_w_0s table
# Test when input vst_plot_w_0s lacks required column
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_plot_w_0s <- estimateBiomassOutputs_mod$vst_plot_w_0s %>% dplyr::select(-agb_Mg_per_ha__Live)
testthat::test_that(desc = "Table 'vst_plot_w_0s' missing column", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Required columns missing from 'vst_plot_w_0s': agb_Mg_per_ha__Live")
})

#   Test when vst_plot_w_0s has no data
#VstDat_mod <- filter_df_in_list(VstDat, "vst_plot_w_0s")
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_plot_w_0s <- estimateBiomassOutputs_mod$vst_plot_w_0s %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_plot_w_0s' missing data", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Table 'vst_plot_w_0s' has no data.")
})

### Test: Generate expected errors for issues with vst_site table
# Test when input vst_site lacks required column
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_site <- estimateBiomassOutputs_mod$vst_site %>% dplyr::select(-vst_live_Mg_per_ha_ave)
testthat::test_that(desc = "Table 'vst_site' missing column", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Required columns missing from 'vst_site': vst_live_Mg_per_ha_ave")
})

#   Test when vst_site has no data
estimateBiomassOutputs_mod <- estimateBiomassOutputs
estimateBiomassOutputs_mod$vst_site <- estimateBiomassOutputs_mod$vst_site %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'vst_site' missing data", {
  testthat::expect_error(object = estimateProductivity(input = estimateBiomassOutputs_mod),
                         regexp = "Table 'vst_site' has no data.")
})



### Test: Generate error if output increment_all value not as expected
testthat::test_that(desc = "Output increment_all value as expected", {
  test <- estimateProductivity(input = estimateBiomassOutputs)
  testthat::expect_equal(object = test$increment_all$Mg_per_ha_per_yr_inc[1],
                         expected = 0.533726)
})


### Test: Generate error if output vst_ANPP_plot_w_taxa value not as expected
testthat::test_that(desc = "Output vst_ANPP_plot_w_taxa value as expected", {
  test <- estimateProductivity(input = estimateBiomassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_plot_w_taxa$wood_ANPP__Mg_ha_yr[1],
                         expected = 0.984351)
})


### Test: Generate error if output vst_ANPP_plot value not as expected
testthat::test_that(desc = "Output vst_ANPP_plot value as expected", {
  test <- estimateProductivity(input = estimateBiomassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_plot$wood_ANPP__Mg_ha_yr[1],
                         expected = 5.063526)
})

### Test: Generate error if output vst_ANPP_site value not as expected
testthat::test_that(desc = "Output vst_ANPP_site value as expected", {
  test <- estimateProductivity(input = estimateBiomassOutputs)
  testthat::expect_equal(object = test$vst_ANPP_site$wood_ANPP__Mg_ha_yr[1],
                         expected = 3.136)
})

### Test: Generate error if output ANPP_site value not as expected
testthat::test_that(desc = "Output ANPP_site value as expected", {
  test <- estimateProductivity(input = estimateBiomassOutputs)
  testthat::expect_equal(object = test$ANPP_site$wood_ANPP__Mg_ha_yr[1],
                         expected = 3.136)
})

