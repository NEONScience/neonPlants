# estimateWoodMass function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org



### Read in test data

VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))

estimateWoodMassOutputs <- estimateWoodMass(inputDataList = VstDat)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateWoodMass(inputDataList = VstDat),
                        type = "list")
})


### Test: Function generates expected output class
testthat::test_that(desc = "Output class vst_agb_per_ha", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_agb_per_ha,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class vst_plot_w_0s", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_plot_w_0s,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class vst_agb_zeros", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_agb_zeros,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class vst_site", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_site,
                            class = "data.frame")
})  



### Test: Function generates data frame with expected dimensions using test data
#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_agb_per_ha),
                             expected = as.integer(12))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_plot_w_0s),
                             expected = as.integer(13))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_agb_zeros),
                             expected = as.integer(6))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_site),
                             expected = as.integer(5))
})




#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_agb_per_ha),
                             expected = as.integer(65))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_plot_w_0s),
                             expected = as.integer(22))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_agb_zeros),
                             expected = as.integer(0))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_site),
                             expected = as.integer(2))
})



### Tests: Generate expected errors for 'inputDataList' ####
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat$vst_apparentindividual), # test whether function stops if supplied with a dataframe instead of list
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct()")
})

#   Test 'inputDataList' contains required tables (expect at least 4: appInd, mapandtag, nonwoody, and perplot)
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat[1:3]),
                         regexp = "Required tables missing from 'inputDataList'")
})

#################################################

### Test: Generate expected errors for issues with vst_apparentindividual table
# Test when vst_apparentindividual lacks required column
VstDat_mod <- VstDat
VstDat_mod$vst_apparentindividual <- VstDat_mod$vst_apparentindividual %>% dplyr::select(-stemDiameter)
#VstDat_mod <- remove_column(VstDat, "vst_apparentindividual", "stemDiameter")
testthat::test_that(desc = "Table 'vst_apparentindividual' missing column", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat_mod),
                         regexp = "Required columns missing from 'vst_apparentindividual': stemDiameter")
})

#   Test when vst_apparentindividual has no data
VstDat_mod <- VstDat
VstDat_mod$vst_apparentindividual <- VstDat_mod$vst_apparentindividual %>% dplyr::filter(uid == "notRealUid")
#VstDat_mod <- filter_df_in_list(VstDat, "vst_apparentindividual")
testthat::test_that(desc = "Table 'vst_apparentindividual' missing data", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat_mod),
                         regexp = "Table 'vst_apparentindividual' has no data.")
})



### Test: Generate expected errors for issues with vst_mappingandtagging table
# Test when vst_mappingandtagging lacks required column
VstDat_mod <- VstDat
VstDat_mod$vst_mappingandtagging <- VstDat_mod$vst_mappingandtagging %>% dplyr::select(-taxonID)
#VstDat_mod <- remove_column(VstDat, "vst_mappingandtagging", "taxonID")
testthat::test_that(desc = "Table 'vst_mappingandtagging' missing column", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat_mod),
                         regexp = "Required columns missing from 'vst_mappingandtagging': taxonID")
})

#   Test when vst_mappingandtagging has no data
VstDat_mod <- VstDat
VstDat_mod$vst_mappingandtagging <- VstDat_mod$vst_mappingandtagging %>% dplyr::filter(uid == "notRealUid")
#VstDat_mod <- filter_df_in_list(VstDat, "vst_mappingandtagging")
testthat::test_that(desc = "Table 'vst_mappingandtagging' missing data", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat_mod),
                         regexp = "Table 'vst_mappingandtagging' has no data.")
})



### Test: Generate expected errors for issues with vst_perplotperyear table
# Test when vst_perplotperyear lacks required column
VstDat_mod <- VstDat
VstDat_mod$vst_perplotperyear <- VstDat_mod$vst_perplotperyear %>% dplyr::select(-totalSampledAreaTrees)
#VstDat_mod <- remove_column(VstDat, "vst_perplotperyear", "totalSampledAreaTrees")
testthat::test_that(desc = "Table 'vst_perplotperyear' missing column", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat_mod),
                         regexp = "Required columns missing from 'vst_perplotperyear': totalSampledAreaTrees")
})

#   Test when vst_perplotperyear has no data
VstDat_mod <- VstDat
VstDat_mod$vst_perplotperyear <- VstDat_mod$vst_perplotperyear %>% dplyr::filter(uid == "notRealUid")
#VstDat_mod <- filter_df_in_list(VstDat, "vst_perplotperyear")
testthat::test_that(desc = "Table 'vst_perplotperyear' missing data", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat_mod),
                         regexp = "Table 'vst_perplotperyear' has no data.")
})


### Test: Generate error if output vst_agb_per_ha value not as expected
testthat::test_that(desc = "Output vst_agb_per_ha value as expected", {
  test <- estimateWoodMass(inputDataList = VstDat)
  testthat::expect_equal(object = test$vst_agb_per_ha$agb_Mgha[2],
                         expected = 15.931413)
})


### Test: Generate error if output vst_plot_w_0s value not as expected
testthat::test_that(desc = "Output vst_plot_w_0s value as expected", {
  test <- estimateWoodMass(inputDataList = VstDat)
  testthat::expect_equal(object = test$vst_plot_w_0s$agb_Mgha__Live[2],
                         expected = 10.3589)
})


### Test: Generate error if output vst_site value not as expected
testthat::test_that(desc = "Output vst_site value as expected", {
  test <- estimateWoodMass(inputDataList = VstDat)
  testthat::expect_equal(object = test$vst_site$woodLiveMassMean_Mgha[2],
                         expected = 52.286)
})
