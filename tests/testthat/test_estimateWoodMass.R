### Tests for neonPlants::estimateWoodMass function ####
#   Samuel M Simkin; ssimkin@BattelleEcology.org
#   Courtney Meier; cmeier@BattelleEcology.org



### Read in test data ####
VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))

#   Generate function outputsDF with argument defaults
outputsDF <- estimateWoodMass(inputDataList = VstDat)



### Test: Function generates expected output type ####
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateWoodMass(inputDataList = VstDat),
                        type = "list")
})


### Test: Function generates expected output class ####
testthat::test_that(desc = "Output class vst_agb_kg", {
  testthat::expect_s3_class(object = outputsDF$vst_agb_kg,
                            class = "data.frame")
})

testthat::test_that(desc = "Output class vst_missing", {
  testthat::expect_s3_class(object = outputsDF$vst_missing,
                            class = "data.frame")
})

testthat::test_that(desc = "Output class vst_plot_Mgha", {
  testthat::expect_s3_class(object = outputsDF$vst_plot_Mgha,
                            class = "data.frame")
})

testthat::test_that(desc = "Output class vst_site_Mgha", {
  testthat::expect_s3_class(object = outputsDF$vst_site_Mgha,
                            class = "data.frame")
})



### Test: Function generates data frames with expected dimensions using test data #### --> begin again here
#   Check expected column number of data frames
testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_agb_kg),
                             expected = as.integer(17))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_plot_w_0s),
                             expected = as.integer(15))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_agb_zeros),
                             expected = as.integer(6))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_site),
                             expected = as.integer(7))
})

#   Check expected row number of data frames
testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_agb_kg),
                             expected = as.integer(1347))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_plot_w_0s),
                             expected = as.integer(129))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_agb_zeros),
                             expected = as.integer(0))
})

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_site),
                             expected = as.integer(10))
})



### Tests: Generate expected errors for 'inputDataList' ####
#--> add tests for incorrect input arguments

#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat$vst_apparentindividual), # test whether function stops if supplied with a dataframe instead of list
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct()")
})

#   Test 'inputDataList' contains required tables (expect at least 3: appInd, mapandtag, and perplot)
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  testthat::expect_error(object = estimateWoodMass(inputDataList = VstDat[1:2]),
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


### Test: Generate error if output vst_agb_kg value not as expected
testthat::test_that(desc = "Output vst_agb_kg value as expected", {
  test <- estimateWoodMass(inputDataList = VstDat)
  testthat::expect_equal(object = test$vst_agb_kg$agb_kg[889],
                         expected = 1274.513)
})


### Test: Generate error if output vst_plot_w_0s value not as expected
testthat::test_that(desc = "Output vst_plot_w_0s value as expected", {
  test <- estimateWoodMass(inputDataList = VstDat)
  testthat::expect_equal(object = test$vst_plot_w_0s$Live_Mgha[85],
                         expected = 10.3588)
})


### Test: Generate error if output vst_site value not as expected
testthat::test_that(desc = "Output vst_site value as expected", {
  test <- estimateWoodMass(inputDataList = VstDat)
  testthat::expect_equal(object = test$vst_site$woodLiveMassMean_Mgha[7],
                         expected = 51.90)
})
