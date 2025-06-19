# estimateHerbProd function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org

### Read in test data ####
HbpDat <- readRDS(testthat::test_path("testdata", "HbpDat.rds"))


scaleHerbMassOutputs <- scaleHerbMass(inputDataList = HbpDat)
estimateHerbProdOutputs <- estimateHerbProd(inputDataList = scaleHerbMassOutputs)



### Output type tests ####
### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateHerbProd(inputDataList = scaleHerbMassOutputs),
                        type = "list")
})


### Test: Function generates expected output class

testthat::test_that(desc = "Output class", {
  testthat::expect_s3_class(object = estimateHerbProdOutputs$herb_ANPP_site,
                            class = "data.frame")
})



### Output dimension tests ####
### Test: Function generates data frame with expected dimensions using test data
#   Check expected column number of data frame

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateHerbProdOutputs$herb_ANPP_site),
                             expected = as.integer(8))
})

#   Check expected row number of data frame

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateHerbProdOutputs$herb_ANPP_site),
                             expected = as.integer(4))
})



### Expected error tests: Generate expected errors for inputDataList object ####
#   Test that inputDataList argument is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = estimateHerbProd(inputDataList = scaleHerbMassOutputs$hbp_agb), # test whether function stops if supplied with a dataframe instead of list
                         regexp = "The inputDataList argument is expected to be a list")
})

#   Test that inputDataList object contains required tables (expect 3 tables: "hbp_agb", "hbp_plot", "herb_ANPP_site")
testthat::test_that(desc = "Required tables missing from 'inputDataList' list", {
  testthat::expect_error(object = estimateHerbProd(inputDataList = scaleHerbMassOutputs[1:2]),
                         regexp = "Required tables missing from 'inputDataList' list")
})



### Test: Generate expected errors for issues with hbp_agb table
# Test when input hbp_agb lacks required column
scaleHerbMassOutputs_mod <- scaleHerbMassOutputs
scaleHerbMassOutputs_mod$hbp_agb <- scaleHerbMassOutputs_mod$hbp_agb %>% dplyr::select(-AllHerbaceousPlants_gm2)
testthat::test_that(desc = "Table 'hbp_agb' missing column", {
  testthat::expect_error(object = estimateHerbProd(inputDataList = scaleHerbMassOutputs_mod),
                         regexp = "Required columns missing from 'hbp_agb': AllHerbaceousPlants_gm2")
})

#   Test when hbp_agb has no data
scaleHerbMassOutputs_mod <- scaleHerbMassOutputs
scaleHerbMassOutputs_mod$hbp_agb <- scaleHerbMassOutputs_mod$hbp_agb %>% dplyr::filter(year == "notRealyear")
testthat::test_that(desc = "Table 'hbp_agb' missing data", {
  testthat::expect_error(object = estimateHerbProd(inputDataList = scaleHerbMassOutputs_mod),
                         regexp = "Table 'hbp_agb' has no data.")
})



### Test: Generate expected errors for issues with hbp_plot table
# Test when input hbp_plot lacks required column
scaleHerbMassOutputs_mod <- scaleHerbMassOutputs
scaleHerbMassOutputs_mod$hbp_plot <- scaleHerbMassOutputs_mod$hbp_plot %>% dplyr::select(-herbPeakMassTotal_Mgha)
testthat::test_that(desc = "Table 'hbp_plot' missing column", {
  testthat::expect_error(object = estimateHerbProd(inputDataList = scaleHerbMassOutputs_mod),
                         regexp = "Required columns missing from 'hbp_plot': herbPeakMassTotal_Mgha")
})



### Test: Generate error if output herb_ANPP_site value not as expected
testthat::test_that(desc = "Output herb_ANPP_site value as expected", {
  test <- estimateHerbProd(inputDataList = scaleHerbMassOutputs)
  testthat::expect_equal(object = test$herb_ANPP_site$herbANPPMean_Mghayr[4],
                         expected = 0.7182)
})

