# scaleHerbMass function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org

### Read in test data

HbpDat <- readRDS(testthat::test_path("testdata", "HbpDat.rds"))
HbpDat <- HbpDat

scaleHerbMassOutputs <- scaleHerbMass(inputDataListHbp = HbpDat)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = scaleHerbMass(inputDataListHbp = HbpDat),
                        type = "list")
})


### Test: Function generates expected output class

testthat::test_that(desc = "Output class hbp_agb", {
  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_agb,
                            class = "data.frame")
})  

testthat::test_that(desc = "Output class hbp_plot", {
  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_plot,
                            class = "data.frame")
})  



### Test: Function generates data frame with expected dimensions using test data
#   Check expected column number of data frame

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(25))
})

testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot),
                             expected = as.integer(13))
})



#   Check expected row number of data frame

testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(72))
})



### Tests: Generate expected errors for 'inputDataListHbp' ####
#   Test 'inputDataListHbp' is a list
testthat::test_that(desc = "Argument 'inputDataListHbp' is list object", {
  testthat::expect_error(object = scaleHerbMass(inputDataListHbp = HbpDat$hbp_perbout), # test whether function stops if supplied with a dataframe instead of list
                         regexp = "The inputDataListHbp argument is expected to be either a list or NA")
})

#   Test 'inputDataListHbp' contains required tables (expect at least 2: perbout, and massdata)
testthat::test_that(desc = "Required tables present in 'inputDataListHbp' input", {
  testthat::expect_error(object = scaleHerbMass(inputDataListHbp = HbpDat[1:1]),
                         regexp = "Required tables missing from 'inputDataListHbp'")
})

#################################################

### Test: Generate expected errors for issues with hbp_perbout table
# Test when hbp_perbout lacks required column
HbpDat_mod <- HbpDat
HbpDat_mod$hbp_perbout <- HbpDat_mod$hbp_perbout %>% dplyr::select(-clipArea)
testthat::test_that(desc = "Table 'hbp_perbout' missing column", {
  testthat::expect_error(object = scaleHerbMass(inputDataListHbp = HbpDat_mod),
                         regexp = "Required columns missing from 'hbp_perbout': clipArea")
})

#   Test when hbp_perbout has no data
HbpDat_mod <- HbpDat
HbpDat_mod$hbp_perbout <- HbpDat_mod$hbp_perbout %>% dplyr::filter(uid == "notRealUid")
testthat::test_that(desc = "Table 'hbp_perbout' missing data", {
  testthat::expect_error(object = scaleHerbMass(inputDataListHbp = HbpDat_mod),
                         regexp = "Table 'hbp_perbout' has no data.")
})



### Test: Generate expected errors for issues with hbp_massdata table
# Test when hbp_massdata lacks required column
HbpDat_mod <- HbpDat
HbpDat_mod$hbp_massdata <- HbpDat_mod$hbp_massdata %>% dplyr::select(-dryMass)
testthat::test_that(desc = "Table 'hbp_massdata' missing column", {
  testthat::expect_error(object = scaleHerbMass(inputDataListHbp = HbpDat_mod),
                         regexp = "Required columns missing from 'hbp_massdata': dryMass")
})

#   Test when hbp_massdata has no data
HbpDat_mod <- HbpDat
HbpDat_mod$hbp_massdata <- HbpDat_mod$hbp_massdata %>% dplyr::filter(uid == "notRealUid")
testthat::test_that(desc = "Table 'hbp_massdata' missing data", {
  testthat::expect_error(object = scaleHerbMass(inputDataListHbp = HbpDat_mod),
                         regexp = "Table 'hbp_massdata' has no data.")
})



### Test: Generate error if output hbp_agb value not as expected
testthat::test_that(desc = "Output hbp_agb value as expected", {
  test <- scaleHerbMass(inputDataListHbp = HbpDat)
  testthat::expect_equal(object = test$hbp_agb$dryMass_gm2_AllHerbaceousPlants[2], 
                         expected = 47.70)
})


### Test: Generate error if output hbp_plot value not as expected
testthat::test_that(desc = "Output hbp_plot value as expected", {
  test <- scaleHerbMass(inputDataListHbp = HbpDat)
  testthat::expect_equal(object = test$hbp_plot$herbPeakMassTotal_Mgha[2],
                         expected = 0.218)
})

