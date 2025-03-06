# scaleHerbMass function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org



### Read in test data
HbpDat <- readRDS(testthat::test_path("testdata", "HbpDat.rds"))


scaleHerbMassOutputs <- scaleHerbMass(inputDataList = HbpDat)



### Tests: Expected output types and data frame dimensions ####

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  
  testthat::expect_type(object = scaleHerbMass(inputDataList = HbpDat),
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
#   Check expected column number of per sampling cell data frame output
testthat::test_that(desc = "Output data frame column number", {
  
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(25))
})

#   Check expected column number of per plot output
testthat::test_that(desc = "Output data frame column number", {
  
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot),
                             expected = as.integer(13))
})

#   Check expected row number of per sampling cell data frame output
testthat::test_that(desc = "Output data frame row number", {
  
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(72))
})



### Tests: Error handling ####

### Tests: Generate expected errors for 'inputDataList'
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  
  testthat::expect_error(object = scaleHerbMass(inputDataList = HbpDat$hbp_perbout),
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct()")
})

#   Test 'inputDataList' contains required tables (expect at least 2: perbout, and massdata)
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  testthat::expect_error(object = scaleHerbMass(inputDataList = HbpDat[1:1]),
                         regexp = "Required tables missing from 'inputDataList'")
})



### Test: Generate expected errors for issues with hbp_perbout table
# Test when hbp_perbout lacks required column
testthat::test_that(desc = "Table 'inputBout' missing column", {
  
  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_perbout <- HbpDat_mod$hbp_perbout %>% 
    dplyr::select(-clipArea)
  
  testthat::expect_error(object = scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Required columns missing from 'inputBout': clipArea")
})

#   Test when hbp_perbout has no data
testthat::test_that(desc = "Table 'inputBout' missing data", {
  
  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_perbout <- HbpDat_mod$hbp_perbout %>% 
    dplyr::filter(uid == "notRealUid")
  
  testthat::expect_error(object = scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Table 'inputBout' has no data.")
})



### Test: Generate expected errors for issues with hbp_massdata table
# Test when hbp_massdata lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {
  
  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_massdata <- HbpDat_mod$hbp_massdata %>% 
    dplyr::select(-dryMass)
  
  testthat::expect_error(object = scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when hbp_massdata has no data
testthat::test_that(desc = "Table 'hbp_massdata' missing data", {
  
  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_massdata <- HbpDat_mod$hbp_massdata %>% 
    dplyr::filter(uid == "notRealUid")
  
  testthat::expect_error(object = scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Table 'inputMass' has no data.")
})



### Test: Generate error if output hbp_agb value not as expected
testthat::test_that(desc = "Output hbp_agb value as expected", {
  
  test <- scaleHerbMass(inputDataList = HbpDat)
  
  testthat::expect_equal(object = test$hbp_agb$AllHerbaceousPlants_gm2[2], 
                         expected = 47.70)
})


### Test: Generate error if output hbp_plot value not as expected
testthat::test_that(desc = "Output hbp_plot value as expected", {
  
  test <- scaleHerbMass(inputDataList = HbpDat)
  
  testthat::expect_equal(object = test$hbp_plot$herbPeakMassTotal_Mgha[2],
                         expected = 0.218)
})

