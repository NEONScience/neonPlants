### Function tests for scaleHerbMass
#   Courtney Meier; cmeier@BattelleEcology.org
#   Samuel M Simkin; simkin@BattelleEcology.org


#---> Add test to verify that number of rows in site-level output table equals number of sites in input dataset



### Read in test data
HbpDat <- readRDS(testthat::test_path("testdata", "hbp_testDat.RDS"))


scaleHerbMassOutputs <- neonPlants::scaleHerbMass(inputDataList = HbpDat)



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


testthat::test_that(desc = "Output class hbp_plot_extra", {

  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_plot_extra,
                            class = "data.frame")
})


testthat::test_that(desc = "Output class hbp_plot_crop", {

  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_plot_crop,
                            class = "data.frame")
})


testthat::test_that(desc = "Output class hbp_site", {

  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_site,
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
#   Check expected column number of per sampling cell data frame output
testthat::test_that(desc = "Output data frame column number 'hbp_agb'", {

  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(32))
})

#   Check expected column number of per plot output
testthat::test_that(desc = "Output data frame column number 'hbp_plot'", {

  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot),
                             expected = as.integer(17))
})

#   Check expected column number of per plot output
testthat::test_that(desc = "Output data frame column number 'hbp_plot_crop'", {

  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot_crop),
                             expected = as.integer(27))
})

#   Check expected column number of per plot output
testthat::test_that(desc = "Output data frame column number 'hbp_plot_extra'", {

  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot_extra),
                             expected = as.integer(17))
})

#   Check expected column number of per site output
testthat::test_that(desc = "Output data frame column number 'hbp_site'", {

  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_site),
                             expected = as.integer(14))
})

#   Check expected row number of per sampling cell data frame output
testthat::test_that(desc = "Output data frame row number 'hbp_agb'", {

  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(1481))
})

#   Check expected row number of per plot output
testthat::test_that(desc = "Output data frame row number 'hbp_plot", {

  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_plot),
                             expected = as.integer(258))
})

#   Check expected row number of per plot output
testthat::test_that(desc = "Output data frame row number 'hbp_plot_crop", {

  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_plot_crop),
                             expected = as.integer(55))
})

#   Check expected row number of per plot output
testthat::test_that(desc = "Output data frame row number 'hbp_plot_extra", {

  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_plot_extra),
                             expected = as.integer(29))
})

#   Check expected row number of per site output
testthat::test_that(desc = "Output data frame row number 'hbp_site", {

  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_site),
                             expected = as.integer(12))
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

  testthat::expect_equal(object = test$hbp_agb$TotalMass_gm2[12],
                         expected = 257.7)
})



### Test: Generate error if output hbp_plot value not as expected
testthat::test_that(desc = "Output hbp_plot value as expected", {

  test <- scaleHerbMass(inputDataList = HbpDat)

  testthat::expect_equal(object = test$hbp_plot$herbTotalMass_Mgha[7],
                         expected = 3.48)
})



### Test: Generate error if output hbp_plot value not as expected
testthat::test_that(desc = "Output hbp_plot_crop value as expected", {

  test <- scaleHerbMass(inputDataList = HbpDat)

  testthat::expect_equal(object = test$hbp_plot_crop$herbTotalMass_Mgha[21],
                         expected = 0)
})



### Test: Generate error if output hbp_plot value not as expected
testthat::test_that(desc = "Output hbp_plot_extra value as expected", {

  test <- scaleHerbMass(inputDataList = HbpDat)

  testthat::expect_equal(object = test$hbp_plot_extra$herbTotalMass_Mgha[29],
                         expected = 1.04)
})



### Test: Generate error if output hbp_site value not as expected
testthat::test_that(desc = "Output hbp_site value as expected", {

  test <- scaleHerbMass(inputDataList = HbpDat)

  testthat::expect_equal(object = test$hbp_site$herbTotalMean_Mgha[1],
                         expected = 0.25)
})



### Test: Verify sites with data in input data frame exist in output 'hbp_agb'
testthat::test_that(desc = "Output hbp_agb sites as expected", {

  inputSites <- sort(unique(HbpDat$hbp_perbout$siteID))
  outputSites <- sort(unique(scaleHerbMassOutputs$hbp_agb$siteID))

  testthat::expect_identical(object = outputSites,
                             expected = inputSites)

})



### Test: Verify sites with data in input data frame exist across plot-level outputs
testthat::test_that(desc = "Output sites in plot-level tables as expected", {

  inputSites <- sort(unique(HbpDat$hbp_perbout$siteID))
  plotOutputSites <- unique(scaleHerbMassOutputs$hbp_plot$siteID)
  cropOutputSites <- unique(scaleHerbMassOutputs$hbp_plot_crop$siteID)
  extraOutputSites <- unique(scaleHerbMassOutputs$hbp_plot_extra$siteID)
  outputSites <- sort(unique(c(plotOutputSites, cropOutputSites, extraOutputSites)))

  testthat::expect_identical(object = outputSites,
                             expected = inputSites)

})
