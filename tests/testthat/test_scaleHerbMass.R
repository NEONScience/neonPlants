### Function tests for scaleHerbMass (updated 2026-06-01)
#   Courtney Meier; cmeier@BattelleEcology.org
#   Samuel M Simkin



### Read in test data ####
#--> Test dataset details documented in 'test_hbp_dataset_prep.R' in 'dev_scripts' folder.

HbpDat <- readRDS(testthat::test_path("testdata", "hbp_testDat.RDS"))



### Generate scaleHerbMass outputs ####
scaleHerbMassOutputs <- neonPlants::scaleHerbMass(inputDataList = HbpDat)





### Output type tests ####

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = scaleHerbMassOutputs,
                        type = "list")
})



### Tests: Function generates expected output class for all output list objects
#   Check 'hbp_agb' output table is a data frame
testthat::test_that(desc = "Output class hbp_agb", {
  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_agb,
                            class = "data.frame")
})

#   Check 'hbp_plot' output table is a data frame
testthat::test_that(desc = "Output class hbp_plot", {
  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_plot,
                            class = "data.frame")
})

#   Check 'hbp_plot_extra' output table is a data frame
testthat::test_that(desc = "Output class hbp_plot_extra", {
  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_plot_extra,
                            class = "data.frame")
})

#   Check 'hbp_plot_crop' output table is a data frame
testthat::test_that(desc = "Output class hbp_plot_crop", {
  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_plot_crop,
                            class = "data.frame")
})

#   Check 'hbp_site' output table is a data frame
testthat::test_that(desc = "Output class hbp_site", {
  testthat::expect_s3_class(object = scaleHerbMassOutputs$hbp_site,
                            class = "data.frame")
})





### Output dimension tests ####

### Test: Function generates data frame with expected dimensions using test data and function defaults (plotSubset = "all")
#   Check expected column number of 'hbp_agb' data frame
testthat::test_that(desc = "Output data frame column number 'hbp_agb'", {
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(32))
})

#   Check expected row number of 'hbp_agb' data frame
testthat::test_that(desc = "Output data frame row number 'hbp_agb'", {
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_agb),
                             expected = as.integer(1806))
})

#   Check expected column number of 'hbp_plot' data frame
testthat::test_that(desc = "Output data frame column number 'hbp_plot'", {
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot),
                             expected = as.integer(17))
})

#   Check expected row number of 'hbp_plot' data frame
testthat::test_that(desc = "Output data frame row number 'hbp_plot", {
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_plot),
                             expected = as.integer(372))
})

#   Check expected column number of 'hbp_plot_crop' data frame
testthat::test_that(desc = "Output data frame column number 'hbp_plot_crop'", {
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot_crop),
                             expected = as.integer(27))
})

#   Check expected row number of 'hbp_plot_crop' data frame
testthat::test_that(desc = "Output data frame row number 'hbp_plot_crop", {
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_plot_crop),
                             expected = as.integer(55))
})

#   Check expected column number 'hbp_plot_extra' data frame
testthat::test_that(desc = "Output data frame column number 'hbp_plot_extra'", {
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_plot_extra),
                             expected = as.integer(17))
})

#   Check expected row number of 'hbp_plot_extra' data frame
testthat::test_that(desc = "Output data frame row number 'hbp_plot_extra", {
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_plot_extra),
                             expected = as.integer(35))
})

#   Check expected column number of 'hbp_site' data frame
testthat::test_that(desc = "Output data frame column number 'hbp_site'", {
  testthat::expect_identical(object = ncol(scaleHerbMassOutputs$hbp_site),
                             expected = as.integer(14))
})

#   Check expected row number of 'hbp_site' data frame
testthat::test_that(desc = "Output data frame row number 'hbp_site", {
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_site),
                             expected = as.integer(16))
})





### Error handling tests ####

### Tests: Generate expected errors for 'inputDataList'
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = neonPlants::scaleHerbMass(inputDataList = HbpDat$hbp_perbout),
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct()")
})

#   Test 'inputDataList' contains required tables (expect at least 2: perbout, and massdata)
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  testthat::expect_error(object = neonPlants::scaleHerbMass(inputDataList = HbpDat[1:1]),
                         regexp = "Required tables missing from 'inputDataList'")
})



### Test: Generate expected errors for issues with hbp_perbout table
# Test when hbp_perbout lacks required column
testthat::test_that(desc = "Table 'inputBout' missing column", {

  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_perbout <- HbpDat_mod$hbp_perbout %>%
    dplyr::select(-"clipArea")

  testthat::expect_error(object = neonPlants::scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Required columns missing from 'inputBout': clipArea")
})

#   Test when hbp_perbout has no data
testthat::test_that(desc = "Table 'inputBout' missing data", {

  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_perbout <- HbpDat_mod$hbp_perbout %>%
    dplyr::filter(uid == "notRealUid")

  testthat::expect_error(object = neonPlants::scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Table 'inputBout' has no data.")
})



### Test: Generate expected errors for issues with hbp_massdata table
# Test when hbp_massdata lacks required column
testthat::test_that(desc = "Table 'inputMass' missing column", {

  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_massdata <- HbpDat_mod$hbp_massdata %>%
    dplyr::select(-"dryMass")

  testthat::expect_error(object = neonPlants::scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Required columns missing from 'inputMass': dryMass")
})

#   Test when hbp_massdata has no data
testthat::test_that(desc = "Table 'hbp_massdata' missing data", {

  HbpDat_mod <- HbpDat
  HbpDat_mod$hbp_massdata <- HbpDat_mod$hbp_massdata %>%
    dplyr::filter(uid == "notRealUid")

  testthat::expect_error(object = neonPlants::scaleHerbMass(inputDataList = HbpDat_mod),
                         regexp = "Table 'inputMass' has no data.")
})





### Output value tests ####

##  Test: Generate error if output 'hbp_agb' value not as expected
testthat::test_that(desc = "Output 'hbp_agb' value as expected", {
  testthat::expect_equal(object = scaleHerbMassOutputs$hbp_agb$TotalMass_gm2[12],
                         expected = 257.7)
})


##  Test: Generate error if output hbp_plot value not as expected
testthat::test_that(desc = "Output 'hbp_plot' value as expected", {
  testthat::expect_equal(object = scaleHerbMassOutputs$hbp_plot$herbTotalMass_Mgha[7],
                         expected = 0.26)
})


##  Test: Generate error if output hbp_plot value not as expected
testthat::test_that(desc = "Output 'hbp_plot_crop' value as expected", {
  testthat::expect_equal(object = scaleHerbMassOutputs$hbp_plot_crop$herbTotalMass_Mgha[21],
                         expected = 0)
})


##  Test: Generate error if output hbp_plot_extra value not as expected
testthat::test_that(desc = "Output 'hbp_plot_extra' value as expected", {
  testthat::expect_equal(object = scaleHerbMassOutputs$hbp_plot_extra$herbTotalMass_Mgha[29],
                         expected = 1.04)
})


##  Test: Generate error if output hbp_site value not as expected
testthat::test_that(desc = "Output 'hbp_site' value as expected", {
  testthat::expect_equal(object = scaleHerbMassOutputs$hbp_site$herbTotalMean_Mgha[1],
                         expected = 0.25)
})


##  Test: Verify sites in input data exist in 'hbp_agb' output
testthat::test_that(desc = "Output 'hbp_agb' sites as expected", {

  inputSites <- sort(unique(HbpDat$hbp_perbout$siteID))
  outputSites <- sort(unique(scaleHerbMassOutputs$hbp_agb$siteID))

  testthat::expect_identical(object = outputSites,
                             expected = inputSites)

})


##  Test: Verify sites in input data exist across plot-level outputs
testthat::test_that(desc = "Output sites in plot-level tables as expected", {

  inputSites <- sort(unique(HbpDat$hbp_perbout$siteID))
  plotOutputSites <- unique(scaleHerbMassOutputs$hbp_plot$siteID)
  cropOutputSites <- unique(scaleHerbMassOutputs$hbp_plot_crop$siteID)
  extraOutputSites <- unique(scaleHerbMassOutputs$hbp_plot_extra$siteID)
  outputSites <- sort(unique(c(plotOutputSites, cropOutputSites, extraOutputSites)))

  testthat::expect_identical(object = outputSites,
                             expected = inputSites)

})


##  Test: Verify number of site-years in input data is equivalent to row number of 'hbp_site' output
testthat::test_that(desc = "Rows in 'hbp_site' equal to number of site-years in input data", {

  #   Identify number of site-years in hbp_perbout input table; remove BLAN since Ag sites not included in 'hbp_site' peak biomass output
  inputSiteYears <- HbpDat$hbp_perbout %>%
    dplyr::filter(.data$siteID != "BLAN") %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(string = .data$eventID,
                                                         pattern = "20[0-9]{2}")),
                  .before = "eventID") %>%

    dplyr::mutate(collectDate = as.Date(.data$collectDate),
                  year = dplyr::case_when(.data$siteID == "SJER" &
                                            .data$collectDate < as.Date(glue::glue("{.data$year}-07-15")) ~
                                            (.data$year - 1),
                                          TRUE ~ .data$year)) %>%

    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-")) %>%
    dplyr::distinct(.data$siteYear)

  #   Check identical objects
  testthat::expect_identical(object = nrow(scaleHerbMassOutputs$hbp_site),
                             expected = nrow(inputSiteYears))

})
