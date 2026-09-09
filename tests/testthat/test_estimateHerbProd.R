### Function tests for estimateHerbProd
#   Courtney Meier; cmeier@BattelleEcology.org
#   Samuel M Simkin; samuel.simkin@gmail.com



### Read in test data ####
#--> Test dataset details documented in 'test_hbp_dataset_prep.R' in 'dev_scripts' folder.
hbpTestDF <- readRDS(testthat::test_path("testdata", "hbp_testDat.RDS"))



### Generate estimateHerbProd outputs ####
herbProdOutputs <- neonPlants::estimateHerbProd(inputDataList = hbpTestDF)



### Output type tests ####

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = herbProdOutputs,
                        type = "list")
})



### Tests: Function generates expected output class for all output list objects
#   Check all output tables are data frames
testthat::test_that(desc = "Output class list objects", {

  testthat::expect_s3_class(object = herbProdOutputs$herb_ANPP_site,
                            class = "data.frame")

  testthat::expect_s3_class(object = herbProdOutputs$herb_ANPP_plot,
                            class = "data.frame")

  testthat::expect_s3_class(object = herbProdOutputs$herb_ANPP_grazed_extra,
                            class = "data.frame")

  testthat::expect_s3_class(object = herbProdOutputs$herb_grazed_consumption,
                            class = "data.frame")
})






### Output dimension tests ####

### Test: Function generates data frames with expected dimensions using test data and function defaults (plotSubset = "all")
#   Check expected column and row numbers of 'herb_ANPP_site' data frame
testthat::test_that(desc = "Output 'herb_ANPP_site' column and row numbers", {

  testthat::expect_identical(object = ncol(herbProdOutputs$herb_ANPP_site),
                             expected = as.integer(11))

  testthat::expect_identical(object = nrow(herbProdOutputs$herb_ANPP_site),
                             expected = as.integer(16))
})

#   Check expected column and row numbers of 'herb_ANPP_plot' data frame
testthat::test_that(desc = "Output 'herb_ANPP_plot' column and row numbers", {

  testthat::expect_identical(object = ncol(herbProdOutputs$herb_ANPP_plot),
                             expected = as.integer(27))

  testthat::expect_identical(object = nrow(herbProdOutputs$herb_ANPP_plot),
                             expected = as.integer(228))
})

#   Check expected column row numbers of 'herb_ANPP_grazed_extra' data frame
testthat::test_that(desc = "Output 'herb_ANPP_grazed_extra' column and row numbers", {

  testthat::expect_identical(object = ncol(herbProdOutputs$herb_ANPP_grazed_extra),
                             expected = as.integer(24))

  testthat::expect_identical(object = nrow(herbProdOutputs$herb_ANPP_grazed_extra),
                             expected = as.integer(8))
})

#   Check expected column and row numbers of 'herb_grazed_consumption' data frame
testthat::test_that(desc = "Output 'herb_grazed_consumption' column and row numbers", {

  testthat::expect_identical(object = ncol(herbProdOutputs$herb_grazed_consumption),
                             expected = as.integer(15))

  testthat::expect_identical(object = nrow(herbProdOutputs$herb_grazed_consumption),
                             expected = as.integer(33))
})






### Error handling tests ####

### Tests: Generate expected errors for 'inputDataList' and 'plotSubset' arguments
#   Check that error is produced when inputDataList argument is not a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = neonPlants::estimateHerbProd(inputDataList = hbpTestDF$hbp_perbout),
                         regexp = "The inputDataList argument is expected to be a list")
})

#   Check that 'plotSubset' error is produced when expected
testthat::test_that(desc = "Argument 'plotSubset' is valid", {
  testthat::expect_error(object = neonPlants::estimateHerbProd(inputDataList = hbpTestDF,
                                                               plotSubset = "fish"),
                         regexp = "The only valid plotSubset options are 'all', 'tower', 'distributed'")
})






### Output value tests ####

##  Check output 'herb_ANPP_site' value is as expected
testthat::test_that(desc = "Output 'herb_ANPP_site' table value as expected", {
  testthat::expect_equal(object = herbProdOutputs$herb_ANPP_site$herbProd_Mghayr[1],
                         expected = 4.64)
})


##  Check output 'herb_ANPP_plot' value is as expected
testthat::test_that(desc = "Output 'herb_ANPP_plot' table value as expected", {
  testthat::expect_equal(object = herbProdOutputs$herb_ANPP_plot$herbProd_Mghayr[1],
                         expected = 5.02)
})


##  Check output 'herb_ANPP_grazed_extra' value is as expected
testthat::test_that(desc = "Output 'herb_ANPP_grazed_extra' table value as expected", {
  testthat::expect_equal(object = herbProdOutputs$herb_ANPP_grazed_extra$grazedProd_gm2yr[3],
                         expected = 136.47)
})


##  Check output 'herb_grazed_consumption' value is as expected
testthat::test_that(desc = "Output 'herb_grazed_consumption' table value as expected", {
  testthat::expect_equal(object = herbProdOutputs$herb_grazed_consumption$consumMean_gm2[10],
                         expected = 40.35)
})


##  Check that sites in input data exist across aggregate site- and plot-level output tables
testthat::test_that(desc = "Output sites in site- and plot-level tables as expected", {

  inputSites <- hbpTestDF$hbp_perbout %>%
    dplyr::filter(.data$domainID != "D09") %>%
    dplyr::distinct(.data$siteID)

  inputSites <- sort(inputSites$siteID)

  outputSites <- sort(unique(herbProdOutputs$herb_ANPP_site$siteID))

  testthat::expect_identical(object = outputSites,
                             expected = inputSites)

})


##  Check number of site-years in input data is equivalent to row number of 'hbp_ANPP_site' output
testthat::test_that(desc = "Rows in 'hbp_ANPP_site' equal to number of site-years in input data", {

  #   Identify number of site-years in hbp_perbout input table
  inputSiteYears <- hbpTestDF$hbp_perbout %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(string = .data$eventID,
                                                         pattern = "20[0-9]{2}")),
                  .before = "eventID") %>%

    dplyr::mutate(collectDate = as.Date(.data$collectDate),
                  year = dplyr::case_when(.data$siteID == "SJER" &
                                            .data$collectDate < as.Date(glue::glue("{.data$year}-07-15")) ~
                                            (.data$year - 1),
                                          TRUE ~ .data$year)) %>%

    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-")) %>%

    #   Temporarily remove D09 sites
    dplyr::filter(.data$domainID != "D09") %>%

    dplyr::distinct(.data$siteYear)

  #   Check identical objects
  testthat::expect_identical(object = nrow(herbProdOutputs$herb_ANPP_site),
                             expected = nrow(inputSiteYears))

})
