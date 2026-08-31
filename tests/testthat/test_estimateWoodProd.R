### Function tests for estimateWoodProd ####
#   Courtney Meier; cmeier@BattelleEcology.org



### Read in test data ####
vstTestDF <- readRDS(testthat::test_path("testdata", "vst_testDat.rds"))

#   Reduce to one site to meet function requirements
theSite <- "ABBY"

map <- vstTestDF$vst_mappingandtagging %>%
  dplyr::filter(siteID == theSite)

appInd <- vstTestDF$vst_apparentindividual %>%
  dplyr::filter(siteID == theSite)

perPlot <- vstTestDF$vst_perplotperyear %>%
  dplyr::filter(siteID == theSite)

nonWoody <- vstTestDF$`vst_non-woody` %>%
  dplyr::filter(siteID == theSite)

#   Create test input list
testInput <- list(vst_mappingandtagging = map,
                  vst_apparentindividual = appInd,
                  vst_perplotperyear = perPlot,
                  `vst_non-woody` = nonWoody)



### Generate estimateWoodProd outputs with argument defaults ####

woodProdOutputs <- neonPlants::estimateWoodProd(inputDataList = testInput,
                                                plotSubset = "all",
                                                flagged = "retain",
                                                missing = "filter")






### Output type tests ####

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = woodProdOutputs,
                        type = "list")
})



### Tests: Function generates expected output class for all output list objects
#   Check 'vst_ANPP_indiv' output table is a data frame
testthat::test_that(desc = "Output class vst_ANPP_indiv", {
  testthat::expect_s3_class(object = woodProdOutputs$vst_ANPP_indiv,
                            class = "data.frame")
})

#   Check 'vst_ANPP_plot' output table is a data frame
testthat::test_that(desc = "Output class vst_ANPP_plot", {
  testthat::expect_s3_class(object = woodProdOutputs$vst_ANPP_plot,
                            class = "data.frame")
})

#   Check 'vst_ANPP_site' output table is a data frame
testthat::test_that(desc = "Output class vst_ANPP_site", {
  testthat::expect_s3_class(object = woodProdOutputs$vst_ANPP_site,
                            class = "data.frame")
})

#   Check 'duplicates' output table is a data frame
testthat::test_that(desc = "Output class duplicates", {
  testthat::expect_s3_class(object = woodProdOutputs$duplicates,
                            class = "data.frame")
})

#   Check 'flagged' output table is a data frame
testthat::test_that(desc = "Output class flagged", {
  testthat::expect_s3_class(object = woodProdOutputs$flagged,
                            class = "data.frame")
})

#   Check 'missing' output table is a data frame
testthat::test_that(desc = "Output class missing", {
  testthat::expect_s3_class(object = woodProdOutputs$missing,
                            class = "data.frame")
})






### Output dimension tests ####

### Test: Function generates data frames with expected dimensions using test data and function defaults
#--> Defaults: plotSubset = "all", flagged = "retain", missing = "filter"

#   Check expected column number of 'vst_ANPP_indiv' data frame
testthat::test_that(desc = "Output data frame column number 'vst_ANPP_indiv'", {
  testthat::expect_identical(object = ncol(woodProdOutputs$vst_ANPP_indiv),
                             expected = as.integer(36))
})

#   Check expected row number of 'vst_ANPP_indiv' data frame
testthat::test_that(desc = "Output data frame row number 'vst_ANPP_indiv'", {
  testthat::expect_identical(object = nrow(woodProdOutputs$vst_ANPP_indiv),
                             expected = as.integer(5400))
})

#   Check expected column number of 'vst_ANPP_plot' data frame
testthat::test_that(desc = "Output data frame column number 'vst_ANPP_plot'", {
  testthat::expect_identical(object = ncol(woodProdOutputs$vst_ANPP_plot),
                             expected = as.integer(11))
})

#   Check expected row number of 'vst_ANPP_plot' data frame
testthat::test_that(desc = "Output data frame row number 'vst_ANPP_plot'", {
  testthat::expect_identical(object = nrow(woodProdOutputs$vst_ANPP_plot),
                             expected = as.integer(142))
})

#   Check expected column number of 'vst_ANPP_site' data frame
testthat::test_that(desc = "Output data frame column number 'vst_ANPP_site'", {
  testthat::expect_identical(object = ncol(woodProdOutputs$vst_ANPP_site),
                             expected = as.integer(9))
})

#   Check expected row number of 'vst_ANPP_site' data frame
testthat::test_that(desc = "Output data frame row number 'vst_ANPP_site'", {
  testthat::expect_identical(object = nrow(woodProdOutputs$vst_ANPP_site),
                             expected = as.integer(9))
})

#   Check expected column number of 'duplicates' data frame
testthat::test_that(desc = "Output data frame column number 'duplicates'", {
  testthat::expect_identical(object = ncol(woodProdOutputs$duplicates),
                             expected = as.integer(28))
})

#   Check expected row number of 'duplicates' data frame
testthat::test_that(desc = "Output data frame row number 'duplicates'", {
  testthat::expect_identical(object = nrow(woodProdOutputs$duplicates),
                             expected = as.integer(63))
})

#   Check expected column number of 'flagged' data frame
testthat::test_that(desc = "Output data frame column number 'flagged'", {
  testthat::expect_identical(object = ncol(woodProdOutputs$flagged),
                             expected = as.integer(32))
})

#   Check expected row number of 'flagged' data frame
testthat::test_that(desc = "Output data frame row number 'flagged'", {
  testthat::expect_identical(object = nrow(woodProdOutputs$flagged),
                             expected = as.integer(245))
})

#   Check expected column number of 'missing' data frame
testthat::test_that(desc = "Output data frame column number 'missing'", {
  testthat::expect_identical(object = ncol(woodProdOutputs$missing),
                             expected = as.integer(31))
})

#   Check expected row number of 'missing' data frame
testthat::test_that(desc = "Output data frame row number 'missing'", {
  testthat::expect_identical(object = nrow(woodProdOutputs$missing),
                             expected = as.integer(31))
})






### Error handling tests ####

### Tests: Generate expected errors for 'inputDataList'
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = neonPlants::estimateWoodProd(inputDataList = testInput$vst_apparentindividual),
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct()")
})

#   Test 'inputDataList' contains all required tables
testthat::test_that(desc = "The'inputDataList' object contains required tables", {
  temp <- testInput
  temp$vst_mappingandtagging <- NULL

  testthat::expect_error(object = neonPlants::estimateWoodProd(inputDataList = temp),
                         regexp = "Required tables missing from 'inputDataList'")
})

#   Test 'inputDataList' contains only one siteID
testthat::test_that(desc = "Input data contains a single siteID", {
  testthat::expect_error(object = neonPlants::estimateWoodProd(inputDataList = vstTestDF),
                         regexp = "Woody productivity may only be estimated for one siteID at a time")
})



### Test: Generate expected errors for issues with input arguments
#   Test for unexpected 'plotSubset' argument
testthat::test_that(desc = "Unexpected 'plotSubset' argument", {
  testthat::expect_error(object = neonPlants::estimateWoodProd(inputDataList = testInput,
                                                               plotSubset = "fromage"),
                         regexp = "The 'plotSubset' argument must be one of: 'all', 'towerAll', 'towerAnnualSubset', 'distributed'")
})

#   Test for unexpected 'flagged' argument
testthat::test_that(desc = "Unexpected 'flagged' argument", {
  testthat::expect_error(object = neonPlants::estimateWoodProd(inputDataList = testInput,
                                                               flagged = "apostate"),
                         regexp = "The 'flagged' argument must be one of: 'filter', 'retain'")
})

#   Test for unexpected 'missing' argument
testthat::test_that(desc = "Unexpected 'missing' argument", {
  testthat::expect_error(object = neonPlants::estimateWoodProd(inputDataList = testInput,
                                                               missing = "armadillo"),
                         regexp = "The 'missing' argument must be one of: 'filter', 'retain'")
})






### Output value tests ####

### Tests: Generate expected values in output data frames

#   Test: Check for expected 'vst_ANPP_indiv' value
testthat::test_that(desc = "Output 'vst_ANPP_indiv' value as expected", {
  testthat::expect_equal(object = woodProdOutputs$vst_ANPP_indiv$agb_kg[5],
                         expected = 29.41)
})

#   Test: Check for expected 'vst_ANPP_plot' value
testthat::test_that(desc = "Output 'vst_ANPP_plot' value as expected", {
  testthat::expect_equal(object = woodProdOutputs$vst_ANPP_plot$woodProd_Mghayr[2],
                         expected = 11.79)
})

#   Test: Check for expected 'vst_ANPP_site' value
testthat::test_that(desc = "Output 'vst_ANPP_site' value as expected", {
  testthat::expect_equal(object = woodProdOutputs$vst_ANPP_site$woodProd_Mghayr[2],
                         expected = 2.05)
})

#   Test: Check for expected 'duplicates' value
testthat::test_that(desc = "Output 'duplicates' value as expected", {
  testthat::expect_equal(object = woodProdOutputs$duplicates$individualID[1],
                         expected = "NEON.PLA.D16.ABBY.00021")
})

#   Test: Check for expected 'flagged' value
testthat::test_that(desc = "Output 'flagged' value as expected", {
  testthat::expect_equal(object = woodProdOutputs$flagged$agb_kg[2],
                         expected = 241.54)
})

#   Test: Check for expected 'missing' value
testthat::test_that(desc = "Output 'missing' value as expected", {
  testthat::expect_equal(object = woodProdOutputs$missing$individualID[1],
                         expected = "NEON.PLA.D16.ABBY.00034")
})



### Test: Verify eventIDs in input data exist in 'vst_ANPP_site' output
testthat::test_that(desc = "Output 'vst_ANPP_site' contains eventIDs as expected", {

  #   Prep input eventID list
  inputEvents <- sort(unique(perPlot$eventID))

  #   Prep output eventID list
  outputEvents <- sort(unique(woodProdOutputs$vst_ANPP_site$eventID))

  #   Check identical
  testthat::expect_identical(object = outputEvents,
                             expected = inputEvents)
})



### Test: Verify unique plot-events in input data exist in 'vst_ANPP_plot' output
testthat::test_that(desc = "Output 'plot-events' match input 'plot-events'", {

  ##  Derive expected 'plot-events' from input data set
  inputPlotEvent <- perPlot %>%
    dplyr::filter(.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical)) %>%
    dplyr::mutate(plotEvent = paste(.data$plotID, .data$eventID, sep = "-")) %>%
    dplyr::distinct(.data$plotEvent) %>%
    dplyr::arrange(.data$plotEvent)

  inputPlotEvent <- inputPlotEvent$plotEvent


  ##  Derive expected 'plot-events' from output data set
  outputPlotEvent <- vst_ANPP_plot %>%
    dplyr::mutate(plotEvent = paste(.data$plotID, .data$eventID, sep = "-")) %>%
    dplyr::distinct(.data$plotEvent) %>%
    dplyr::arrange(.data$plotEvent)

  outputPlotEvent <- outputPlotEvent$plotEvent


  ##  Conduct identical plot-event test
  testthat::expect_identical(object = outputPlotEvent,
                             expected = inputPlotEvent)
})


