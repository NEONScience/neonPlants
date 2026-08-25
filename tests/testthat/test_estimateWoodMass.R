### Function tests for estimateWoodMass ####
#   Courtney Meier; cmeier@BattelleEcology.org



### Read in test data ####
vstTestDF <- readRDS(testthat::test_path("testdata", "vst_testDat.rds"))



### Generate estimateWoodMass outputs with argument defaults ####
estimateWoodMassOutputs <- neonPlants::estimateWoodMass(inputDataList = vstTestDF)





### Output type tests ####

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateWoodMassOutputs,
                        type = "list")
})



### Tests: Function generates expected output class for all output list objects
#   Check 'vst_agb_kg' output table is a data frame
testthat::test_that(desc = "Output class vst_agb_kg", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_agb_kg,
                            class = "data.frame")
})

#   Check 'vst_missing' output table is a data frame
testthat::test_that(desc = "Output class vst_missing", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_missing,
                            class = "data.frame")
})

#   Check 'vst_plot_Mgha' output table is a data frame
testthat::test_that(desc = "Output class vst_plot_Mgha", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_plot_Mgha,
                            class = "data.frame")
})

#   Check 'vst_site_Mgha' output table is a data frame
testthat::test_that(desc = "Output class vst_site_Mgha", {
  testthat::expect_s3_class(object = estimateWoodMassOutputs$vst_site_Mgha,
                            class = "data.frame")
})





### Output dimension tests ####

### Test: Function generates data frames with expected dimensions using test data and function defaults
#--> Defaults: plotSubset = "all", growthFormSubset = "all"

#   Check expected column number of 'vst_agb_kg' data frame
testthat::test_that(desc = "Output data frame column number 'vst_agb_kg'", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_agb_kg),
                             expected = as.integer(30))
})

#   Check expected row number of 'vst_agb_kg' data frame
testthat::test_that(desc = "Output data frame row number 'vst_agb_kg'", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_agb_kg),
                             expected = as.integer(22769))
})

#   Check expected column number of 'vst_missing' data frame
testthat::test_that(desc = "Output data frame column number 'vst_missing'", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_missing),
                             expected = as.integer(50))
})

#   Check expected row number of 'vst_missing' data frame
testthat::test_that(desc = "Output data frame row number 'vst_missing'", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_missing),
                             expected = as.integer(9751))
})

#   Check expected column number of 'vst_plot_Mgha' data frame
testthat::test_that(desc = "Output data frame column number 'vst_plot_Mgha'", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_plot_Mgha),
                             expected = as.integer(12))
})

#   Check expected row number of 'vst_plot_Mgha' data frame
testthat::test_that(desc = "Output data frame row number 'vst_plot_Mgha'", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_plot_Mgha),
                             expected = as.integer(287))
})

#   Check expected column number of 'vst_site_Mgha' data frame
testthat::test_that(desc = "Output data frame column number 'vst_site_Mgha'", {
  testthat::expect_identical(object = ncol(estimateWoodMassOutputs$vst_site_Mgha),
                             expected = as.integer(12))
})

#   Check expected row number of 'vst_site_Mgha' data frame
testthat::test_that(desc = "Output data frame row number 'vst_site_Mgha'", {
  testthat::expect_identical(object = nrow(estimateWoodMassOutputs$vst_site_Mgha),
                             expected = as.integer(19))
})





### Error handling tests ####

### Tests: Generate expected errors for 'inputDataList'
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = vstTestDF$vst_apparentindividual),
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct()")
})

#   Test 'inputDataList' contains required tables (expect at least 3: appInd, mapandtag, and perplot)
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = vstTestDF[1:2]),
                         regexp = "Required tables missing from 'inputDataList'")
})



### Test: Generate expected errors for issues with vst_apparentindividual table
#   Test when vst_apparentindividual lacks required column
testthat::test_that(desc = "Table 'vst_apparentindividual' missing column", {

  temp <- vstTestDF
  temp$vst_apparentindividual <- temp$vst_apparentindividual %>%
    dplyr::select(-"stemDiameter")

  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = temp),
                         regexp = "Required columns missing from 'vst_apparentindividual': stemDiameter")
})

#   Test when vst_apparentindividual has no data
testthat::test_that(desc = "Table 'vst_apparentindividual' missing data", {

  temp <- vstTestDF
  temp$vst_apparentindividual <- temp$vst_apparentindividual %>%
    dplyr::filter(siteID == "bupkis")

  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = temp),
                         regexp = "Table 'vst_apparentindividual' has no data.")
})



### Test: Generate expected errors for issues with vst_mappingandtagging table
# Test when vst_mappingandtagging lacks required column
testthat::test_that(desc = "Table 'vst_mappingandtagging' missing column", {

  temp <- vstTestDF
  temp$vst_mappingandtagging <- temp$vst_mappingandtagging %>%
    dplyr::select(-"taxonID")

  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = temp),
                         regexp = "Required columns missing from 'vst_mappingandtagging': taxonID")
})

#   Test when vst_mappingandtagging has no data
testthat::test_that(desc = "Table 'vst_mappingandtagging' missing data", {

  temp <- vstTestDF
  temp$vst_mappingandtagging <- temp$vst_mappingandtagging %>%
    dplyr::filter(siteID == "schmendrik")

  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = temp),
                         regexp = "Table 'vst_mappingandtagging' has no data.")
})



### Test: Generate expected errors for issues with vst_perplotperyear table
#   Test when vst_perplotperyear lacks required column
testthat::test_that(desc = "Table 'vst_perplotperyear' missing column", {

  temp <- vstTestDF
  temp$vst_perplotperyear <- temp$vst_perplotperyear %>%
    dplyr::select(-"totalSampledAreaTrees")

  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = temp),
                         regexp = "Required columns missing from 'vst_perplotperyear': totalSampledAreaTrees")
})

#   Test when vst_perplotperyear has no data
testthat::test_that(desc = "Table 'vst_perplotperyear' missing data", {

  temp <- vstTestDF
  temp$vst_perplotperyear <- temp$vst_perplotperyear %>%
    dplyr::filter(siteID == "shlemiel")

  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = temp),
                         regexp = "Table 'vst_perplotperyear' has no data.")
})



### Test: Generate expected errors for issues with input arguments
#   Test when unexpected value for 'plotSubset' is provided
testthat::test_that(desc = "Unexpected 'plotSubset' argument", {
  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = vstTestDF,
                                                               plotSubset = "poltroon"),
                         regexp = "The plotSubset argument must be one of: 'all', 'towerAll', 'towerAnnualSubset', 'distributed'")
})

#   Test when unexpected value for 'growthFormSubset' is provided
testthat::test_that(desc = "Unexpected 'growthFormSubset' argument", {
  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = vstTestDF,
                                                               growthFormSubset = "putz"),
                         regexp = "The growthFormSubset argument must be one of: 'all', 'tree'")
})






### Output value tests ####

### Test: Generate error if output 'vst_agb_kg' value not as expected
testthat::test_that(desc = "Output 'vst_agb_kg' value as expected", {
  testthat::expect_equal(object = estimateWoodMassOutputs$vst_agb_kg$agb_kg[1],
                         expected = 0.12)
})



### Test: Generate error if output 'vst_missing' value not as expected
testthat::test_that(desc = "Output 'vst_missing' value as expected", {
  testthat::expect_equal(object = estimateWoodMassOutputs$vst_missing$individualID[175],
                         expected = "NEON.PLA.D02.BLAN.08420")
})



### Test: Generate error if output 'vst_plot_Mgha' value not as expected
testthat::test_that(desc = "Output 'vst_plot_Mgha' value as expected", {
  testthat::expect_equal(object = estimateWoodMassOutputs$vst_plot_Mgha$agb_Mgha[13],
                         expected = 79.18)
})



### Test: Generate error if output 'vst_site_Mgha' value not as expected
testthat::test_that(desc = "Output 'vst_site_Mgha' value as expected", {
  testthat::expect_equal(object = estimateWoodMassOutputs$vst_site_Mgha$woodMassMean_Mgha[18],
                         expected = 244.9)
})



### Test: Verify site-years in input data exist in 'vst_site_Mgha' output
testthat::test_that(desc = "Output 'vst_site_Mgha' sites as expected", {

  #   Prep input site-year list
  inputSiteYear <- vstTestDF$vst_perplotperyear %>%
    dplyr::filter(samplingImpractical == "OK" | is.na(samplingImpractical),
                  !dataCollected %in% c("partial", "dendrometerOnly")) %>%
    dplyr::distinct(siteID,
                    eventID) %>%
    dplyr::mutate(site_year = paste(siteID,
                                     stringr::str_extract(eventID, "[0-9]{4}$"),
                                     sep = "-")) %>%
    dplyr::arrange(site_year)

  inputSiteYear <- inputSiteYear$site_year

  #   Prep output site-year list
  outputSiteYear <- estimateWoodMassOutputs$vst_site_Mgha %>%
    dplyr::mutate(site_year = paste(siteID, year, sep = "-")) %>%
    dplyr::arrange(site_year)

  outputSiteYear <- outputSiteYear$site_year

  #   Check identical
  testthat::expect_identical(object = outputSiteYear,
                             expected = inputSiteYear)
})



### Test: Verify unique plot-events in input data exist in 'vst_plot_Mgha' output
testthat::test_that(desc = "Output 'plot-events' match input 'plot-events'", {

  ##  Derive expected 'plot-events' from input data set
  #   Identify partially sampled plots: These should not be in 'vst_plot_Mgha' output table
  plotEventPartial <- vstTestDF$vst_perplotperyear %>%
    dplyr::filter(dataCollected %in% c("dendrometerOnly", "partial")) %>%
    dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
    dplyr::distinct(plotEvent)

  #   Identify fully sampled plots in input AI data
  inputPlotEvent <- vstTestDF$vst_apparentindividual %>%
    dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
    dplyr::filter(!plotEvent %in% plotEventPartial$plotEvent) %>%
    dplyr::distinct(plotEvent)

  inputPlotEvent <- inputPlotEvent$plotEvent

  #   Identify fully sampled plots in input NW data
  nwPlotEvent <- vstTestDF$`vst_non-woody` %>%
    dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
    dplyr::filter(!plotEvent %in% plotEventPartial$plotEvent) %>%
    dplyr::distinct(plotEvent)

  nwPlotEvent <- nwPlotEvent$plotEvent

  #   Identify plot-events with zero biomass (no AI or NW records)
  zeroPlotEvent <- vstTestDF$vst_perplotperyear %>%
    dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
    dplyr::filter(targetTaxaPresent == "N",
                  !plotEvent %in% plotEventPartial$plotEvent) %>%
    dplyr::distinct(plotEvent)

  zeroPlotEvent <- zeroPlotEvent$plotEvent

  #   Consolidate relevant input plot-events
  inputPlotEvent <- sort(unique(c(inputPlotEvent, nwPlotEvent, zeroPlotEvent)))


  ##  Derive expected 'plot-events' from output data set
  #   Get 'plot-events' from plot-level output table
  outputPlotEvent <- estimateWoodMassOutputs$vst_plot_Mgha %>%
    dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
    dplyr::filter(!plotEvent %in% plotEventPartial$plotEvent) %>%
    dplyr::distinct(plotEvent)

  outputPlotEvent <- outputPlotEvent$plotEvent

  #   Get 'plot-events' from 'missing' output table
  missingPlotEvent <- estimateWoodMassOutputs$vst_missing %>%
    dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
    dplyr::filter(!plotEvent %in% plotEventPartial$plotEvent) %>%
    dplyr::distinct(plotEvent)

  missingPlotEvent <- missingPlotEvent$plotEvent

  #   Consolidate relevant output plot-events
  outputPlotEvent <- sort(unique(c(outputPlotEvent, missingPlotEvent)))


  ##  Conduct identical plot-event test
  testthat::expect_identical(object = outputPlotEvent,
                             expected = inputPlotEvent)
})
