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


#--> Focus only on those checks within woodProd function...
### Tests: Generate expected errors for 'inputDataList'
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = neonPlants::estimateWoodMass(inputDataList = vstTestDF$vst_apparentindividual),
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct()")
})






