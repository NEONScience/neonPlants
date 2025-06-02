# estimateMass function tests
# Samuel M Simkin (2025-04-02)  ssimkin@battelleecology.org



### Read in test data

VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))
HbpDat <- readRDS(testthat::test_path("testdata", "HbpDat.rds"))
BbcDat <- readRDS(testthat::test_path("testdata", "BbcDat.rds"))

estimateMassOutputs <- estimateMass(dataProducts = c("Vst","Hbp","Bbc"),
                                        inputDataListVst = VstDat,
                                        inputDataListHbp = HbpDat,
                                        inputDataListBbc = BbcDat,
                                        plotSubset = "all"
)

### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateMass(dataProducts = c("Vst","Hbp","Bbc"),
                                              inputDataList = VstDat,
                                              inputDataListHbp = HbpDat,
                                              inputDataListBbc = BbcDat),
                        type = "list")
})



### Test: Function generates expected output class
testthat::test_that(desc = "Output class biomass_site", {
  testthat::expect_s3_class(object = estimateMassOutputs$biomass_site,
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data
#   Check expected column number of data frame
testthat::test_that(desc = "Output data frame column number", {
  testthat::expect_identical(object = ncol(estimateMassOutputs$biomass_site),
                             expected = as.integer(13))
})


#   Check expected row number of data frame
testthat::test_that(desc = "Output data frame row number", {
  testthat::expect_identical(object = nrow(estimateMassOutputs$biomass_site),
                             expected = as.integer(15))
})


### Tests: Generate expected errors for 'dataProducts' ####
#   Test 'dataProducts' includes appropriate dataProduct option(s)
testthat::test_that(desc = "Argument 'dataProducts' includes appropriate dataProduct option(s)", {
  testthat::expect_error(object = estimateMass(dataProducts = c("Wat","Air"), # test whether function stops if supplied with an incorrect dataProduct code
                                               inputDataListVst = VstDat),
                         regexp = "The dataProducts argument must be one of: 'Bbc', 'Hbp', 'Vst', or a comma-separated combination of two or more of these.")
})


### Test: Generate error if output biomass_site value not as expected
testthat::test_that(desc = "Output biomass_site value as expected", {
  test <- estimateMass(dataProducts = c("Vst","Hbp","Bbc"),
                                        inputDataListVst = VstDat,
                                        inputDataListHbp = HbpDat,
                                        inputDataListBbc = BbcDat,
                                        plotSubset = "all")
  testthat::expect_equal(object = test$biomass_site$woodLiveMassMean_Mgha[7],
                         expected = 2.00)
})

