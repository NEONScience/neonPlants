# estimateHerbProd function tests
# Samuel M Simkin (2024-12-15)  ssimkin@battelleecology.org
# Courtney Meier (2025-06-20)   cmeier@BattelleEcology.org

### Read in test data ####
HbpDat <- readRDS(testthat::test_path("testdata", "HbpDat.rds"))



### Generate estimateHerbProd outputs ####
estimateHerbProdOutputs <- estimateHerbProd(inputDataList = HbpDat)



### Output type tests ####
### Test: Function generates expected output type
testthat::test_that(desc = "Output type", {
  testthat::expect_type(object = estimateHerbProd(inputDataList = HbpDat),
                        type = "list")
})



### Test: Function generates expected output class
#   Check 'herb_ANPP_site' output table is a data frame
testthat::test_that(desc = "Output class 'herb_ANPP_site'", {
  testthat::expect_s3_class(object = estimateHerbProdOutputs$herb_ANPP_site,
                            class = "data.frame")
})

#   Check 'herb_ANPP_plot' output table is a data frame
testthat::test_that(desc = "Output class 'herb_ANPP_plot'", {
  testthat::expect_s3_class(object = estimateHerbProdOutputs$herb_ANPP_plot,
                            class = "data.frame")
})

#   Check that 'herb_ANPP_plot_herbGroup' output table is a data frame
testthat::test_that(desc = "Output class 'herb_ANPP_plot_herbgroup'", {
  testthat::expect_s3_class(object = estimateHerbProdOutputs$herb_ANPP_plot_herbgroup,
                            class = "data.frame")
})



### Output dimension tests ####
### Test: Function generates data frame with expected dimensions using test data and function defaults (plotSubset = "tower")
#   Check expected column number of 'herb_ANPP_site' data frame
testthat::test_that(desc = "Output 'herb_ANPP_site' column number", {
  testthat::expect_identical(object = ncol(estimateHerbProdOutputs$herb_ANPP_site),
                             expected = as.integer(19))
})

#   Check expected row number of 'herb_ANPP_site' data frame
testthat::test_that(desc = "Output 'herb_ANPP_site' row number", {
  testthat::expect_identical(object = nrow(estimateHerbProdOutputs$herb_ANPP_site),
                             expected = as.integer(13))
})

#   Check expected column number of 'herb_ANPP_plot' data frame
testthat::test_that(desc = "Output 'herb_ANPP_plot' column number", {
  testthat::expect_identical(object = ncol(estimateHerbProdOutputs$herb_ANPP_plot),
                             expected = as.integer(16))
})

#   Check expected row number of 'herb_ANPP_plot' data frame
testthat::test_that(desc = "Output 'herb_ANPP_plot' row number", {
  testthat::expect_identical(object = nrow(estimateHerbProdOutputs$herb_ANPP_plot),
                             expected = as.integer(14))
})

#   Check expected column number of 'herb_ANPP_plot_groups' data frame
testthat::test_that(desc = "Output 'herb_ANPP_plot_herbgroup' column number", {
  testthat::expect_identical(object = ncol(estimateHerbProdOutputs$herb_ANPP_plot_herbgroup),
                             expected = as.integer(16))
})

#   Check expected row number of 'herb_ANPP_plot' data frame
testthat::test_that(desc = "Output 'herb_ANPP_plot_herbgroup' row number", {
  testthat::expect_identical(object = nrow(estimateHerbProdOutputs$herb_ANPP_plot_herbgroup),
                             expected = as.integer(70))
})



### Output value tests ####
#   Check output 'herb_ANPP_site' value is as expected
testthat::test_that(desc = "Output 'herb_ANPP_site' table value as expected", {
  test <- estimateHerbProd(inputDataList = HbpDat)
  testthat::expect_equal(object = test$herb_ANPP_site$herbANPP_Mghayr[1],
                         expected = 0.72)
})

#   Check output 'herb_ANPP_plot' value is as expected
testthat::test_that(desc = "Output 'herb_ANPP_plot' table value as expected", {
  test <- estimateHerbProd(inputDataList = HbpDat)
  testthat::expect_equal(object = test$herb_ANPP_plot$herbANPP_gm2yr[1],
                         expected = 72.38)
})

#   Check output 'herb_ANPP_plot_herbgroup' value is as expected
testthat::test_that(desc = "Output 'herb_ANPP_plot_herbgroup' table value as expected", {
  test <- estimateHerbProd(inputDataList = HbpDat)
  testthat::expect_equal(object = test$herb_ANPP_plot_herbgroup$herbANPP_gm2yr[2],
                         expected = 70.03)
})



### Expected error tests: Generate expected errors for input arguments ####
#   Check that error is produced when inputDataList argument is not a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  testthat::expect_error(object = estimateHerbProd(inputDataList = HbpDat$hbp_perbout),
                         regexp = "The inputDataList argument is expected to be a list")
})

#   Check that 'plotSubset' error is produced when expected
testthat::test_that(desc = "Argument 'plotSubset' is valid", {
  testthat::expect_error(object = estimateHerbProd(inputDataList = HbpDat,
                                                   plotSubset = "fish"),
                         regexp = "The only valid plotSubset options are 'all', 'tower', 'distributed'")
})
