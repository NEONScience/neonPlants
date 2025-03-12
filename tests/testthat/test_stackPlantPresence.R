### Unit tests for stackPlantPresence function ####
### POC: Dave Barnett, dbarnett@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "stackPlantPresence_testData.rds"))
test_div_1m2Data <- testList$div_1m2Data
test_div_10m2Data100m2Data <- testList$div_10m2Data100m2Data



### Test: Function generates expected output type
#   Test list input
testthat::test_that(desc = "Output type list input", {
  
  testthat::expect_type(object = stackPlantPresence(inputDataList = testList),
                        type = "list")
  
})

#   Test table input
testthat::test_that(desc = "Output type table input", {
  
  testthat::expect_type(object = stackPlantPresence(input_1m2Data = test_div_1m2Data,
                                                    input_10m2Data100m2Data = test_div_10m2Data100m2Data),
                        type = "list")
})



### Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list input", {
  
  testthat::expect_s3_class(object = stackPlantPresence(inputDataList = testList),
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  testthat::expect_s3_class(object = stackPlantPresence(input_1m2Data = test_div_1m2Data,
                                                        input_10m2Data100m2Data = test_div_10m2Data100m2Data),
                            class = "data.frame")
})



### Test: Function converts old subplotID types correctly
testthat::test_that(desc = "subplotID contains only the expected values", {
  
  # Filter and mutate the first dataframe
  filtered_1m2Data <- test_div_1m2Data %>%
    dplyr::filter(uid == "b2313e67-aa8d-48e2-8689-5b2781f9ee68") %>%
    dplyr::mutate(subplotID = replace(subplotID, 
                                      subplotID == "31_1_1",
                                      "31.1.1"))
  
  # Filter and mutate the second dataframe
  filtered_10m2Data <- test_div_10m2Data100m2Data %>%
    dplyr::filter(uid %in% c("bbcbcee8-e8c2-48b0-a1ff-8b65e4fa08f4", "c8876aed-69d9-40d9-9cfe-1927a9340f2a")) %>%
    dplyr::mutate(subplotID = dplyr::case_when(subplotID == "31_10_1" ~ "31.1.10",
                                               subplotID == "31_100" ~ "31",
                                               TRUE ~ subplotID))
  
  # Call stackPlantPresence with the filtered dataframes
  result <- stackPlantPresence(input_1m2Data = filtered_1m2Data, 
                               input_10m2Data100m2Data = filtered_10m2Data)
  
  # Test that all subplotID values in the result are in the allowed set
  testthat::expect_true(
    all(result$subplotID %in% c("31_1_1", "31_10_1", "31_100", "400")),
    info = "Not all values in subplotID are from the allowed set"
  )
})



### Test: Function generates data frame with expected dimensions using test data
##  Test list input
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number list input", {
  
  testthat::expect_identical(object = nrow(stackPlantPresence(inputDataList = testList)),
                             expected = as.integer(320))
})


#   Check expected column number of output
testthat::test_that(desc = "Output data frame column number list input", {
  
  testthat::expect_identical(object = ncol(stackPlantPresence(inputDataList = testList)),
                             expected = as.integer(29))
})


##  Test table inputs
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = nrow(stackPlantPresence(input_1m2Data = test_div_1m2Data,
                                                              input_10m2Data100m2Data = test_div_10m2Data100m2Data)),
                             expected = as.integer(320))
})

#   Check expected column number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = ncol(stackPlantPresence(input_1m2Data = test_div_1m2Data,
                                                              input_10m2Data100m2Data = test_div_10m2Data100m2Data)),
                             expected = as.integer(29))
})



### Tests: Generate expected errors for 'inputDataList' ####
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  
  testthat::expect_error(object = stackPlantPresence(inputDataList = test_div_1m2Data),
                         regexp = "Argument 'inputDataList' must be a list object")
})

#   Test 'inputDataList' contains required tables
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  
  testthat::expect_error(object = stackPlantPresence(inputDataList = testList[1]),
                         regexp = "Required tables missing from 'inputDataList'")
})

#   Test table inputs are NA if 'inputDataList' supplied
testthat::test_that(desc = "Table inputs NA when required", {
  
  testthat::expect_error(object = stackPlantPresence(inputDataList = testList,
                                                     input_1m2Data = test_div_1m2Data),
                         regexp = "When 'inputDataList' is supplied all table input arguments must be NA")
})

#   Test table inputs are NA if 'inputDataList' supplied
testthat::test_that(desc = "Table inputs NA when required", {
  
  testthat::expect_error(object = stackPlantPresence(inputDataList = testList,
                                                     input_10m2Data100m2Data = test_div_10m2Data100m2Data),
                         regexp = "When 'inputDataList' is supplied all table input arguments must be NA")
})



### Tests: Generate expected errors with table inputs ####
testthat::test_that(desc = "Table inputs are data frames when required", {
  
  testthat::expect_error(object = stackPlantPresence(input_1m2Data = testList,
                                                     input_10m2Data100m2Data = test_div_10m2Data100m2Data),
                         regexp = "Data frames must be supplied for all table inputs if 'inputDataList' is missing")
})



### Test: Generate expected errors for issues with div_1m2Data table (works for inputDataList or input_1m2Data source)
# Test when div_1m2Data lacks required column
testthat::test_that(desc = "Table 'input_1m2Data' missing column", {
  
  testthat::expect_error(object = stackPlantPresence(input_1m2Data = test_div_1m2Data %>%
                                                      dplyr::select(-taxonID),
                                                     input_10m2Data100m2Data = test_div_10m2Data100m2Data),
                         regexp = "Required columns missing from 'input_1m2Data': taxonID")
})

#   Test when div_1m2Data has no data
testthat::test_that(desc = "Table 'div_1m2Data' missing data", {
  
  testthat::expect_error(object = stackPlantPresence(input_1m2Data = test_div_1m2Data %>%
                                                       dplyr::filter(uid == "coconut"),
                                                     input_10m2Data100m2Data = test_div_10m2Data100m2Data),
                         regexp = "Table 'input_1m2Data' has no data.")
})



### Test: Generate expected errors for issues with div_10m2Data100m2Data table (works for inputDataList or div_10m2Data100m2Data source)
# Test when div_10m2Data100m2Data lacks required column
testthat::test_that(desc = "Table 'input_10m2Data100m2Datainput_10m2Data100m2Data' missing column", {
  
  testthat::expect_error(object = stackPlantPresence(input_1m2Data = test_div_1m2Data,
                                                     input_10m2Data100m2Data = test_div_10m2Data100m2Data %>%
                                                       dplyr::select(-taxonID)),
                         regexp = "Required columns missing from 'input_10m2Data100m2Data': taxonID")
})

#   Test when div_10m2Data100m2Data has no data
testthat::test_that(desc = "Table 'div_10m2Data100m2Data' missing data", {
  
  testthat::expect_error(object = stackPlantPresence(input_1m2Data = test_div_1m2Data,
                                                     input_10m2Data100m2Data = test_div_10m2Data100m2Data %>%
                                                       dplyr::filter(uid == "doppelganger")),
                         regexp = "Table 'input_10m2Data100m2Data' has no data.")
})

