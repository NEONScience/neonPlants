### Unit tests for estimateAquPercentCover function ####
### POC: Madaline Ritter, ritterm1@BattelleEcology.org

# # retrieve test data
# ap <- neonUtilities::loadByProduct(
#   dpID="DP1.20072.001", #APL-clip: DP1.20066.001, APC: DP1.20072.001
#   check.size=F,
#   startdate = '2023-06',
#   enddate = '2023-06',
#   site = c("CUPE", "GUIL"),
#   include.provisional = T,
#   release = "LATEST",
#   token = Sys.getenv('NEON_PAT'))
# 
# ap$categoricalCodes_20072 <- NULL
# ap$issueLog_20072 <- NULL
# ap$readme_20072 <- NULL
# ap$validation_20072 <- NULL  
# ap$variables_20072 <- NULL
# 
# saveRDS(ap, "C:/Users/ritterm1/Documents/GitHub/a_neonPackages/neonPlants/tests/testthat/testdata/estimateAquPercentCover_testData_D04_202306.rds")


### Read in test data ####
testList <- readRDS(testthat::test_path("testdata", "estimateAquPercentCover_testData_D04_202306.rds"))
testPoint <- testList$apc_pointTransect
testPerTax <- testList$apc_perTaxon
testTaxProc <- testList$apc_taxonomyProcessed



##  Test: Function generates expected output type ####
#   Test list input
testthat::test_that(desc = "Output type list input", {
  
  testthat::expect_type(object = estimateAquPercentCover(inputDataList = testList),
                            type = "list")
})

#   Test table input
testthat::test_that(desc = "Output type table input", {  

  testthat::expect_type(object = estimateAquPercentCover(inputPoint = testPoint,
                                                         inputPerTax = testPerTax,
                                                         inputTaxProc = testTaxProc),
                        type = "list")
})


### Test: Function generates expected output class ####
#   Test list input
testthat::test_that(desc = "Output class list input", 
                    {
  desc = estimateAquPercentCover(inputDataList = testList)
  
  testthat::expect_s3_class(desc, class = "list")
  testthat::expect_s3_class(desc[1], class = "data.frame")
  testthat::expect_s3_class(desc[2], class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {  
  
  desc = estimateAquPercentCover(inputPoint = testPoint,
                                 inputPerTax = testPerTax,
                                 inputTaxProc = testTaxProc)
  
  testthat::expect_s3_class(desc, class = "list")
  testthat::expect_s3_class(desc[1], class = "data.frame")
  testthat::expect_s3_class(desc[2], class = "data.frame")
})


### Test: Function generates data frame with expected dimensions using test data ####
##  Test list input
#   Check expected dimensions of output df 1
testthat::test_that(desc = "Output percentCover df dimensions list input", {
  
  out = estimateAquPercentCover(inputDataList = testList)
  
  testthat::expect_identical(object = nrow(out[[1]]),
                             expected = as.integer(106))
  
  testthat::expect_identical(object = ncol(out[[1]]),
                             expected = as.integer(6))
})

#   Check expected dimensions of output df 2
testthat::test_that(desc = "Output transectMetrics df dimensions list input", {
  
  out = estimateAquPercentCover(inputDataList = testList)
  
  testthat::expect_identical(object = nrow(out[[2]]),
                             expected = as.integer(20))
  
  testthat::expect_identical(object = ncol(out[[2]]),
                             expected = as.integer(8))
})

##  Test table inputs
#   Check expected dimensions of output df 1
testthat::test_that(desc = "Output percentCover df dimensions list input", {
  
  out = estimateAquPercentCover(inputPoint = testPoint,
                                inputPerTax = testPerTax,
                                inputTaxProc = testTaxProc)
  
  testthat::expect_identical(object = nrow(out[[1]]),
                             expected = as.integer(106))
  
  testthat::expect_identical(object = ncol(out[[1]]),
                             expected = as.integer(6))
})

#   Check expected dimensions of output df 2
testthat::test_that(desc = "Output transectMetrics df dimensions list input", {
  
  out = estimateAquPercentCover(inputPoint = testPoint,
                                inputPerTax = testPerTax,
                                inputTaxProc = testTaxProc)
  
  testthat::expect_identical(object = nrow(out[[2]]),
                             expected = as.integer(20))
  
  testthat::expect_identical(object = ncol(out[[2]]),
                             expected = as.integer(8))
})





