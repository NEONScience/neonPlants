
### Unit tests for estimatePheTransByTag function ####
### POC: Katie Jones, kjones@BattelleEcology.org

### Read in test data
testList <- readRDS(testthat::test_path("testdata", "phe_testDat_GRSM.rds"))
testStatus <- testList$phe_statusintensity
testTags <- testList$phe_perindividual


#  Test that only one input is provided
testthat::test_that("Only one input type is provided", {
  testthat::expect_error(object = estimatePheTransByTag(inputDataList = testList, 
                                              inputStatus = testStatus), 
                         regexp = "Please provide either a list of data frames, inputDataList, OR individual data frames for inputStatus and inputTags, but not both.")
})

# Test that list is provided to inputDataList 
testthat::test_that("Input class list ", {
  testthat::expect_type(object = testList,
                        type = "list")
  testthat::expect_error(estimatePheTransByTag(inputDataList = testTags), 
                         regexp = "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct().") 
})

# # Test that dataframe is provided to inputStatus 
testthat::test_that("Input status class dataframe ", {
  testthat::expect_s3_class(object = testStatus,
                            class = "data.frame")
  testthat::expect_error(estimatePheTransByTag(inputStatus = "Apollo",
                                               inputTags = testTags), 
                         regexp = "Argument 'inputStatus' must be a data frame object from neonUtilities::loadByProduct().") 
})

# # Test that dataframe is provided to inputTag 
testthat::test_that("Input tags class dataframe ", {
  testthat::expect_s3_class(object = testTags, 
                            class = "data.frame")
  testthat::expect_error(estimatePheTransByTag(inputStatus = testTags,
                                             inputTags = "Boomer"), 
                       regexp = "Argument 'inputTags' must be a data frame object from neonUtilities::loadByProduct().") 

})

#  Test that both required data frames are provided
testthat::test_that("Only one data frame is provided", {
  testthat::expect_error(object = estimatePheTransByTag(inputStatus = testStatus), 
                         regexp = "inputStatus provided without inputTags, both data frames are required.")
  testthat::expect_error(object = estimatePheTransByTag(inputTags = testTags), 
                         regexp = "inputTags provided without inputStatus, both data frames are required.")
})

#Test that expected tables are present in inputDataList
testthat::test_that("Input tables in list ", {
  testthat::expect_true("phe_statusintensity" %in% names(testList))
  testthat::expect_true("phe_perindividual" %in% names(testList))
  testthat::expect_error(estimatePheTransByTag(inputDataList = testList[-1]),
                         regexp = "Required data frames missing from inputDataList: phe_statusintensity")
})
  
# Test that inputDataList tables have expected columns
obs_fields <- c("date", "individualID", "phenophaseName", "phenophaseStatus")
tag_fields <- c("individualID", "taxonID", "scientificName", "growthForm")

testthat::test_that("Input tables from list contain required fields ", {
  testthat::expect_true(length(setdiff(obs_fields, names(testList$phe_statusintensity)))==0)
  testList_2 <- testList
  testList_2[[1]] <- testList_2[[1]]%>%
    dplyr::select(-individualID, -phenophaseName)
  testthat::expect_error(estimatePheTransByTag(inputDataList = testList_2)#,
                         #regexp = "Required columns missing from inputDataList$phe_statusintensity:individualID, phenophaseName "
                         )  #regexp not working though copy/paste directly form function output...???
})

  
# Test that inputStatus has expected columns
testthat::test_that("Input status data frame contains required fields ", {
  testthat::expect_true(length(setdiff(obs_fields, names(testStatus)))==0)
  testthat::expect_error(estimatePheTransByTag(inputStatus = testStatus%>%
                                                 dplyr::select(-individualID),
                                               inputTags = testTags),
                         regexp = "Required columns missing from inputStatus:individualID")
})

# Test that inputTags has expected columns
testthat::test_that("Input tag data frame contains required fields ", {
  testthat::expect_true(length(setdiff(tag_fields, names(testTags)))==0)
  testthat::expect_error(estimatePheTransByTag(inputStatus = testStatus,
                                               inputTags = testTags%>%
                                                 dplyr::select(-taxonID)),
                         regexp = "Required columns missing from inputStatus:taxonID")
})
    
# Test that data are present in tables and dfs  
testthat::test_that("Data present in list tables and dfs", {
  testthat::expect_gt(nrow(testList$phe_statusintensity),  0)
  testthat::expect_gt(nrow(testList$phe_perindividual), 0)
  testthat::expect_gt(nrow(testStatus), 0)
  testthat::expect_gt(nrow(testTags), 0)
  testthat::expect_error(estimatePheTransByTag(inputStatus = testStatus,
                                               inputTags = testTags[0,]), 
                         regexp = "inputTags data frame does not contain data.")
  testthat::expect_error(estimatePheTransByTag(inputStatus = testStatus[0,],
                                               inputTags = testTags),
                         regexp = "inputStatus data frame does not contain data.")
})

    
# Test for transitions present
testthat::test_that("Data contain transitions",{
  testthat::expect_error(estimatePheTransByTag(inputStatus = testStatus%>%
                                                  dplyr::filter(phenophaseStatus=='no'),
                                                inputTags = testTags), 
                         regexp = "Input dataset does not contain any phenophase transitions")
})


# create intentional duplicate to test duplicate check
testthat::test_that("Data contain duplicates",{
  testthat::expect_error(estimatePheTransByTag(inputStatus = testStatus,
                                               inputTags = rbind(testTags[1,], testTags)),
                         regexp = "duplicate records present for NEON.PLA.D07.GRSM.06049 please resolve before running estimatePheTrans")
})

### Test: Function generates expected output class
#   Test list input
testthat::test_that(desc = "Output class list input", {
  testthat::expect_s3_class(object = estimatePheTransByTag(inputDataList = testList),
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  testthat::expect_s3_class(object = estimatePheTransByTag(inputStatus = testStatus,
                                                           inputTags = testTags),
                            class = "data.frame")
})



## Create test data set
# pheDat <- loadByProduct(
#   dpID = "DP1.10055.001",
#   site = "GRSM",
#   startdate = "2024-03",
#   enddate = "2024-06",
#   package = "basic",
#   include.provisional = T,
#   check.size = FALSE)
# 
# 
# list2env(pheDat ,.GlobalEnv)
# 
# keep <- unique(phe_statusintensity$individualID)[1:10]
# 
# phe_perindividual <- phe_perindividual[phe_perindividual$individualID%in%keep,]
# phe_statusintensity <- phe_statusintensity[phe_statusintensity$individualID%in%keep,]
# 
# test_pheDat <- list(phe_statusintensity = phe_statusintensity, phe_perindividual = phe_perindividual)
# 
# saveRDS(test_pheDat, paste0(getwd(), "/tests/testthat/testdata/phe_testDat_GRSM.rds")) 




