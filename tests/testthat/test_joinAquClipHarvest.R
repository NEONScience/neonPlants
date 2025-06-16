### Unit tests for joinAquClipHarvest function ####
### POC: Madaline Ritter, ritterm1@BattelleEcology.org

### Download initial test data - DELETE
# clip_testDat <- neonUtilities::loadByProduct(
#   dpID="DP1.20066.001",
#   check.size=F,
#   startdate = '2023-07',
#   enddate = '2023-07',
#   site = "all",
#   # site = "SYCA",
#   package='expanded',
#   include.provisional = T,
#   release = "LATEST",
#   token = Sys.getenv('NEON_PAT'))
# 
# clip_testDat$apl_identificationHistory <- NULL
# clip_testDat$apl_taxonomyRaw <- NULL
# clip_testDat$categoricalCodes_20066 <- NULL
# clip_testDat$issueLog_20066 <- NULL
# clip_testDat$readme_20066 <- NULL
# clip_testDat$validation_20066 <- NULL
# clip_testDat$variables_20066 <- NULL
# 
# View(clip_testDat$apc_morphospecies)
# View(clip_testDat$apl_biomass)
# View(clip_testDat$apl_clipHarvest)
# View(clip_testDat$apl_taxonomyProcessed)
# 
# saveRDS(clip_testDat, "C:/Users/ritterm1/Documents/GitHub/a_neonPackages/neonPlants/tests/testthat/testdata/joinAquClipHarvest_testData_202307.rds")
# testList <- readRDS("C:/Users/ritterm1/Documents/GitHub/a_neonPackages/neonPlants/tests/testthat/testdata/joinAquClipHarvest_testData_202307.rds")


### Read in test data
testList <- readRDS(testthat::test_path("testdata", "joinAquClipHarvest_testData_202307.rds"))
testBio <- testList$apl_biomass
testClip <- testList$apl_clipHarvest
testTaxProc <- testList$apl_taxonomyProcessed
testMorph <- testList$apc_morphospecies



### Test: Function generates expected output type ####
#   Test list input
testthat::test_that(desc = "Output type list input", {
  
  testthat::expect_type(object = joinAquClipHarvest(inputDataList = testList),
                        type = "list")
  
})

#   Test table input
testthat::test_that(desc = "Output type table input", {
  
  testthat::expect_type(object = joinAquClipHarvest(inputBio = testBio,
                                                   inputClip = testClip,
                                                   inputTaxProc = testTaxProc,
                                                   inputMorph = testMorph),
                        type = "list")
})



### Test: Function generates expected output class ####
#   Test list input
testthat::test_that(desc = "Output class list input", {
  
  testthat::expect_s3_class(object = joinAquClipHarvest(inputDataList = testList),
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  testthat::expect_s3_class(object = joinAquClipHarvest(inputBio = testBio,
                                                        inputClip = testClip,
                                                        inputTaxProc = testTaxProc,
                                                        inputMorph = testMorph),
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data ####
##  Test list input
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number list input", {
  
  testthat::expect_identical(object = nrow(joinAquClipHarvest(inputDataList = testList)),
                             expected = as.integer(324))
})


#   Check expected column number of output
testthat::test_that(desc = "Output data frame column number list input", {
  
  testthat::expect_identical(object = ncol(joinAquClipHarvest(inputDataList = testList)),
                             expected = as.integer(103))
})


##  Test table inputs
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = nrow(joinAquClipHarvest(inputBio = testBio,
                                                              inputClip = testClip,
                                                              inputTaxProc = testTaxProc,
                                                              inputMorph = testMorph)),
                             expected = as.integer(324))
})

#   Check expected column number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = ncol(joinAquClipHarvest(inputBio = testBio,
                                                              inputClip = testClip,
                                                              inputTaxProc = testTaxProc,
                                                              inputMorph = testMorph)),
                             expected = as.integer(103))
})



### Test: Function generates data frame with expected dimensions using test data ####
##  Test dataframe output 
#   Check 'acceptedTaxonID' is pulled from apc_taxonomyProcessed if taxProc data exists
testthat::test_that(desc = "Output data frame source: taxonomyProcessed", {
  
  outDF <- joinAquClipHarvest(inputDataList = testList)
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'PRLA.20230703.AP1.P6')]),
                             expected = "apl_taxonomyProcessed")
})


#   Check 'acceptedTaxonID' is pulled from apc_morphospecies if identification is in morphospecies table
testthat::test_that(desc = "Output data frame source: apc_morphospecies", {
  
  outDF <- joinAquClipHarvest(inputDataList = testList)
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'BLUE.20230717.AP1.Q2')]),
                             expected = "apc_morphospecies")
  
  
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'BLUE.20230717.AP1.Q2')]),
                             expected = "LURE2")
})


#   Check 'acceptedTaxonID' is pulled from apl_biomass if identification is not in morphospecies or taxProcessed tables
testthat::test_that(desc = "Output data frame source: biomass", {
  
  outDF <- joinAquClipHarvest(inputDataList = testList)
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'CRAM.20230720.AP1.P2')]),
                             expected = "apl_biomass")
  
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'CRAM.20230720.AP1.P2')]),
                             expected = "DRADA")
})



### Tests: Generate expected errors for 'inputDataList' ####
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputDataList = testBio),
                         regexp = "Argument 'inputDataList' must be a list object")
})

#   Test 'inputDataList' contains required tables
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputDataList = testList[1:2]),
                         regexp = "Required tables missing from 'inputDataList'")
})

#   Test table inputs are NA if 'inputDataList' supplied
testthat::test_that(desc = "Table inputs NA when required", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputDataList = testList,
                                                    inputBio = testBio),
                         regexp = "When 'inputDataList' is supplied, all table input arguments must be NA")
})



### Tests: Generate expected errors with table inputs ####
testthat::test_that(desc = "Table inputs are data frames when required", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputMorph = testMorph,
                                                    inputBio = testBio),
                         regexp = "Data frames must be supplied for table inputs if 'inputDataList' is missing")
})



### Test: Generate expected errors for issues with biomass table (works for inputDataList or inputBio source) ####
# Test when inputBio lacks required column
testthat::test_that(desc = "Table 'inputBio' missing column", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputBio = testBio %>%
                                                       dplyr::select(-taxonID),
                                                     inputClip = testClip),
                         regexp = "Required columns missing from 'inputBio': taxonID")
})

#   Test when inputBio has no data
testthat::test_that(desc = "Table 'inputBio' missing data", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputBio = testBio %>%
                                                       dplyr::filter(taxonID == "coconut"),
                                                     inputClip = testClip),
                         regexp = "Table 'inputBio' has no data.")
})


### Test: Generate expected errors for issues with clipHarvest table (works for inputDataList or inputClip source) ####
# Test when inputClip lacks required column
testthat::test_that(desc = "Table 'inputClip' missing column", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputClip = testClip %>%
                                                       dplyr::select(-eventID),
                                                     inputBio = testBio),
                         regexp = "Required columns missing from 'inputClip': eventID")
})

#   Test when inputClip has no data
testthat::test_that(desc = "Table 'inputClip' missing data", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputClip = testClip %>%
                                                       dplyr::filter(eventID == "moon landing"),
                                                     inputBio = testBio),
                         regexp = "Table 'inputClip' has no data.")
})



### Test: Generate expected errors for issues with taxonomyProcessed table (works for inputDataList or inputTaxProc source) ####
# Test when inputTaxProc lacks required column
testthat::test_that(desc = "Table 'inputTaxProc' missing column", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputTaxProc = testTaxProc %>%
                                                      dplyr::select(-taxonID),
                                                     inputBio = testBio,
                                                     inputClip = testClip),
                         regexp = "Required columns missing from 'inputTaxProc': taxonID")
})



### Test: Generate expected errors for issues with morphospecies table (works for inputDataList or inputMorph source) ####
# Test when inputMorph lacks required column
testthat::test_that(desc = "Table 'inputMorph' missing column", {
  
  testthat::expect_error(object = joinAquClipHarvest(inputMorph = testMorph %>%
                                                      dplyr::select(-taxonID),
                                                     inputBio = testBio,
                                                     inputClip = testClip),
                         regexp = "Required columns missing from 'inputMorph': taxonID")
})



