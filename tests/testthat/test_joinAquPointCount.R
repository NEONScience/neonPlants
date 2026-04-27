### Unit tests for joinAquPointCount function ####
### POC: Madaline Ritter, ritterm1@BattelleEcology.org


### Read in test data ####
testList <- readRDS(testthat::test_path("testdata", "joinAquPointCount_testData_202307.rds"))

testPoint <- testList$apc_pointTransect
testPerTax <- testList$apc_perTaxon
testTaxProc <- testList$apc_taxonomyProcessed
testTaxRaw <- testList$apc_taxonomyRaw
testMorph <- testList$apc_morphospecies


  
### Test: Function generates expected output type ####
#   Test list input
testthat::test_that(desc = "Output type list input", {
  
  testthat::expect_type(object = joinAquPointCount(inputDataList = testList),
                        type = "list")
  
})

#   Test table input
testthat::test_that(desc = "Output type table input", {
  
  testthat::expect_type(object = joinAquPointCount(inputPoint = testPoint,
                                                   inputPerTax = testPerTax,
                                                   inputTaxonomy = testTaxProc,
                                                   inputMorph = testMorph),
                        type = "list")
})



### Test: Function generates expected output class ####
#   Test list input
testthat::test_that(desc = "Output class list input", {
  
  testthat::expect_s3_class(object = joinAquPointCount(inputDataList = testList),
                            class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  testthat::expect_s3_class(object = joinAquPointCount(inputPoint = testPoint,
                                                       inputPerTax = testPerTax,
                                                       inputTaxonomy = testTaxProc,
                                                       inputMorph = testMorph),
                            class = "data.frame")
})



### Test: Function generates data frame with expected dimensions using test data ####
##  Test list input
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number list input", {
  
  testthat::expect_identical(object = nrow(joinAquPointCount(inputDataList = testList)),
                             expected = as.integer(145))
})


#   Check expected column number of output
testthat::test_that(desc = "Output data frame column number list input", {
  
  testthat::expect_identical(object = ncol(joinAquPointCount(inputDataList = testList)),
                             expected = as.integer(74))
})


##  Test table inputs
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  testthat::expect_identical(object = nrow(joinAquPointCount(inputPoint = testPoint,
                                                             inputPerTax = testPerTax,
                                                             inputTaxonomy = testTaxRaw,
                                                             inputMorph = testMorph)),
                             expected = as.integer(145))
})

#   Check expected column number of output
testthat::test_that(desc = "Output data frame column number table input", {
  
  testthat::expect_identical(object = ncol(joinAquPointCount(inputPoint = testPoint,
                                                             inputPerTax = testPerTax,
                                                             inputTaxonomy = testTaxRaw,
                                                             inputMorph = testMorph)),
                             expected = as.integer(74))
})



### Test: Generates expected data using test data ####
##  Test dataframe output 
#   Check 'acceptedTaxonID' is pulled from apc_perTaxon if identification is not in morphospecies or taxonomy tables
testthat::test_that(desc = "Output data frame source: taxonomyProcessed", {
  
  outDF <- joinAquPointCount(inputDataList = testList)
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'HOPB.20230727.AP2.1.T1')]),
                             expected = "apc_perTaxon")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'HOPB.20230727.AP2.1.T1')]),
                             expected = "PLLE3")
  
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'POSE.20230718.AP16.1.T7')]),
                             expected = "apc_perTaxon")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'POSE.20230718.AP16.1.T7')]),
                             expected = "2PLANT")
})

#   Check 'acceptedTaxonID' is pulled from apc_taxonomyProcessed when expertTaxonomyRequired = 'Y'
testthat::test_that(desc = "Output data frame source: taxonomyProcessed", {
  
  outDF <- joinAquPointCount(inputDataList = testList)
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'KING.20230719.MACROALGAE17.T5')]),
                             expected = "apc_taxonomyProcessed")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'KING.20230719.MACROALGAE17.T5')]),
                             expected = c("NEONDREX1220001", "NEONDREX444000"))
  
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'WLOU.20230703.AP19.1.T7')]),
                             expected = "apc_taxonomyProcessed")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'WLOU.20230703.AP19.1.T7')]),
                             expected = "HYOC5")
  
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'REDB.20230719.AP19.1.T4')]),
                             expected = "apc_taxonomyProcessed")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'REDB.20230719.AP19.1.T4')]),
                             expected = "EQAR")
})

#   Check 'acceptedTaxonID' is pulled from apc_taxonomRaw when expertTaxonomyRequired = 'Y' and only apc_taxonomyRaw is provided
testthat::test_that(desc = "Output data frame source: taxonomyRaw", {
  tempTestList <- testList
  tempTestList$apc_taxonomyProcessed <- NULL
  outDF <- joinAquPointCount(inputDataList = tempTestList)
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'KING.20230719.MACROALGAE17.T5')]),
                             expected = "apc_taxonomyRaw")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'KING.20230719.MACROALGAE17.T5')]),
                             expected = c("NEONDREX444000", "NEONDREX1220001"))
  
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'WLOU.20230703.AP19.1.T7')]),
                             expected = "apc_taxonomyRaw")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'WLOU.20230703.AP19.1.T7')]),
                             expected = "HYOC5")
  
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'REDB.20230719.AP19.1.T4')]),
                             expected = "apc_taxonomyRaw")
  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'REDB.20230719.AP19.1.T4')]),
                             expected = "EQAR")
})


#   Check 'acceptedTaxonID' is pulled from apc_morphospecies if identification is in morphospecies table and not apc_taxonomyProcessed
testthat::test_that(desc = "Output data frame source: apc_morphospecies", {
  
  outDF <- joinAquPointCount(inputDataList = testList)
  testthat::expect_identical(object = unique(outDF$taxonIDSourceTable[which(outDF$sampleID == 'KING.20230719.AP11.1.T1')]),
                             expected = "apc_morphospecies")

  testthat::expect_identical(object = unique(outDF$acceptedTaxonID[which(outDF$sampleID == 'KING.20230719.AP11.1.T1')]),
                             expected = "NAOF")
})



### Tests: Generate expected errors for 'inputDataList' ####
#   Test 'inputDataList' is a list
testthat::test_that(desc = "Argument 'inputDataList' is list object", {
  
  testthat::expect_error(object = joinAquPointCount(inputDataList = testPoint),
                         regexp = "Argument 'inputDataList' must be a list object")
})

#   Test 'inputDataList' contains required tables
testthat::test_that(desc = "Required tables present in 'inputDataList' input", {
  
  testthat::expect_error(object = joinAquPointCount(inputDataList = testList[1:2]),
                         regexp = "Required tables missing from 'inputDataList'")
})

#   Test table inputs are NA if 'inputDataList' supplied
testthat::test_that(desc = "Table inputs NA when required", {
  
  testthat::expect_error(object = joinAquPointCount(inputDataList = testList,
                                               inputPoint = testPoint),
                         regexp = "When 'inputDataList' is supplied, all table input arguments must be NA.")
})



### Tests: Generate expected errors with table inputs ####
testthat::test_that(desc = "Table inputs are data frames when required", {
  
  testthat::expect_error(object = joinAquPointCount(inputPoint = testPoint,
                                                    inputTaxonomy = testTaxProc),
                         regexp = "Data frames must be supplied for table inputs if 'inputDataList' is missing")
})



### Test: Generate expected errors for issues with pointCount table (works for inputDataList or inputPoint source) ####
# Test when inputPoint lacks required column
testthat::test_that(desc = "Table 'inputPoint' missing column", {
  
  testthat::expect_error(object = joinAquPointCount(inputPoint = testPoint %>%
                                                      dplyr::select(-pointNumber),
                                                    inputPerTax = testPerTax),
                         regexp = "Required columns missing from 'inputPoint': pointNumber")
})

#   Test when inputPoint has no data
testthat::test_that(desc = "Table 'inputPoint' missing data", {
  
  testthat::expect_error(object = joinAquPointCount(inputPoint = testPoint %>%
                                                      dplyr::filter(pointNumber == "999"),
                                                    inputPerTax = testPerTax),
                         regexp = "Table 'inputPoint' has no data.")
})


### Test: Generate expected errors for issues with perTaxon table (works for inputDataList or inputPerTax source) ####
# Test when inputPerTax lacks required column
testthat::test_that(desc = "Table 'inputPerTax' missing column", {
  
  testthat::expect_error(object = joinAquPointCount(inputPerTax = testPerTax %>%
                                                      dplyr::select(-taxonID),
                                                    inputPoint = testPoint),
                         regexp = "Required columns missing from 'inputPerTax': taxonID")
})

#   Test when inputPerTax has no data
testthat::test_that(desc = "Table 'inputPerTax' missing data", {
  
  testthat::expect_error(object = joinAquPointCount(inputPerTax = testPerTax %>%
                                                      dplyr::filter(taxonID == "coconut"),
                                                    inputPoint = testPoint),
                         regexp = "Table 'inputPerTax' has no data.")
})



### Test: Generate expected errors for issues with taxonomy table (works for inputDataList or inputTaxonomy source) ####
# Test when inputTaxonomy  lacks required column
testthat::test_that(desc = "Table 'inputTaxonomy' missing column", {
  
  testthat::expect_error(object = joinAquPointCount(inputTaxonomy = testTaxProc %>%
                                                      dplyr::select(-acceptedTaxonID),
                                                    inputPoint = testPoint,
                                                    inputPerTax = testPerTax),
                         regexp = "Required columns missing from 'inputTaxonomy': acceptedTaxonID")
})



### Test: Generate expected errors for issues with morphospecies table (works for inputDataList or inputMorph source) ####
# Test when inputMorph lacks required column
testthat::test_that(desc = "Table 'inputMorph' missing column", {
  
  testthat::expect_error(object = joinAquPointCount(inputMorph = testMorph %>%
                                                      dplyr::select(-taxonID),
                                                    inputPoint = testPoint,
                                                    inputPerTax = testPerTax),
                         regexp = "Required columns missing from 'inputMorph': taxonID")
})
