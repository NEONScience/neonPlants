### Unit tests for joinAquClipHarvest function ####
### POC: Madaline Ritter, ritterm1@BattelleEcology.org

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
  
  desc = joinAquClipHarvest(inputDataList = testList)

  testthat::expect_s3_class(desc[[1]], class = "data.frame")
  testthat::expect_s3_class(desc[[2]], class = "data.frame")
})

#   Test table input
testthat::test_that(desc = "Output class table input", {
  
  desc = joinAquClipHarvest(inputBio = testBio,
                            inputClip = testClip,
                            inputTaxProc = testTaxProc,
                            inputMorph = testMorph)
  
  testthat::expect_s3_class(desc[[1]], class = "data.frame")
  testthat::expect_s3_class(desc[[2]], class = "data.frame")
})



### Test: Function generates data frames with expected dimensions using test data ####
##  Test list input
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number list input", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  
  testthat::expect_identical(object = nrow(out$joinedBiomass),
                             expected = as.integer(7))
  testthat::expect_identical(object = nrow(out$fieldTaxonomy),
                             expected = as.integer(13))
})


#   Check expected column number of output
testthat::test_that(desc = "Output data frame column number list input", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  
  testthat::expect_identical(object = ncol(out$joinedBiomass),
                             expected = as.integer(104))
  
  testthat::expect_identical(object = ncol(out$fieldTaxonomy),
                             expected = as.integer(76))
})


##  Test table inputs
#   Check expected row number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  out <- joinAquClipHarvest(inputBio = testBio,
                            inputClip = testClip,
                            inputTaxProc = testTaxProc,
                            inputMorph = testMorph)
  
  testthat::expect_identical(object = nrow(out$joinedBiomass),
                             expected = as.integer(7))
  testthat::expect_identical(object = nrow(out$fieldTaxonomy),
                             expected = as.integer(13))
})

#   Check expected column number of output
testthat::test_that(desc = "Output data frame row number table input", {
  
  out <- joinAquClipHarvest(inputBio = testBio,
                            inputClip = testClip,
                            inputTaxProc = testTaxProc,
                            inputMorph = testMorph)
  
  testthat::expect_identical(object = ncol(out$joinedBiomass),
                             expected = as.integer(104))
  
  testthat::expect_identical(object = ncol(out$fieldTaxonomy),
                             expected = as.integer(76))
})



### Test: Function joins biomass data correctly using test data ####
##  Test dataframe output 
#   Check 'acceptedTaxonID' is pulled from apc_taxonomyProcessed if taxProc data exists
testthat::test_that(desc = "Output data frame source: taxonomyProcessed", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  
  testthat::expect_identical(object = unique(out$joinedBiomass$taxonIDSourceTable[which(out$joinedBiomass$sampleID == 'BLUE.20230717.MACROALGAE1.Q8')]),
                             expected = "apl_taxonomyProcessed")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'BLUE.20230717.MACROALGAE1.Q8')]),
                             expected = "NEONDREX309000")
})


#   Check 'acceptedTaxonID' is pulled from apc_morphospecies if identification is in morphospecies table
testthat::test_that(desc = "Output data frame source: apc_morphospecies", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  
  testthat::expect_identical(object = unique(out$joinedBiomass$taxonIDSourceTable[which(out$joinedBiomass$sampleID == 'BLUE.20230717.AP1.Q2')]),
                             expected = "apc_morphospecies")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'BLUE.20230717.AP1.Q2')]),
                             expected = "LURE2")
  
  testthat::expect_identical(object = unique(out$joinedBiomass$taxonIDSourceTable[which(out$joinedBiomass$sampleID == 'FLNT.20230724.AP2.P3')]),
                             expected = "apc_morphospecies")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'FLNT.20230724.AP2.P3')]),
                             expected = "SEAP")
})


#   Check 'acceptedTaxonID' is pulled from apl_biomass if identification is not in morphospecies or taxProcessed tables
testthat::test_that(desc = "Output data frame source: biomass", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  testthat::expect_identical(object = unique(out$joinedBiomass$taxonIDSourceTable[which(out$joinedBiomass$sampleID == 'FLNT.20230724.MACROALGAE1.P1')]),
                             expected = "apl_biomass")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'FLNT.20230724.MACROALGAE1.P1')]),
                             expected = "UNKALG")
  
  testthat::expect_identical(object = unique(out$joinedBiomass$taxonIDSourceTable[which(out$joinedBiomass$sampleID == 'TOOK.20230726.AP3.P6')]),
                             expected = "apl_taxonomyProcessed")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'TOOK.20230726.AP3.P6')]),
                             expected = "NEONDREX1220001")
  
  testthat::expect_identical(object = unique(out$joinedBiomass$taxonIDSourceTable[which(out$joinedBiomass$sampleID == 'BLUE.20230717.AP3.Q2')]),
                             expected = "apl_biomass")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'BLUE.20230717.AP3.Q2')]),
                             expected = "RIFL4")
  
  testthat::expect_identical(object = unique(out$joinedBiomass$taxonIDSourceTable[which(out$joinedBiomass$sampleID == 'BLUE.20230717.AP2.Q2')]),
                             expected = "apl_biomass")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'BLUE.20230717.AP2.Q2')]),
                             expected = "LERI6")
})



### Test: Generate joinedBiomass dataframe with correct taxonomic IDs ####
##  Test dataframe output 
#   Check tax info is correct when sampleID has >1 taxonID in apl_taxonomyProcessed and max algalParameterValue is unique
testthat::test_that(desc = "Output taxonomy correct: multiple taxa per sampleID, single max algalParamValue", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'TOOK.20230726.AP3.P6')]),
                             expected = "NEONDREX1220001")
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'BLUE.20230717.MACROALGAE1.Q8')]),
                             expected = "NEONDREX309000")
  
  testthat::expect_identical(object = unique(out$joinedBiomass$additionalTaxa[which(out$joinedBiomass$sampleID == 'TOOK.20230726.AP3.P6')]),
                             expected = "NEONDREX885004|AUDSP|NEONDREX920001|NITELLASP")
  testthat::expect_identical(object = unique(out$joinedBiomass$additionalTaxa[which(out$joinedBiomass$sampleID == 'BLUE.20230717.MACROALGAE1.Q8')]),
                             expected = NA_character_)
  
})

#   Check tax info is correct when sampleID has >1 taxonID in apl_taxonomyProcessed and max algalParameterValue is unique
testthat::test_that(desc = "Output additional taxa correct: multiple taxa per sampleID, many max algalParamValue", {

  #   modify test data
  testList2 <- testList
  testList2$apl_taxonomyProcessed <- testList2$apl_taxonomyProcessed %>% dplyr::filter(algalParameterValue != 5)
  out <- joinAquClipHarvest(inputDataList = testList2)
  
  testthat::expect_identical(object = unique(out$joinedBiomass$acceptedTaxonID[which(out$joinedBiomass$sampleID == 'TOOK.20230726.AP3.P6')]),
                             expected = "NEONDREX885004")

  testthat::expect_identical(object = unique(out$joinedBiomass$additionalTaxa[which(out$joinedBiomass$sampleID == 'TOOK.20230726.AP3.P6')]),
                             expected = "AUDSP|NEONDREX920001|NITELLASP")
})


#   Check 'acceptedTaxonID' is empty when only 1 taxonID exists per sampleID in apl_taxonomyProcessed
testthat::test_that(desc = "Output additional taxa correct: single taxon per sampleID", {
  
  #   modify test data
  testList3 <- testList
  testList3$apl_taxonomyProcessed <- testList3$apl_taxonomyProcessed %>% dplyr::filter(siteID == 'BLUE')
  out <- joinAquClipHarvest(inputDataList = testList3)
  
  testthat::expect_identical(object = unique(out$joinedBiomass$additionalTaxa),
                            expected = NA_character_)

})



### Test: Generate fieldTaxonomy dataframe with correct taxonomic IDs ####
##  Test dataframe output 
#   Check each fieldID has correct number of associated taxa
testthat::test_that(desc = "Output fieldTaxonomy: multiple rows per fieldID", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  
  testthat::expect_identical(object = as.numeric(sum(out$fieldTaxonomy$fieldID == 'BLUE.20230717.QUADRAT.Q2', na.rm = TRUE)),
                             expected = 3)
  testthat::expect_identical(object = as.numeric(sum(out$fieldTaxonomy$fieldID == 'TOOK.20230726.RAKE.P6', na.rm = TRUE)),
                             expected = 5)
  
})

#   Check each fieldID has correct joined taxonIDs
testthat::test_that(desc = "Output fieldTaxonomy: correct taxa per fieldID", {
  
  out <- joinAquClipHarvest(inputDataList = testList)
  
  testthat::expect_identical(object = unique(out$fieldTaxonomy$acceptedTaxonID[which(out$fieldTaxonomy$fieldID == 'BLUE.20230717.QUADRAT.Q2')]),
                             expected = c("RIFL4", "LERI6", "LURE2"))
  testthat::expect_identical(object = unique(out$fieldTaxonomy$acceptedTaxonID[which(out$fieldTaxonomy$fieldID == 'TOOK.20230726.RAKE.P6')]),
                             expected = c("NEONDREX1220001", "NEONDREX885004", "AUDSP", "NEONDREX920001", "NITELLASP"))
  
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


### Test: Generate expected message when apl_taxProcessed isn't provided (works for inputDataList or inputMorph source) ####
# Test when inputMorph lacks required column
testthat::test_that(desc = "Message: expert tax data not provided", {
  
  testthat::expect_message(object = joinAquClipHarvest(inputMorph = testMorph,
                                                      inputBio = testBio,
                                                      inputClip = testClip),
                         regexp = "Output tables do not include identifications from the expert taxonomists.")
})



