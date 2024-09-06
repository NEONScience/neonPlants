### Scratchpad for neonPlants root function development ####

#   Retrieve bbc data from NEON Portal
library(dplyr)
library(neonUtilities)



### Testing 
#   Newer site x month combos with rootStatus == NA for all subsampleIDs
bbc1 <- neonUtilities::loadByProduct(dpID = "DP1.10067.001",
                                     site = "all",
                                     startdate = "2022-07",
                                     enddate = "2022-08",
                                     tabl = "all",
                                     check.size = FALSE,
                                     token = Sys.getenv("NEON_TOKEN"))

vars <- bbc1$variables_10067
rootMass1 <- bbc1$bbc_rootmass
rootPool1 <- bbc1$bbc_chemistryPooling
rootChem1 <- bbc1$bbc_rootChemistry

outputTest1 <- root_table_join(input_mass = bbc1$bbc_rootmass,
                              input_pool = bbc1$bbc_chemistryPooling,
                              input_chem = bbc1$bbc_rootChemistry)


#   Older dataset for bbc_rootmass that has rootStatus == "live" and "dead"; stopped sorting in 2021
bbc2 <- neonUtilities::loadByProduct(dpID = "DP1.10067.001",
                                     site = "all",
                                     startdate = "2018-07",
                                     enddate = "2018-08",
                                     tabl = "all",
                                     check.size = FALSE,
                                     token = Sys.getenv("NEON_TOKEN"))

vars <- bbc2$variables_10067
rootCore2 <- bbc2$bbc_percore
rootMass2 <- bbc2$bbc_rootmass
rootPool2 <- bbc2$bbc_chemistryPooling
rootChem2 <- bbc2$bbc_rootChemistry
rootDilution <- bbc2$bbc_dilution

#   Write out test datasets for testthat
testDataPath <- "tests/testthat/testdata"

saveRDS(object = rootCore2,
        file = paste(testDataPath, "valid-rootcore-201807.RDS", sep = "/"))

saveRDS(object = rootMass2, 
        file = paste(testDataPath, "valid-rootmass-201807.RDS", sep = "/"))

saveRDS(object = rootPool2,
        file = paste(testDataPath, "valid-rootpool-201807.RDS", sep = "/"))

saveRDS(object = rootChem2,
        file = paste(testDataPath, "valid-rootchem-201807.RDS", sep = "/"))

saveRDS(object = rootDilution,
        file = paste(testDataPath, "valid-rootdilution-201807.RDS", sep = "/"))

#   Evaluate function output
outputTest2 <- root_table_join(input_mass = rootMass2,
                               input_pool = rootPool2,
                               input_chem = rootChem2)




#   Input with input_mass missing one or more required columns
testMass <- rootMass %>% dplyr::select(-rootStatus)

outputTest3 <- root_table_join(input_mass = testMass,
                              input_pool = bbc2$bbc_chemistryPooling,
                              input_chem = bbc2$bbc_rootChemistry)

#--> returns expected error message for single missing column and when missing two columns

#   Input with input_mass missing all data
testMass <- rootMass %>% dplyr::filter(rootStatus == "unicorn")

outputTest4 <- root_table_join(input_mass = testMass,
                              input_pool = bbc2$bbc_chemistryPooling,
                              input_chem = bbc2$bbc_rootChemistry)



### Evaluate function output for rootMassStandardize()
#   Test for output using testMass data frame
stdTest1 <- neonPlants::rootMassStandardize(inputMass = testMass)

#   Test when no data frame input supplied
stdTest2 <- neonPlants::rootMassStandardize()
#--> expected error 'argument "inputMass" is missing, with no default'

stdTest3 <- neonPlants::rootMassStandardize(inputMass = testMass %>%
                                              dplyr::select(-dryMass))
#--> expected error "Required columns missing from 'inputMass': dryMass

stdTest3 <- neonPlants::rootMassStandardize(inputMass = testMass %>%
                                              dplyr::filter(uid == "mangrove"))
#--> expected error "Table 'inputMass' has no data"



### Evaluate function output for rootMassScale()
##  Test for output with no inputDilution argument
scale1 <- neonPlants::rootMassScale(inputCore = testCore,
                                    inputMass = testMass)
#--> calculations look correct
#--> number of rows is correct given length(unique(testMass$sampleID)) and two cores in testCore having
#--> samplingImpractical == "obstruction".


##  Test for output with inputDilution argument but includeFragments FALSE
scale2 <- neonPlants::rootMassScale(inputCore = testCore,
                                    inputMass = testMass,
                                    inputDilution = testDilution)
#--> scale2$totalDryMass[3] == 5.5857, which correctly does not include fragment mass




