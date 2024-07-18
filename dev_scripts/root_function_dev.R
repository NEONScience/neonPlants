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
rootMass <- bbc1$bbc_rootmass
rootPool <- bbc1$bbc_chemistryPooling
rootChem <- bbc1$bbc_rootChemistry

inputTest1 <- root_table_join(input_mass = bbc1$bbc_rootmass,
                              input_pool = bbc1$bbc_chemistryPooling,
                              input_chem = bbc1$bbc_rootChemistry)


#   Older dataset for bbc_rootmass that has rootStatus == "live" and "dead"
bbc2 <- neonUtilities::loadByProduct(dpID = "DP1.10067.001",
                                     site = "all",
                                     startdate = "2017-07",
                                     enddate = "2017-08",
                                     tabl = "all",
                                     check.size = FALSE,
                                     token = Sys.getenv("NEON_TOKEN"))

vars <- bbc1$variables_10067
rootMass <- bbc2$bbc_rootmass
rootPool <- bbc2$bbc_chemistryPooling
rootChem <- bbc2$bbc_rootChemistry

inputTest2 <- root_table_join(input_mass = bbc2$bbc_rootmass,
                              input_pool = bbc2$bbc_chemistryPooling,
                              input_chem = bbc2$bbc_rootChemistry)
