### Scratchpad for stackPlantPresence dev and testing ####


### Testing: Run all DIV data through function
div <- neonUtilities::loadByProduct(dpID = "DP1.10058.001",
                                    site = "all",
                                    release = "LATEST",
                                    check.size = FALSE,
                                    token = Sys.getenv("NEON_TOKEN"))

divOut <- neonPlants::stackPlantPresence(inputDataList = div)
#--> Appears to work with no issues for all DIV data
