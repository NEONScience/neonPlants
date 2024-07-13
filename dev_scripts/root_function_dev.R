### Scratchpad for neonPlants root function development ####

#   Retrieve bbc data from NEON Portal
library(neonUtilities)

bbc <- neonUtilities::loadByProduct(dpID = "DP1.10067.001",
                                    site = "all",
                                    startdate = "2022-07",
                                    enddate = "2022-08",
                                    tabl = "all",
                                    check.size = FALSE,
                                    token = Sys.getenv("NEON_TOKEN"))

vars <- bbc$variables_10067
bbc_rootmass <- bbc$bbc_rootmass

### Root mass required columns
domainID
siteID
plotID
sampleID
subsampleID
samplingImpractical
dryMass
qaDryMass

### Root pooling required columns
