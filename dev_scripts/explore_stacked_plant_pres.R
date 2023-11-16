# load neonUtilities
library (neonUtilities)

# install dev version
devtools::install_github("NEONScience/neonPlants@dev")
library(neonPlants)

# # get data

# allDiv <- loadByProduct(
#   dpID = "DP1.10058.001",
#   site = "SRER",
#   startdate = "2010-01",
#   package = "basic",
#   check.size = FALSE)
#
# saveRDS(allDiv,"dev_scripts/allDiv.RDS")

allDiv <- readRDS("dev_scripts/allDiv.RDS")

# stack the data - rename to stackPlantOccurrence or stackPlantPresence?
data_stacked <- stackPlantPresence(
  divDataList = allDiv)


# explore stacked data
names(data_stacked)

data_stacked$scientificName %>% unique()

data_no_sci_name <- data_stacked %>%
  filter(is.na(scientificName))

data_no_taxonID <- data_stacked %>%
  filter(is.na(taxonID))
# data_no_sci_name_old <- data_no_sci_name_old

# old data_stacked record count = 87428
# with dedpub by primary key count = 87256
