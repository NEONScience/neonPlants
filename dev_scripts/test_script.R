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


# stack the data
data_stacked <- stackPlantDiv(
  div_1m2Data = allDiv$div_1m2Data,
  div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data)


data_stacked %>% names()
data_stacked$subplotID %>% unique()

# stack the data and filter to 10m plot
data_stacked_10m <- stackPlantDiv(
  div_1m2Data = allDiv$div_1m2Data,
  div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data,
  totalSampledAreaFilter = 10)

data_stacked_10m %>% names()
data_stacked_10m$subplotID %>% unique()

# stack the data and filter to 10m plot
data_stacked_10m <- stackPlantDiv(
  div_1m2Data = allDiv$div_1m2Data,
  div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data,
  totalSampledAreaFilter = 34)

data_stacked_10m %>% names()
data_stacked_10m$subplotID %>% unique()


