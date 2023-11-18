# load neonUtilities
library (neonUtilities)
library(dplyr)

# install latest dev version
detach(package:neonPlants)
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
data_stacked <- stackPlantPresence(
  divDataList = allDiv)

data_stacked %>% names()
data_stacked$subplotID %>% unique()


# make your own list and stack the data
my_1m_data <- allDiv$div_1m2Data
my_10_100m_data <- allDiv$div_10m2Data100m2Data

data_stacked <- stackPlantPresence(
  divDataList = list(
    div_1m2Data = my_1m_data,
    div_10m2Data100m2Data = my_10_100m_data))

# list with pipe
data_stacked <- list(div_1m2Data = allDiv$div_1m2Data,
                     div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data) %>%
  stackPlantPresence()



# send list of data using pipe
data_stacked <- allDiv |>
  stackPlantPresence()

# filter to 10m plots
data_stacked_10 <- allDiv |>
  stackPlantPresence(totalSampledAreaFilter = 10)

# filter to 10m plots
data_stacked_10 <- allDiv %>%
  stackPlantPresence() %>%
  filter(totalSampledArea == 10)


# stack the data
data_stacked <- stackPlantPresence(
  div_1m2Data = allDiv$div_1m2Data,
  div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data)

# warning when sending a list and dataframes
data_stacked <- stackPlantPresence(
  divDataList = allDiv,
  div_1m2Data = allDiv$div_1m2Data,
  div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data)

# informative errors
data_stacked <- stackPlantPresence(
  divDataList = NA,
  div_1m2Data = NA,
  div_10m2Data100m2Data = NA)

data_stacked <- stackPlantPresence(
  divDataList = NA,
  div_1m2Data = my_1m_data,
  div_10m2Data100m2Data = NA)

data_stacked <- stackPlantPresence(
  divDataList = my_1m_data,
  div_1m2Data = NA,
  div_10m2Data100m2Data = NA)




# stack the data and filter to 10m plot
data_stacked_10m <- stackPlantPresence(
  div_1m2Data = allDiv$div_1m2Data,
  div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data,
  totalSampledAreaFilter = 10)

data_stacked_10m %>% names()
data_stacked_10m$subplotID %>% unique()

# stack the data and filter to 10m plot
data_stacked_10m <- stackPlantPresence(
  div_1m2Data = allDiv$div_1m2Data,
  div_10m2Data100m2Data = allDiv$div_10m2Data100m2Data,
  totalSampledAreaFilter = 34)

data_stacked_10m %>% names()
data_stacked_10m$subplotID %>% unique()


