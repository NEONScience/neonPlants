# load neonUtilities
library (neonUtilities)

# get data
allDiv <- loadByProduct(
  dpID = "DP1.10058.001",
  site = "SRER",
  startdate = "2010-01",
  package = "basic",
  check.size = FALSE)

saveRDS(allDiv,"dev_scripts/allDiv.RDS")
allDiv <- readRDS("dev_scripts/allDiv.RDS")

##extract 1m2 data from list of lists and some processing
data_1m2 <- allDiv[["div_1m2Data"]]
###get 10_100
data_10_100m2 <- allDiv[["div_10m2Data100m2Data"]]

data_stacked <- stackPlantDiv(
  div_1m2Data = data_1m2,
  div_10m2Data100m2Data = data_10_100m2)


data_stacked %>% names()
data_stacked$subplotID %>% unique()

plot_32.4.10 <- data_stacked %>% filter(subplotID == "32.4.10")
plot_32.4.10$taxonID %>% length()
plot_32.4.10$eventID %>% unique()
plot_32.4.10$taxonID %>% unique() %>% length()
