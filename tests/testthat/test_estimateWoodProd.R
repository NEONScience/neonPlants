### Function tests for estimateWoodProd ####
#   Courtney Meier; cmeier@BattelleEcology.org



### Read in test data ####
vstTestDF <- readRDS(testthat::test_path("testdata", "vst_testDat.rds"))

# Temporary: Reduce to one site for function dev purposes
theSite <- "ABBY"

map <- vstTestDF$vst_mappingandtagging %>%
  dplyr::filter(siteID == theSite)

appInd <- vstTestDF$vst_apparentindividual %>%
  dplyr::filter(siteID == theSite)

perPlot <- vstTestDF$vst_perplotperyear %>%
  dplyr::filter(siteID == theSite)

nonWoody <- vstTestDF$`vst_non-woody` %>%
  dplyr::filter(siteID == theSite)

#   Temporary: Further reduce to 2016-2019 to test handling of plots with zero tree biomass when there are no years with trees in the dataset for specific plots
#--> expect these plots to show up as zeroes
theEvents <- c("vst_ABBY_2016", "vst_ABBY_2017", "vst_ABBY_2018", "vst_ABBY_2019")

appInd <- appInd %>%
  dplyr::filter(eventID %in% theEvents)

perPlot <- perPlot %>%
  dplyr::filter(eventID %in% theEvents)

nonWoody <- nonWoody %>%
  dplyr::filter(eventID %in% theEvents)






estimateWoodProdOutputs <- estimateWoodProd(inputDataList = VstDat,
                                            plotSubset = "towerAnnualSubset")





