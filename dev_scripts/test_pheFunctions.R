library(neonUtilities)
library(tidyverse)


pheDat <- loadByProduct(
  dpID = "DP1.10055.001",
  site = "UKFS",
  startdate = "2022-01",
  enddate = "2022-12",
  package = "basic",
  check.size = FALSE)

transitions <- estimatePheTransByTag(inputDataList = pheDat)
transitions2 <- estimatePheTransByTag(inputStatus = pheDat$phe_statusintensity, 
                                      inputTags = pheDat$phe_perindividual)


phaseDuration <- estimatePheDurationByTag(
  inputDataList = pheDat)

phaseDuration2 <- estimatePheDurationByTag(
  inputStatus = pheDat$phe_statusintensity,
  inputTags = pheDat$phe_perindividual)
