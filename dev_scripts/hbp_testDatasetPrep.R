### neonPlants test dataset preparation ###################################
library(neonUtilities)
library(tidyverse)


### HBP test dataset prep ####
#   Test dataset details:
#--> BLAN 2018, 2019: Agricultural site with a mix of crop types in subsets of both Distributed and Tower plots
#--> TREE 2018, 2019: A "standard" forested site some qualifying Distributed plots and Tower plots clipped 1X per year
#--> KONZ 2018, 2019: Mix of Tower plots that are grazed and ungrazed; included to ensure fixed eventIDs work properly
#--> NOGP 2018, 2019: Mix of Tower plots that are grazed and ungrazed
#--> CLBJ 2018, 2019: Mix of Tower plots that are grazed and ungrazed, with multiple bouts for ungrazed
#--> OAES 2018, 2019: Year 2018 site was ungrazed with multiple Tower plot clips, both Distributed and Tower plots sampled; 2019 a subset of Tower plots grazed and no Distributed plots.
#--> SRER 2018, 2019: Year 2018 multiple Tower plot clips, with productivity summed across bouts; 2019 year with multiple Tower plot clips and Distributed plot single clip; also checked to ensure older exclosure = "Y" records excluded from analysis.
#--> SJER 2018, 2019: Grazed site with subset of Tower plots not grazed, included to check parsing of months into Mediterranean growing season
#--> TEAK 2019: Included to ensure older exclosure = "Y" records excluded from analysis.

inputDataList <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                              site = c("BLAN", "CLBJ", "KONZ", "NOGP", "OAES", "SJER", "SRER", "TEAK", "TREE"),
                                              startdate = "2018-01",
                                              enddate = "2019-12",
                                              check.size = FALSE,
                                              release = "LATEST",
                                              include.provisional = TRUE,
                                              token = Sys.getenv("NEON_TOKEN"))

#   Extract tables to make KONZ updates, only needed in perBout
perBout <- inputDataList$hbp_perbout
massData <- inputDataList$hbp_massdata

#   Fix KONZ eventID problems
perBout <- perBout %>%
  dplyr::mutate(setDate = as.Date(setDate),
                collectDate = as.Date(collectDate)) %>%
  dplyr::mutate(eventID = dplyr::case_when(
    eventID == "HBP.2019.KONZ.01.TOWER" & collectDate == "2019-08-12" ~ "HBP.2019.KONZ.02.TOWER",
    eventID == "HBP.2019.KONZ.02.TOWER" & collectDate == "2019-10-07" ~ "HBP.2019.KONZ.03.TOWER",
    eventID == "HBP.2018.KONZ.05.TOWER" & collectDate == "2018-10-08" ~ "HBP.2018.KONZ.06.TOWER",
    eventID == "HBP.2018.KONZ.04.TOWER" & collectDate == "2018-10-08" ~ "HBP.2018.KONZ.06.TOWER",
    eventID == "HBP.2018.KONZ.04.TOWER" & collectDate == "2018-09-10" ~ "HBP.2018.KONZ.05.TOWER",
    eventID == "HBP.2018.KONZ.01.TOWER" & (collectDate == "2018-08-13" | collectDate == "2018-08-14") ~ "HBP.2018.KONZ.04.TOWER",
    TRUE ~ eventID))

#   Create new HBP test dataset
hbp_testDat <- list(hbp_perbout = perBout,
                    hbp_massdata = massData)

saveRDS(hbp_testDat,
        file = "tests/testthat/testdata/hbp_testDat.RDS")
