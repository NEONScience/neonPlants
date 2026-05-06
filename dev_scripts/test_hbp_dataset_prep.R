### neonPlants test dataset preparation ###################################
library(neonUtilities)
library(tidyverse)


### HBP test dataset prep ####
#   Test sites: Site list includes Ag (BLAN), grazed (NOGP, SJER), partially grazed (CLBJ, KONZ), partially grazed with multiple bouts for ungrazed plots (CLBJ), ungrazed for some years at site with two clips (OAES), and "standard" sites with and without Distributed plot sampling. SRER is included because spring and summer bouts are summed at SRER due to non-overlapping plant communities
inputDataList <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                              site = c("BLAN", "CLBJ", "KONZ", "NOGP", "OAES", "SJER", "SRER", "TEAK", "TREE"),
                                              startdate = "2018-01",
                                              enddate = "2019-12",
                                              check.size = FALSE,
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
