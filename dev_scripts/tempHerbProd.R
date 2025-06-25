hbpTest <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                        startdate = "2021-09",
                                        enddate = "2024-10",
                                        check.size = FALSE,
                                        token = Sys.getenv("NEON_TOKEN"))

scaleHerbMassOutput <- neonPlants::scaleHerbMass(hbpTest,
                                                 plotSubset = "all")


herbScale <- neonPlants::scaleHerbMass(HbpDat,
                                       plotSubset = "all")


herbProd <- neonPlants::estimateHerbProd(hbpTest,
                                         plotSubset = "all")

#--> unexpected herbGroups in output for 2022 DCFS and 2023 KONZ when plotSubset = "all"
hbp_agb <- scaleHerbMassOutput$hbp_agb

hbp_agb <- hbp_agb %>%
  dplyr::filter(siteID %in% c("DCFS", "KONZ", "SJER"),
                collectDate >= "2022-01-01" & collectDate <= "2023-12-31")

hbp_agb <- test$hbp_agb
