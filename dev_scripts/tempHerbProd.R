hbpTest <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                        site = c("DCFS", "KONZ", "SJER"),
                                        startdate = "2021-09",
                                        enddate = "2024-10",
                                        check.size = FALSE,
                                        token = Sys.getenv("NEON_TOKEN"))

scaleHerbMassOutput <- neonPlants::scaleHerbMass(hbpTest,
                                                 plotSubset = "all")


scaleHerbMassOutput <- neonPlants::scaleHerbMass(HbpDat,
                                                 plotSubset = "all")


herbProd <- neonPlants::estimateHerbProd(hbpTest,
                                         plotSubset = "all")

#--> unexpected herbGroups in output for 2022 DCFS and 2023 KONZ when plotSubset = "all"
hbp_agb <- scaleHerbMassOutput$hbp_agb

hbp_agb <- hbp_agb %>%
  dplyr::filter(siteID %in% c("DCFS", "KONZ", "SJER"),
                collectDate >= "2022-01-01" & collectDate <= "2023-12-31")

hbp_agb <- test$hbp_agb
plotSubset <- "all"


#   DCFS sampling effort
dcfsBoutEffort <- hbp_agb_plot %>%
  dplyr::filter(siteID == "DCFS") %>%
  dplyr::group_by(siteID,
                  year,
                  plotType,
                  eventID) %>%
  dplyr::summarise(plotCount = length(unique(plotID)),
                   eventID = unique(eventID),
                   startDate = min(collectDate),
                   endDate = max(collectDate))


### Combining SD from groups with unequal sample size --> needed for consumption estimate SD

#   Quadrature - use 'eventConsume' data frame
temp1 <- eventConsum %>%
  dplyr::filter(if_all("eventPlotCount_exclosureN":"agbSD_gm2_exclosureY", ~ !is.na(.))) %>%
  dplyr::mutate(consum_gm2 = agbMean_gm2_exclosureY - agbMean_gm2_exclosureN,
                consumSDQuad = round(sqrt(agbSD_gm2_exclosureN^2 + agbSD_gm2_exclosureY^2),
                                     digits = 1),
                consumSDN = round(sqrt((agbSD_gm2_exclosureN^2 / eventPlotCount_exclosureN) +
                                   (agbSD_gm2_exclosureY^2 / eventPlotCount_exclosureY)),
                                  digits = 1)
                )


#--> https://www.mathbench.umd.edu/modules/statistical-tests_t-tests/page06.htm for unequal sample size approach (also what Duck AI returned)





