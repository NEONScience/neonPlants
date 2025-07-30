### Function testing: Odd TREE output from 2018 with eventIDs
inputDataList <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                              site = c("BLAN", "KONZ", "SJER", "SRER", "TEAK", "TREE"),
                                              startdate = "2018-01",
                                              enddate = "2019-12",
                                              check.size = FALSE,
                                              token = Sys.getenv("NEON_TOKEN"))

#--> Appears that scaleHerbMass does not handle plots with targetTaxaPresent == "N" properly

#   Identify all sites where grazing exclosures have been deployed
hbpAll <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                       site = "all",
                                       enddate = "2019-12",
                                       tabl = "hbp_perbout",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

hbpAllBout <- hbpAll$hbp_perbout

hbpAllGrazed <- hbpAllBout %>%
  dplyr::filter(exclosure == "Y") %>%
  dplyr::group_by(domainID,
                  siteID) %>%
  dplyr::summarise(count = n())




### Function testing: Odd DCFS and KONZ output
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


### Removed from scaleHerbMass function as obsolete
# #   Aggregate dryMass across herbGroups in peak biomass bouts
# hbp_peak_biomass_herb_groups <- hbp %>%
#   dplyr::filter(.data$herbGroup != "AllHerbaceousPlants")
#
# hbp_peak_biomass_sum_groups <- hbp_peak_biomass_herb_groups %>%
#   dplyr::group_by(.data$sampleID) %>%
#   dplyr::summarise(dryMassSum = sum(.data$dryMass_gm2))
#
# #   Populate "AllHerbaceousPlants" column for peak biomass bouts
# hbp2 <- merge(hbp_wide,
#               hbp_peak_biomass_sum_groups,
#               by = "sampleID",
#               all.x = TRUE)
#
# hbp2$AllHerbaceousPlants_gm2 <- ifelse(is.na(hbp2$AllHerbaceousPlants_gm2),
#                                        hbp2$dryMassSum,
#                                        hbp2$AllHerbaceousPlants_gm2)

# #   Separate "eventID" into components, relocate and remove columns, set "year" data type, arrange
# hbp_standing_biomass_in_clip_cells <- hbp2 %>%
#   dplyr::select(-"dryMassSum") %>%
#   tidyr::separate(col = "eventID",
#                   into = c("data_prod", "year", "siteID2", "bout"),
#                   sep = "\\.",
#                   remove = FALSE,
#                   extra = "drop") %>%
#   dplyr::relocate("sampleID",
#                   .after = "peak") %>%
#   dplyr::relocate("year", .before = "collectDate") %>%
#   dplyr::relocate("AllHerbaceousPlants_gm2", .after = "sampleID") %>%
#   dplyr::select(-"siteID2",
#                 -"bout") %>%
#   dplyr::mutate(year = as.numeric(.data$year)) %>%
#   dplyr::arrange(.data$domainID,
#                  .data$siteID,
#                  .data$year,
#                  .data$plotID,
#                  .data$clipID)



### Removed from estimateHerbProd function as obsolete
# ### Error: Stop if not at least 2 years of data ####
# #   Define 'start' and 'end' years for productivity interval
# start <- min(hbp_plot$year)
# end  <- max(hbp_plot$year)
#
# #   Check for valid interval
# if (as.numeric(end) - as.numeric(start) < 1) {
#
#   stop(glue::glue("At least 2 years of data are needed to calculate productivity (more when the plot sampling interval is longer than annual). Input dataset only has biomass data from: {unique(hbp_plot$year)}"))
#
# }
