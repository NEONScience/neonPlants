### Function testing: Odd TREE output from 2018 with eventIDs
#   Test sites: Site list includes Ag (BLAN), grazed (CPER, SJER), partially grazed (CLBJ, KONZ), and "standard" sites with and without Distributed plot sampling.
inputDataList <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                              site = c("BLAN", "CLBJ", "CPER", "KONZ", "NOGP", "OSBS", "SJER", "SRER", "TEAK", "TREE"),
                                              startdate = "2018-01",
                                              enddate = "2019-12",
                                              check.size = FALSE,
                                              token = Sys.getenv("NEON_TOKEN"))

#   Test data: Single "standard" site to test if code handles pared down data
inputDataList <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                              site = "ONAQ",
                                              startdate = "2023-01",
                                              enddate = "2025-12",
                                              check.size = FALSE,
                                              include.provisional = TRUE,
                                              token = Sys.getenv("NEON_TOKEN"))

inputBout <- inputDataList$hbp_perbout
inputMass <- inputDataList$hbp_massdata





### Identify all sites where grazing exclosures have been deployed; need to identify sites where
### exclosures were deployed and trialed but were not successful (e.g., SRER)
hbpAll <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                       site = "all",
                                       enddate = "2024-12",
                                       tabl = "hbp_perbout",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

hbpAllBout <- hbpAll$hbp_perbout

#   Add 'year' column to aid with summarizing exclosure presence/absence
hbpAllBout <- hbpAllBout %>%
  dplyr::mutate(year = lubridate::year(.data$collectDate),
                .after = "collectDate")

#   Summarize exclosure history by siteID and year; remove NA 'exclosure' for now
grazeSummary <- hbpAllBout %>%
  dplyr::filter(!is.na(exclosure),
                plotType == "tower") %>%
  dplyr::group_by(domainID,
                  siteID,
                  year,
                  exclosure) %>%
  dplyr::summarise(count = n(),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = "exclosure",
                     values_from = "count")

#-->  SERC, JERC, OSBS, TREE, UKFS, JORN, SRER, TEAK all should be hardcoded to have
#--> exclosure=Y filtered out

#   More detailed look at both SRER and TEAK to determine which bouts should be discarded
grazeDetails <- hbpAllBout %>%
  dplyr::filter(!is.na(exclosure),
                plotType == "tower",
                siteID %in% c("SRER", "TEAK")) %>%
  dplyr::group_by(domainID,
                  siteID,
                  year,
                  eventID,
                  exclosure) %>%
  dplyr::summarise(count = n(),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = "exclosure",
                     values_from = "count")

#--> At TEAK, a handful of bouts where only one plot was sampled when exclosure == "Y"; remove all data from these bouts

teakRemove <- grazeDetails %>%
  dplyr::filter(siteID == "TEAK",
                N < 3)

teakRemove <- c("HBP.2019.TEAK.02.TOWER", "HBP.2019.TEAK.03.TOWER", "HBP.2019.TEAK.04.TOWER", "HBP.2021.TEAK.23.TOWER", "HBP.2021.TEAK.27.TOWER", "HBP.2021.TEAK.35.TOWER", "HBP.2021.TEAK.43.TOWER")




### Example: Retain all rows in a group when a row within the group matches a filter criterion;
### useful to identify plots that are grazed but that intermittently have exclosures (due to
### damage) or to identify plots planted with crops for part of the year.
result <- df %>%
  group_by(group) %>%
  filter(any(value > 4))






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


##  Identify peak biomass bouts at grazed sites --> re-insert a version of this into plot-level section?
#   Define list of sites with grazing management where exclosures are deployed; list developed by counting 'perbout' records with exclosure == "Y" across sites. Sites that trialed exclosures for a while are not included (e.g., SRER, TEAK)
grazedSites <- c("LAJA", "KONZ", "DCFS", "NOGP", "WOOD", "CPER", "CLBJ", "OAES", "MOAB", "SJER")

#   Remove records at sites not in 'grazedSites' with exclosure == "Y"; these sites never produced high-quality consumption data.
hbp <- hbp %>%
  dplyr::filter(.data$siteID %in% grazedSites |
                  (!.data$siteID %in% grazedSites & (.data$exclosure == "N" | is.na(.data$exclosure))))

#   Assign bouts at grazed sites to "atPeak" and "offPeak" by counting unique values of growthForm observed for the bout
peakEvents <- hbp %>%
  dplyr::filter(.data$siteID %in% grazedSites,
                !is.na(.data$herbGroup)) %>%
  dplyr::group_by(.data$domainID,
                  .data$siteID,
                  .data$eventID) %>%
  dplyr::summarise(countHerbGroup = length(unique(.data$herbGroup)),
                   .groups = "drop") %>%
  dplyr::filter(.data$countHerbGroup > 1)

hbp <- hbp %>%
  dplyr::mutate(peak = dplyr::case_when(.data$siteID %in% grazedSites & .data$eventID %in% peakEvents$eventID ~ "atPeak",
                                        .data$siteID %in% grazedSites & !.data$eventID %in% peakEvents$eventID ~ "offPeak",
                                        TRUE ~ "atPeak"))
