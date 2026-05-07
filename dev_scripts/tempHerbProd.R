### Function dev and testing ####

#   Replace first letter of selected columns with lower-case
dplyr::rename_with(~ stringr::str_replace(.x, "^(.)", ~ tolower(.x)),
                   .cols = "CoolSeasonGram_gm2":"Wheat_gm2")


### Check function output for 2025 data from all sites
tempHBP <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                        site = "all",
                                        startdate = "2025-01",
                                        enddate = "2025-12",
                                        check.size = FALSE,
                                        include.provisional = TRUE,
                                        token = Sys.getenv("NEON_TOKEN"))

outputDF <- neonPlants::estimateHerbProd(inputDataList = tempHBP)

plotProd <- outputDF$herb_ANPP_plot
siteProd <- outputDF$herb_ANPP_site
grazeExtra <- outputDF$herb_ANPP_grazed_extra
consume <- outputDF$herb_grazed_consumption

#   Problems & oddities:
#--> plotProd: Fallow plots at STER coming back with herbProd = NA even though herbGroups have mass
#--> siteProd: LAJA, WOOD, CPER, OAES, MOAB, SJER all have NAs in all columns (basically most of the grazed sites, but not all - e.g., DCFS, NOGP do have data)
#--> consume: HBP.2025.NOGP.24.TOWER has zeroes for exclN and exclY biomass columns, strange, need to investigate...
#--> consume: HBP.2025.SJER.05.TOWER only has one exclN record --> maybe a weekBoutBegan eventID error in input data?
#--> grazeExtra: Sites with NA for 'herbProd_gm2yr' all have no ungrazed plots --> need logic to deal with all plots being grazed.



### Retrieve all HBP data for investigative purposes
#--> Identify all sites where exclosures were deployed and trialed but were not successful (e.g., SRER, TEAK)
#--> Identify sites with no exclosures and multiple bouts of Tower plot sampling

hbpAll <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                       site = "all",
                                       enddate = "2025-12",
                                       tabl = "hbp_perbout",
                                       check.size = FALSE,
                                       include.provisional = TRUE,
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



### Identify sites with no exclosures and multiple bouts of Tower plot sampling
towerSummary <- hbpAllBout %>%
  dplyr::filter(plotType == "tower") %>%
  dplyr::group_by(domainID,
                  siteID,
                  year) %>%
  dplyr::filter(!any(exclosure == "Y")) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(domainID,
                  siteID,
                  year,
                  eventID) %>%
  dplyr::summarise(count = n())

towerSummary2 <- hbpAllBout %>%
  dplyr::filter(plotType == "tower") %>%
  dplyr::group_by(domainID,
                  siteID,
                  year) %>%
  dplyr::filter(!any(exclosure == "Y")) %>%
  dplyr::summarise(eventCount = length(unique(eventID)))

#--> OAES in 2017, 2018 has two bouts (one June, one October)
#--> A number of eventID errors that create perception of > 1 Tower plot bout

distSummary <- hbpAllBout %>%
  dplyr::filter(plotType == "distributed") %>%
  dplyr::group_by(domainID,
                  siteID,
                  year) %>%
  dplyr::summarise(eventCount = length(unique(eventID)))

temp <- hbpAllBout %>%
  dplyr::filter(plotType == "distributed",
                siteID == "BONA",
                year == 2024) %>%
  dplyr::group_by(domainID,
                  siteID,
                  year) %>%
  dplyr::summarise(eventCount = length(unique(eventID)),
                   events = paste(unique(eventID), collapse = ", "))



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


### Removed from scaleHerbMass function as obsolete ####
### Grazed sites: Estimate consumption to add to finalStandingMass ####

### Calculate consumption productivity component using exclosure data
#   Calculate mean exclosure == "Y" and exclosure == "N" mass across clip samples by eventID
exclosure <- hbp_agb_long %>%
  dplyr::filter(.data$herbGroup == "AllHerbaceousPlants",
                .data$siteID %in% grazed_sites,
                .data$plotType == "tower",
                !is.na(.data$agb_gm2),
                .data$plotYear %in% grazedPlotYears) %>%
  dplyr::group_by(.data$domainID,
                  .data$siteID,
                  .data$year,
                  .data$eventID,
                  .data$peak,
                  .data$herbGroup,
                  .data$exclosure) %>%
  dplyr::summarise("clipCount" = dplyr::n(),
                   "agbMean_gm2" = round(mean(.data$agb_gm2, na.rm = TRUE),
                                         digits = 2),
                   "agbSD_gm2" = round(stats::sd(.data$agb_gm2, na.rm = TRUE),
                                       digits = 2),
                   .groups = "drop") %>%
  dplyr::arrange(.data$domainID,
                 .data$siteID,
                 .data$year,
                 .data$eventID)

#   Calculate consumption mean and SD per eventID
#--> Uncertainties combined according to: https://www.mathbench.umd.edu/modules/statistical-tests_t-tests/page06.htm

eventConsum <- exclosure %>%
  tidyr::pivot_wider(names_from = "exclosure",
                     values_from = c("clipCount" , "agbMean_gm2", "agbSD_gm2"),
                     names_prefix = "excl") %>%
  dplyr::mutate("consumClipCount" = .data$clipCount_exclN + .data$clipCount_exclY,
                "consumMean_gm2" = round(.data$agbMean_gm2_exclY - .data$agbMean_gm2_exclN,
                                         digits = 2),
                "consumSD_gm2" = round(sqrt((.data$agbSD_gm2_exclN^2 / .data$clipCount_exclN) +
                                              (.data$agbSD_gm2_exclY^2 / .data$clipCount_exclY)),
                                       digits = 2),
                "consumSD2_N" = round(.data$consumSD_gm2^2 / .data$consumClipCount,
                                      digits = 2))


##  Sum consumption for all events per site and year
siteConsum <- eventConsum %>%
  dplyr::filter(!is.na(.data$consumMean_gm2)) %>%
  dplyr::group_by(.data$domainID,
                  .data$siteID,
                  .data$year,
                  .data$herbGroup) %>%
  dplyr::summarise("consumEventCount" = dplyr::n(),
                   "consumClipCount" = sum(.data$consumClipCount),
                   "consumption_gm2" = round(sum(.data$consumMean_gm2, na.rm = TRUE),
                                             digits = 2),
                   "consumptionSD_gm2" = round(sqrt(sum(.data$consumSD2_N, na.rm = TRUE)),
                                               digits = 2),
                   .groups = "drop")


##  Join with 'grazedFinalMass' to calculate herbaceous ANPP at grazed sites
herb_ANPP_grazed <- dplyr::left_join(grazedFinalMass,
                                     siteConsum,
                                     by = c("domainID", "siteID", "year", "herbGroup")) %>%
  dplyr::mutate(herbClipCount = .data$finalClipCount + .data$consumClipCount,
                herbANPP_gm2yr = rowSums(dplyr::across(c("finalAGBMean_gm2", "consumption_gm2")),
                                         na.rm = TRUE),
                herbANPPSD_gm2yr = round(sqrt((.data$finalAGBSD_gm2^2 / .data$finalClipCount) +
                                                (.data$consumptionSD_gm2^2 / .data$consumClipCount)),
                                         digits = 2),
                .after = "herbGroup")


### Standard sites: Calculate site-level productivity and SD by 'year' and 'herbGroup' ####

#   Filter to "standard" plot data: Sites with no grazing, Distributed plots at grazed sites, Tower plots at grazed sites but that are not grazed. Also remove rows for herbGroups with "NA" mass (mostly crops).
standardFinalMass <- hbp_agb_long %>%
  dplyr::filter(!.data$siteID %in% grazed_sites | (.data$siteID %in% grazed_sites & .data$plotType == "distributed") |
                  (.data$siteID %in% grazed_sites & .data$plotType == "tower" & !.data$plotYear %in% grazedPlotYears),
                !is.na(.data$agb_gm2))

#   Identify latest eventID for each 'site' x 'year' x 'plotType' combination
standardLatestEvents <- standardFinalMass %>%
  dplyr::distinct(.data$domainID,
                  .data$siteID,
                  .data$year,
                  .data$plotType,
                  .data$eventID) %>%
  dplyr::group_by(.data$domainID,
                  .data$siteID,
                  .data$year,
                  .data$plotType) %>%
  dplyr::arrange(.data$eventID) %>%
  dplyr::slice_tail()

#   Further filter to latest "standard" eventID --> should be redundant with filtering above unless Tower plot was clipped more than once and never had an exclosure in it for entire 'plot' x 'year' combination
test <- standardFinalMass %>%
  dplyr::filter(!.data$eventID %in% standardLatestEvents$eventID)

standardFinalMass <- standardFinalMass %>%
  dplyr::filter(.data$eventID %in% standardLatestEvents$eventID)
#--> This doesn't quite work right for plots that don't have exclosure but are sampled for all the grazed bouts anyway (happened at SJER in 2017)...


#   Create site-level ANPP estimates for "standard" sites/plots
if (nrow(standardFinalMass) > 0) {

  herb_ANPP_site <- standardFinalMass %>%
    dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$plotType,
                    .data$eventID,
                    .data$herbGroup) %>%
    dplyr::summarise(herbClipCount = dplyr::n(),
                     herbANPP_gm2yr = round(mean(.data$agb_gm2, na.rm = TRUE),
                                            digits = 2),
                     herbANPPSD_gm2yr = round(stats::sd(.data$agb_gm2, na.rm = TRUE),
                                              digits = 2))

} else {

  herb_ANPP_site <- data.frame()

}

#   Create plot-level output for herbGroup = "AllHerbaceousPlants"
herb_ANPP_plot <- standardFinalMass %>%
  dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
  dplyr::rename("herbANPP_gm2yr" = "agb_gm2") %>%
  dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                        digits = 2),
                .after = "exclosure")

herb_ANPP_site <- dplyr::bind_rows(herb_ANPP_site,
                                   herb_ANPP_grazed) %>%
  dplyr::relocate("finalEventID",
                  .after = "eventID")

#   Add columns with "Mg/ha/y" units for ANPP and SD and arrange
herb_ANPP_site <- herb_ANPP_site %>%
  dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                        digits = 2),
                herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,
                                          digits = 2),
                .after = "herbClipCount") %>%
  dplyr::arrange(.data$domainID,
                 .data$siteID,
                 .data$year,
                 .data$plotType)

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

# #   Create long-format table from input 'hbp_agb' data frame
# hbp_agb_long <- hbp_agb %>%
#   dplyr::rename_with(~ stringr::str_remove(., "_gm2"),
#                      c("AllHerbaceousPlants_gm2",
#                        "CoolSeasonGraminoids_gm2",
#                        "AnnualAndPerennialForbs_gm2",
#                        "NFixingPlants_gm2",
#                        "WarmSeasonGraminoids_gm2",
#                        "WoodyStemmedPlants_gm2",
#                        "Corn_gm2",
#                        "Barley_gm2",
#                        "OrchardGrass_gm2",
#                        "Soybean_gm2",
#                        "Sorghum_gm2",
#                        "Wheat_gm2",
#                        "Millet_gm2")) %>%
#   tidyr::pivot_longer(cols = c("AllHerbaceousPlants",
#                                "CoolSeasonGraminoids",
#                                "WoodyStemmedPlants",
#                                "WarmSeasonGraminoids",
#                                "NFixingPlants",
#                                "AnnualAndPerennialForbs",
#                                "Corn",
#                                "Barley",
#                                "OrchardGrass",
#                                "Soybean",
#                                "Sorghum",
#                                "Wheat",
#                                "Millet"),
#                       names_to = "herbGroup",
#                       values_to = "agb_gm2")
#
# #   Populate exclosure == "N" if value is NA
# #--> Skipping this for now, not sure if needed?
# # hbp_agb_long$exclosure <- dplyr::if_else(is.na(hbp_agb_long$exclosure),
# #                                          "N", hbp_agb_long$exclosure,
# #                                          hbp_agb_long$exclosure)
#
# #   Remove duplicates based on primary keys; cannot use 'sampleID' because it is NA for records with targetTaxaPresent == "N"
# hbp_agb_long <- hbp_agb_long[!duplicated(paste0(hbp_agb_long$clipID,
#                                                 hbp_agb_long$eventID,
#                                                 hbp_agb_long$herbGroup),
#                                          fromLast = TRUE), ]
#
#
# ##  Prepare 'hbp_agb_long' data frame for downstream processing
# #   Remove unneeded columns from 'hbp_agb_long' and keep 'ambient' and 'exclosure' data at native spatial resolution;
# #   i.e., assume subplots within large-stature Tower plots are independent.
# hbp_agb_long <- hbp_agb_long %>%
#   dplyr::select("domainID",
#                 "siteID",
#                 "plotID",
#                 "subplotID",
#                 "sampleID",
#                 "nlcdClass",
#                 "plotType",
#                 "plotManagement",
#                 "collectDate",
#                 "year",
#                 "eventID",
#                 "targetTaxaPresent",
#                 "exclosure",
#                 "peak",
#                 "herbGroup",
#                 "agb_gm2") %>%
#
#   #   Create 'plot-year' variable for subsequent ID of Tower plots likely managed for grazing
#   dplyr::mutate(plotYear = paste(.data$plotID, .data$year, sep = "-"),
#                 .before = "plotID")
#
#
# # ##  Identify grazed sites in the dataset using exclosure == Y
# # grazed_sites <- hbp_agb %>%
# #   dplyr::select("siteID",
# #                 "exclosure") %>%
# #   dplyr::filter(.data$exclosure == "Y")
# #
# # grazed_sites <- sort(unique(as.character(grazed_sites$siteID)))
# #--> Likely not helpful to identify grazed sites, since grazing may not occur at all Tower plots within a site.
#
#
#
# ### Standard sites: Determine latest standing ambient biomass within a 'year' for each herbGroup ####
# #-->  This is productivity for sites/plots with no grazing exclosures.
#
# ### Obtain final standing mass for sites with no grazing management; also bring in Distributed plots at grazed sites and Tower plots at grazed sites that are not actively managed for grazing (i.e., only a portion of the Tower plots have cows and exclosures). For plots NOT managed for grazing (i.e., no exclosure == "Y" records) but that WERE clipped every grazed bout, cannot identify peak biomass as latest eventID --> use peak == "atPeak" instead.
#
# ##  For each plot-year combination, determine whether an exclosure was deployed in that year; if "Y", the plot-year is assumed to be under grazing management. Plots managed for grazing in a given year do not contribute to "standard" site-level productivity estimates and instead are put through the "consumption" workflow.
#
# #   Create list of unique Tower 'plot-year' combinations
# towerPlotYears <- sort(unique(hbp_agb_long$plotYear[hbp_agb_long$plotType == "tower"]))
#
# #   Identify grazed Tower 'plot-year' combinations
# if (length(towerPlotYears) > 0) {
#
#   grazedPlotYears <- c()
#
#   for (i in 1:length(towerPlotYears)) {
#
#     tempDF <- hbp_agb_long %>%
#       dplyr::filter(.data$plotYear == towerPlotYears[i])
#
#     if ("Y" %in% tempDF$exclosure) {grazedPlotYears <- c(grazedPlotYears, towerPlotYears[i])}
#
#   }
#
#   rm(tempDF, i)
#
# } else {
#
#   grazedPlotYears <- c()
#
# } # End length(towerPlotYears) conditional


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
