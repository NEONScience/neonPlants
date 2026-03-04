#' @title Estimate herbaceous ANPP (Above-ground Net Primary Productivity) at NEON sites
#'
#' @author
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Estimate above-ground herbaceous productivity at NEON sites using Herbaceous Clip Harvest data tables (DP1.10023.001).
#'
#' @details Input data should be a list of herbaceous biomass (DP1.10023.001) data frames downloaded via the neonUtilities::loadByProduct() function. The companion scaleHerbMass() function is called internally, and outputs from this function are used to estimate herbaceous productivity.
#'
#' At sites where herbaceous plants are not subject to grazing management and exclosures are not in use, and crops are not planted at any point during the year, the estimate of herbaceous ANPP is simply the standing biomass clipped from the bout with the greatest biomass.
#'
#' Where grazing management occurs in Tower plots and exclosures have been established, consumption is calculated for each Tower plot sampling event as average biomass within exclosures minus average biomass outside of exclosures. Consumption at each sampling bout is summed and then added to the standing biomass of the last bout of the season for an estimate of herbaceous ANPP.
#'
#' At sites where crops are planted, productivity is determined on a plot-by-plot basis to account for the potential for multiple plantings throughout the season, as well as "wild-type" plots that are either fallow for the duration of the season or are not managed for agriculture.
#'
#' Note that at grazed sites, the Science Design only supports calculating consumption at the site level. The clip harvests within exclosures are not close enough to ambient clip harvests to support plot-level consumption estimates.
#'
#' @param inputDataList An R list object produced by the neonUtilities::loadByProduct() function for the NEON Herbaceous Clip Harvest data product. [list]
#'
#' @param plotSubset The options are the default "all" (all Tower and Distributed plots), "tower" (all plots in the Tower airshed but no Distributed plots), and "distributed" (all Distributed plots, which are sampled on a 5-year interval and are spatially representative of the NLCD classes at a site, and no Tower plots). [character]
#'
#' @return A list that includes productivity summary data frames. Output tables include:
#'   * herb_ANPP_site - Summarizes herbaceous ANPP for each site x year combination ("Mg/ha/yr" and "g/m2/yr").
#'   * herb_ANPP_plot - Summarizes herbaceous ANPP for the sum of all herbaceous plants for each plot x year combination ("Mg/ha/yr" and "g/m2/yr"). Plot-level summaries are not returned for grazed Tower plots, for the reason outlined in details.
#'   * herb_ANPP_plot_herbgroup - Summarizes herbaceous ANPP by herbGroup ("Cool Season Graminoids", "N-fixing Plants", etc.) for each plot x year combination ("Mg/ha/yr" and "g/m2/yr"). Plot-level summaries are not returned for grazed Tower plots.
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Herbaceous clip harvest data
#' HbpDat <- neonUtilities::loadByProduct(
#' dpID = "DP1.10023.001",
#' package = "basic",
#' check.size = FALSE
#' )
#'
#' # Example with arguments at default values
#' df <-estimateHerbProd(inputDataList = HbpDat)
#'
#' # Example specifying an alternative plotSubset value
#' df <-estimateHerbProd(inputDataList = HbpDat,
#' plotSubset = "tower")
#'
#' }
#'
#' @export estimateHerbProd

estimateHerbProd = function(inputDataList,
                            plotSubset = "all") {

  options(dplyr.summarise.inform = FALSE)



  ### Check inputDataList properties ####
  #   Check that 'inputDataList' is an object with class 'list'
  if(!methods::is(inputDataList, class = "list" )){
    stop("The inputDataList argument is expected to be a list generated with neonUtilities::loadByProduct(). A character, data.frame, or NA argument is not allowed.")
  }



  ### Check plotSubset properties
  if (!plotSubset %in% c("all", "tower", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'tower', 'distributed'.")
  }



  ### Generate scaleHerbMass outputs and unlist
  scaleHerbMassOutput <- neonPlants::scaleHerbMass(inputDataList = inputDataList,
                                                   plotSubset = plotSubset)

  hbp_agb <- scaleHerbMassOutput$hbp_agb






  ### Verify scaleHerbMassOutputs tables contain required columns and data ####

  ### Verify scaleHerbMass output table 'hbp_agb' contains required data
  #   Check for data
  if (!nrow(hbp_agb)) {
    stop(glue::glue("Table from scaleHerbMass() output 'hbp_agb' has no data."))
  }




  #--> start again here after 'scaleHerbMass' is updated to report '0' instead of NA for absent herbGroups ###############

  ### Prepare data for downstream calculations ####

  #   Filter input data using user-supplied 'plotSubset' argument; insurance step since scaleHerbMass() output should already only be produced for user-selected option
  if (plotSubset %in% c("distributed", "tower")) {

    hbp_agb <- hbp_agb %>%
      dplyr::filter(.data$plotType == plotSubset)

  }

  #   Create 'site x year' variable for identifying all records at sites where a subset of plots are cropped
  hbp_agb <- hbp_agb %>%
    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"),
                  .before = "plotID")



  ### Separate records into grazed, cropped, and standard 'siteID x year' data frames; each requires custom logic to calculate ANPP at the site level

  ##  Step 1: Identify records in all 'siteID x year' combinations that supported grazing
  grazedSiteYearDF <- hbp_agb %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$exclosure == "Y")) %>%
    dplyr::ungroup()


  ##  Step 2: Identify records for all 'siteID x year' combinations that contained crops in a plot at any point
  #   Isolate records with mass for a crop of any kind
  temp <- hbp_agb %>%
    dplyr::filter(rowSums(is.na(select(., "Barley_gm2":"Wheat_gm2"))) < 10)

  #   Filter to all records from crop 'site x year' combos
  cropSiteYearDF <- hbp_agb %>%
    dplyr::filter(.data$siteYear %in% temp$siteYear)

  rm(temp)


  ##  Step 3: Identify all "standard" clips - i.e., no grazing, no crops at any point in a 'siteID x year'
  stdSiteYearDF <- hbp_agb %>%
    dplyr::filter(!.data$siteYear %in% grazedSiteYearDF$siteYear,
                  !.data$siteYear %in% cropSiteYearDF$siteYear)






  ### Standard sites: Calculate plot- and site-level ANPP for 'site x year' combos with no grazing or crops ####

  ##  First: Quantify number of Tower plot eventIDs within each 'site x year'
  temp <- stdSiteYearDF %>%
    dplyr::filter(.data$plotType == "tower") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::summarise(events = paste(unique(.data$eventID), collapse = ", "),
                     count = length(unique(.data$eventID))) %>%
    dplyr::filter(count == 1) %>%
    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"))


  ##  Process plots with one Tower plot eventID in a 'site x year'
  #   Get records for all plots in 'site x year' combos with a single eventID
  stdSingleDF <- stdSiteYearDF %>%
    dplyr::filter(.data$siteYear %in% temp$siteYear)

  #   Calculate plot-level ANPP --> averages subplots within 40m x 40m Tower plots
  stdSinglePlotProdDF <- stdSingleDF %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$plotID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$plotSize,
                    .data$plotManagement) %>%
    dplyr::summarise(collectDate = as.Date(ifelse(all(is.na(.data$collectDate)), NA, min(.data$collectDate))),
                     TotalMass_gm2 = round(mean(.data$TotalMass_gm2, na.rm = TRUE),
                                           digits = 2),
                     CoolSeasonGram_gm2 = round(mean(.data$CoolSeasonGram_gm2, na.rm = TRUE),
                                                digits = 2),
                     Forbs_gm2 = round(mean(.data$Forbs_gm2, na.rm = TRUE),
                                       digits = 2),
                     NFixing_gm2 = round(mean(.data$NFixing_gm2, na.rm = TRUE),
                                         digits = 2),
                     WarmSeasonGram_gm2 = round(mean(.data$WarmSeasonGram_gm2, na.rm = TRUE),
                                                digits = 2),
                     WoodyPlants_gm2 = round(mean(.data$WoodyPlants_gm2, na.rm = TRUE),
                                             digits = 2))



  #   Calculate site-level ANPP --> each sampling cell is considered an independent sample







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
















  #   Create long-format table from input 'hbp_agb' data frame
  hbp_agb_long <- hbp_agb %>%
    dplyr::rename_with(~ stringr::str_remove(., "_gm2"),
                       c("AllHerbaceousPlants_gm2",
                         "CoolSeasonGraminoids_gm2",
                         "AnnualAndPerennialForbs_gm2",
                         "NFixingPlants_gm2",
                         "WarmSeasonGraminoids_gm2",
                         "WoodyStemmedPlants_gm2",
                         "Corn_gm2",
                         "Barley_gm2",
                         "OrchardGrass_gm2",
                         "Soybean_gm2",
                         "Sorghum_gm2",
                         "Wheat_gm2",
                         "Millet_gm2")) %>%
    tidyr::pivot_longer(cols = c("AllHerbaceousPlants",
                                 "CoolSeasonGraminoids",
                                 "WoodyStemmedPlants",
                                 "WarmSeasonGraminoids",
                                 "NFixingPlants",
                                 "AnnualAndPerennialForbs",
                                 "Corn",
                                 "Barley",
                                 "OrchardGrass",
                                 "Soybean",
                                 "Sorghum",
                                 "Wheat",
                                 "Millet"),
                        names_to = "herbGroup",
                        values_to = "agb_gm2")

  #   Populate exclosure == "N" if value is NA
  #--> Skipping this for now, not sure if needed?
  # hbp_agb_long$exclosure <- dplyr::if_else(is.na(hbp_agb_long$exclosure),
  #                                          "N", hbp_agb_long$exclosure,
  #                                          hbp_agb_long$exclosure)

  #   Remove duplicates based on primary keys; cannot use 'sampleID' because it is NA for records with targetTaxaPresent == "N"
  hbp_agb_long <- hbp_agb_long[!duplicated(paste0(hbp_agb_long$clipID,
                                                  hbp_agb_long$eventID,
                                                  hbp_agb_long$herbGroup),
                                           fromLast = TRUE), ]


  ##  Prepare 'hbp_agb_long' data frame for downstream processing
  #   Remove unneeded columns from 'hbp_agb_long' and keep 'ambient' and 'exclosure' data at native spatial resolution;
  #   i.e., assume subplots within large-stature Tower plots are independent.
  hbp_agb_long <- hbp_agb_long %>%
    dplyr::select("domainID",
                  "siteID",
                  "plotID",
                  "subplotID",
                  "sampleID",
                  "nlcdClass",
                  "plotType",
                  "plotManagement",
                  "collectDate",
                  "year",
                  "eventID",
                  "targetTaxaPresent",
                  "exclosure",
                  "peak",
                  "herbGroup",
                  "agb_gm2") %>%

    #   Create 'plot-year' variable for subsequent ID of Tower plots likely managed for grazing
    dplyr::mutate(plotYear = paste(.data$plotID, .data$year, sep = "-"),
                  .before = "plotID")


  # ##  Identify grazed sites in the dataset using exclosure == Y
  # grazed_sites <- hbp_agb %>%
  #   dplyr::select("siteID",
  #                 "exclosure") %>%
  #   dplyr::filter(.data$exclosure == "Y")
  #
  # grazed_sites <- sort(unique(as.character(grazed_sites$siteID)))
  #--> Likely not helpful to identify grazed sites, since grazing may not occur at all Tower plots within a site.



  ### Standard sites: Determine latest standing ambient biomass within a 'year' for each herbGroup ####
  #-->  This is productivity for sites/plots with no grazing exclosures.

  ### Obtain final standing mass for sites with no grazing management; also bring in Distributed plots at grazed sites and Tower plots at grazed sites that are not actively managed for grazing (i.e., only a portion of the Tower plots have cows and exclosures). For plots NOT managed for grazing (i.e., no exclosure == "Y" records) but that WERE clipped every grazed bout, cannot identify peak biomass as latest eventID --> use peak == "atPeak" instead.

  ##  For each plot-year combination, determine whether an exclosure was deployed in that year; if "Y", the plot-year is assumed to be under grazing management. Plots managed for grazing in a given year do not contribute to "standard" site-level productivity estimates and instead are put through the "consumption" workflow.

  #   Create list of unique Tower 'plot-year' combinations
  towerPlotYears <- sort(unique(hbp_agb_long$plotYear[hbp_agb_long$plotType == "tower"]))

  #   Identify grazed Tower 'plot-year' combinations
  if (length(towerPlotYears) > 0) {

    grazedPlotYears <- c()

    for (i in 1:length(towerPlotYears)) {

      tempDF <- hbp_agb_long %>%
        dplyr::filter(.data$plotYear == towerPlotYears[i])

      if ("Y" %in% tempDF$exclosure) {grazedPlotYears <- c(grazedPlotYears, towerPlotYears[i])}

    }

    rm(tempDF, i)

  } else {

    grazedPlotYears <- c()

  } # End length(towerPlotYears) conditional



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



  ### Grazed sites: Determine final standing mass needed for ANPP ####

  ##  Obtain final standing mass at plot level for sites with grazing management
  #   First, identify latest eventID for each 'site' x 'year' combination; these are used to estimate standing crop at last sampling eventID of the year.
  grazedLatestEvents <- hbp_agb_long %>%
    dplyr::filter(.data$plotType == "tower",
                  .data$siteID %in% grazed_sites) %>%
    dplyr::distinct(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID) %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::arrange(.data$eventID) %>%
    dplyr::slice_tail()

  #   Determine latest ambient standing crop at grazed sites
  if (length(grazed_sites) > 0) {

    grazedFinalMass <- hbp_agb_long %>%
      dplyr::filter(.data$exclosure == "N",
                    .data$siteID %in% grazed_sites,
                    .data$plotType == "tower",
                    .data$herbGroup == "AllHerbaceousPlants",
                    .data$eventID %in% grazedLatestEvents$eventID) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotID,
                     .data$subplotID)

  } else {

    grazedFinalMass <- data.frame()

  }



  ### Grazed sites: Estimate consumption to add to finalStandingMass ####
  if (nrow(grazedFinalMass) > 0) {

    ### For last eventID of season: Determine mean standing mass by siteID x year
    #   Remove NAs from 'agb_gm2' (insurance step)
    grazedFinalMass <- grazedFinalMass %>%
      dplyr::filter(!is.na(.data$agb_gm2))

    #   Generate means and SD for siteID x year
    grazedFinalMass <- grazedFinalMass %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotType,
                      .data$year,
                      .data$eventID,
                      .data$herbGroup) %>%
      dplyr::summarise("finalClipCount" = dplyr::n(),
                       "finalAGBMean_gm2" = round(mean(.data$agb_gm2, na.rm = TRUE),
                                                  digits = 2),
                       "finalAGBSD_gm2" = round(stats::sd(.data$agb_gm2, na.rm = TRUE),
                                                digits = 2),
                       .groups = "drop") %>%
      dplyr::rename("finalEventID" = "eventID")



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


  } else {

    herb_ANPP_grazed <- data.frame()

  } # end nrow(grazedFinalMass) conditional



  ### Site-level output: Bind 'standard' and 'grazed' data ####

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



  ### Plot-level output: Finalize dataframe ####
  #   Create plot-level output for herbGroup = "AllHerbaceousPlants"
  herb_ANPP_plot <- standardFinalMass %>%
    dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
    dplyr::rename("herbANPP_gm2yr" = "agb_gm2") %>%
    dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                          digits = 2),
                  .after = "exclosure")

  #   Create plot-level output for all herbGroups (but not "AllHerbaceousPlants")
  herb_ANPP_plot_herbgroup <- standardFinalMass %>%
    dplyr::filter(.data$herbGroup != "AllHerbaceousPlants") %>%
    dplyr::rename("herbANPP_gm2yr" = "agb_gm2") %>%
    dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                          digits = 2),
                  .after = "exclosure")



  ### Output data
  output <- list(herb_ANPP_site = herb_ANPP_site,
                 herb_ANPP_plot = herb_ANPP_plot,
                 herb_ANPP_plot_herbgroup = herb_ANPP_plot_herbgroup)

  return(output)
}
