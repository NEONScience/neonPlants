#' @title Estimate ANPP (Aboveground Net Primary Productivity) contributed by herbaceous vegetation
#'
#' @author Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Calculate herbaceous aboveground productivity using inputs from companion scaleHerbMass function.
#'
#' @details A list of herbaceous biomass dataframes created by the companion scaleHerbMass() function is read in, and aboveground net primary productivity (ANPP) is calculated for NEON "Herbaceous clip harvest" (DP1.10023.001) data.
#'
#' Where herbaceous plants are not subject to grazing management and exclosures are not in use, the estimate of herbaceous ANPP is simply the standing biomass of the last bout of the season.
#'
#' Where grazing exclosures have been established, consumption is calculated for each sampling event as average biomass within exclosures minus average biomass outside of exclosures. Consumption at each sampling bout is summed and then added to the standing biomass of the last bout of the season for an estimate of herbaceous ANPP.
#'
#' Note that the Science Design only supports calculating consumption at the site level. The exlosures are not close enough to non-exclosures to support plot level consumption estimates.
#'
#' @param inputDataList An R list object produced by the companion scaleHerbMass() function. [list]
#'
#' @param plotSubset The options are "all" (all tower and distributed plots), "towerAll" (all plots in the tower airshed but no distributed plots), the default of "towerAnnualSubset" (only the subset of tower plots that are sampled annually), and "distributed" (all distributed plots, which are sampled in 5-yr bouts and are spatially representative of the NLCD classes at at site). [character]
#'
#' @return A list that includes productivity summary data frames. Output tables include:
#'   * herb_ANPP_plot - Summarizes herbaceous ANPP for each plot x year combination ("Mg/ha/yr"). Plot level summaries are not returned for grazed sites, for the reason outlined in details.
#'   * herb_ANPP_site - Summarizes herbaceous ANPP for each site x year combination ("Mg/ha/yr").
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Herbaceous clip harvest data
#' HbpDat <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
#'      package = "basic", check.size = FALSE)
#'
#' # Use scaleHerbMass to generate output list that is to be used as input to estimateHerbProd
#' scaleHerbMassOutputs <- neonPlants::scaleHerbMass(
#' inputDataList = HbpDat,
#' inputBout = NA,
#' inputMass = NA
#' )
#'
#'
#' # example with arguments at default values
#' estimateHerbProdOutputs <-estimateHerbProd(inputDataList = scaleHerbMassOutputs)
#'
#' # example specifying many non-default arguments
#' estimateHerbProdOutputs <-estimateHerbProd(inputDataList = scaleHerbMassOutputs,
#' plotSubset = "towerAnnualSubset")
#'
#' }
#'
#' @export estimateHerbProd

estimateHerbProd = function(inputDataList,
                            plotSubset = "towerAnnualSubset") {

  options(dplyr.summarise.inform = FALSE)

  #   Check that 'inputDataList' is an object with class 'list'
  if(!methods::is(inputDataList, class = "list" )){
    stop("The inputDataList argument is expected to be a list generated with scaleHerbMass(). A character, data.frame, or NA argument is not allowed.")
    }

  #   Unlist inputDataList
    hbp_agb <- inputDataList$hbp_agb
    hbp_plot <- inputDataList$hbp_plot
    hbp_site <- inputDataList$hbp_site

  #   Verify that 'inputDataList' contains expected tables
  listExpNames_Hbp <- c("hbp_agb", "hbp_plot", "hbp_site")

  if (length(setdiff(listExpNames_Hbp, names(inputDataList))) > 0) {

    stop(glue::glue("Required tables missing from 'inputDataList' list:",
                    '{paste(setdiff(listExpNames_Hbp, names(inputDataList)), collapse = ", ")}',
                    .sep = " "))
  }

  ### Verify inputDataList tables contain required columns and data ####

  ### Verify user-supplied hbp_agb contains required data
  #   Check for required columns
  hbp_agb_ExpCols <- c("domainID",
                       "siteID",
                       "plotID",
                       "clipID",
                       "eventID",
                       "year",
                       "nlcdClass",
                       "plotType",
                       "plotSize",
                       "data_prod",
                       "bout",
                       "sampleID",
                       "clipArea",
                       "exclosure",
                       "peak",
                       "AllHerbaceousPlants_gm2")

  if (length(setdiff(hbp_agb_ExpCols, colnames(hbp_agb))) > 0) {
    stop(glue::glue("Required columns missing from 'hbp_agb':", '{paste(setdiff(hbp_agb_ExpCols, colnames(hbp_agb)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(hbp_agb) == 0) {
    stop(glue::glue("Table 'hbp_agb' has no data."))
  }



  ### Verify user-supplied hbp_plot contains required data
  #   Check for required columns
  hbp_plot_ExpCols <- c("siteID",
                        "plotID",
                        "year",
                        "nlcdClass",
                        "herbPeakMassTotal_Mgha")

  if (length(setdiff(hbp_plot_ExpCols, colnames(hbp_plot))) > 0) {
    stop(glue::glue("Required columns missing from 'hbp_plot':", '{paste(setdiff(hbp_plot_ExpCols, colnames(hbp_plot)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(hbp_plot) == 0) {
    stop(glue::glue("Table 'hbp_plot' has no data."))
  }


  start <- min(hbp_plot$year)
  end  <- max(hbp_plot$year)



  ### Error if not at least 2 years of data ####
  if (as.numeric(end) - as.numeric(start) < 1) {

    stop(glue::glue("At least 2 years of data are needed to calculate productivity (more when the plot sampling interval is longer than annual). Input dataset only has biomass data from: {unique(hbp_plot$year)}"))

  }

    # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'towerAll', 'towerAnnualSubset', 'distributed'.")
  }

  plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50) # convert to numeric (50 is highest plotPriority)
  plotType <- ifelse(plotSubset == "towerAnnualSubset" | plotSubset == "towerAll", "tower", "distributed")
  plotType <- ifelse(plotSubset == "all", "all", plotType)


  message("Summarizing above-ground herbaceous productivity  ..... ")



  ### Calculate productivity by site and year ####

  hbp_agb_long <- hbp_agb %>%
      dplyr::rename_with(~ stringr::str_remove(., "_gm2"),
                         c("AllHerbaceousPlants_gm2",
                           "CoolSeasonGraminoids_gm2",
                           "AnnualAndPerennialForbs_gm2",
                           "NFixingPlants_gm2",
                           "WarmSeasonGraminoids_gm2",
                           "WoodyStemmedPlants_gm2")) %>%
      tidyr::pivot_longer(cols = c("AllHerbaceousPlants",
                                   "CoolSeasonGraminoids",
                                   "WoodyStemmedPlants",
                                   "WarmSeasonGraminoids",
                                   "NFixingPlants",
                                   "AnnualAndPerennialForbs"),
                          names_to = "herbGroup",
                          values_to = "gm2")

    #   Populate exclosure == "N" if value is NA
    hbp_agb_long$exclosure <- dplyr::if_else(is.na(hbp_agb_long$exclosure),
                                             "N", hbp_agb_long$exclosure,
                                             hbp_agb_long$exclosure)

    #   Remove duplicates based on primary keys
    hbp_agb_long <- hbp_agb_long[!duplicated(paste0(hbp_agb_long$plotID,
                                                    hbp_agb_long$subplotID,
                                                    hbp_agb_long$year,
                                                    hbp_agb_long$bout,
                                                    hbp_agb_long$exclosure,
                                                    hbp_agb_long$herbGroup),
                                             fromLast = TRUE), ]

    # calculate mean of subplots (typically 2) within each plot
    hbp_agb_plot <- hbp_agb_long %>%
      dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$plotType,
                    .data$nlcdClass,
                    .data$year,
                    .data$peak,
                    .data$herbGroup,
                    .data$eventID,
                    .data$bout,
                    .data$exclosure) %>%
    dplyr::summarise(gm2 = mean(stats::na.omit(.data$gm2)) )

    grazed_sites <- hbp_agb %>% dplyr::select("siteID", "exclosure") %>% dplyr::filter(.data$exclosure == "Y")
    grazed_site_list <- unique(as.character(grazed_sites$siteID) )

   hbp_agb_plot <-  hbp_agb_plot[order( hbp_agb_plot$bout), ]
   finalStandingMass <- hbp_agb_plot %>%
    dplyr::filter(.data$exclosure == "N") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$plotType,
                    .data$nlcdClass,
                    .data$year,
                    .data$herbGroup) %>%
    dplyr::summarise(last_bout = dplyr::last(.data$eventID),
                     "finalStandingMass_gm2" = dplyr::last(.data$gm2))
    finalStandingMass <- finalStandingMass %>% dplyr::filter(!is.nan(.data$finalStandingMass_gm2))

 if (nrow(hbp_agb %>% dplyr::filter(.data$exclosure == "Y")) > 0) {

 #   Calculate mean exclosure == "Y" and exclosure == "N" mass by eventID
 exclosure <- hbp_agb_plot %>%
    dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$peak,
                    .data$herbGroup,
                    .data$eventID,
                    .data$bout,
                    .data$exclosure) %>%
    dplyr::summarise(gm2 = mean(stats::na.omit(.data$gm2)),
                     sampleSize = dplyr::n(),
                     .groups = "drop")

 #   Calculate mean consumption per eventID
 eventConsum <- exclosure %>%
  dplyr::select(-"sampleSize") %>%
  tidyr::pivot_wider(names_from = "exclosure",
                     values_from = "gm2",
                     names_prefix = "exclosure")
 eventConsum$consumption_gm2 <- eventConsum$exclosureY - eventConsum$exclosureN

 #   Sum consumption for all events per site and year
 siteConsum <- eventConsum %>% # sum(eventConsum$consumption) + dplyr::last(eventConsum$exclosureN)
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$herbGroup) %>%
    dplyr::summarise(consumption_gm2 = sum(stats::na.omit(.data$consumption_gm2)),
                     bouts = dplyr::n(),
                     .groups = "drop")


  herb_ANPP <- merge(finalStandingMass, siteConsum, by = c("domainID","siteID","year","herbGroup"), all.x = TRUE)
  herb_ANPP$consumption_gm2 <- ifelse(is.nan(herb_ANPP$consumption_gm2), 0, herb_ANPP$consumption_gm2)
  herb_ANPP$consumption_gm2 <- ifelse(is.na(herb_ANPP$consumption_gm2), 0, herb_ANPP$consumption_gm2)

 # Add standing mass and consumption to get productivity
  herb_ANPP$herbANPP_gm2yr <- herb_ANPP$finalStandingMass_gm2 + herb_ANPP$consumption_gm2

  } else {

 herb_ANPP <- finalStandingMass %>%
      dplyr::mutate(consumption_gm2 = 0, bouts = NA, herbANPP_gmn2yr = .data$finalStandingMass_gm2)

  } #   End exclosure conditional



  herb_ANPP$year <- as.numeric(herb_ANPP$year)
  priority_plots <- priority_plots %>% dplyr::select("plotID", "specificModuleSamplingPriority") # load into environment
  herb_ANPP <- merge(herb_ANPP, priority_plots, by = "plotID", all.x = TRUE)

  if(plotType == "tower") {herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$plotType == "tower")} # if plotType argument is "tower" then remove distributed plots
  if(!is.na(plotPriority)) {herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)} # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))
#  herb_ANPP <- herb_ANPP %>% dplyr::filter(!is.na(.data$herbANPP_gm2yr) ) # remove records that are missing productivity

  herb_ANPP <- herb_ANPP %>% dplyr::relocate(dplyr::any_of(c("plotType", "specificModuleSamplingPriority")), .after = "nlcdClass")
  herb_ANPP <- herb_ANPP %>% dplyr::relocate(dplyr::any_of(c("bout", "n_bouts_used_for_consumption")), .after = "herbGroup")
  herb_ANPP$herbANPP_Mghayr <- round(herb_ANPP$herbANPP_gm2yr * 10000 * 0.000001,4) # convert g/m2 to Mg/ha ;   g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha

  herb_ANPP_site <- herb_ANPP %>% dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>% dplyr::group_by(.data$siteID, .data$year, .data$herbGroup) %>% dplyr::summarise(herbPlotNum = dplyr::n(),
    herbANPPSD_gm2yr = round(stats::sd(.data$herbANPP_gm2yr, na.rm = TRUE),2),
    herbANPPMean_gm2yr = round(mean(.data$herbANPP_gm2yr, na.rm = TRUE),4),
    herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,2),  # convert g/m2 to Mg/ha ;   g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
    herbANPPMean_Mghayr = round(.data$herbANPPMean_gm2yr * 10000 * 0.000001,4) )

  herb_ANPP <- herb_ANPP %>% dplyr::filter(!.data$siteID %in% grazed_site_list) # remove grazed plots, since exclosures are not close enough to non-exclosures to accomodate a plot-level estimate

  output.list <- list(
    herb_ANPP_plot = herb_ANPP,
    herb_ANPP_site = herb_ANPP_site)

  return(output.list)
}

