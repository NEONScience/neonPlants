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
#' @param plotType Optional filter for NEON plot type. Options are "tower" (default) or "all". A subset of 5 Tower plots are sampled annually (those plots with the highest plot priority), and remaining Tower plots are scheduled every 5 years. If "all" is selected, results include data from Distributed plots that are also sampled every 5 years. [character]
#'
#' @param plotPriority NEON plots have a priority number to spatially balance plots by NLCD class, etc. in the event that not all scheduled plots can be sampled. The lower the number the higher the priority. Options are "all" or "5". The default is "5" which retains just the 5 highest priority Tower plots that are sampled annually. [character]
#'
#' @return A list that includes productivity summary data frames. Output tables include:
#'   * herb_ANPP_plot - Summarizes herbaceous ANPP for each plot x year combination ("Mg/ha/yr"). Plot level summaries are not returned for grazed sites, for the reason outlined in details.
#'   * herb_ANPP_site - Summarizes herbaceous ANPP for each site x year combination ("Mg/ha/yr").
#'
#' @examples
#' \dontrun{
#' # If list is not in memory, load herbaceous biomass list of dataframes from local file:
#' load('scaleHerbMassOutputs.rds') # load list of dataframes created by scaleHerbMass function

#' # example with arguments at default values
#' estimateHerbProdOutputs <-estimateHerbProd(inputDataList = scaleHerbMassOutputs)
#'
#' # example specifying many non-default arguments
#' estimateHerbProdOutputs <-estimateHerbProd(inputDataList = scaleHerbMassOutputs,
#' plotType = "all",
#' plotPriority = "all")
#'
#' }
#'
#' @export estimateHerbProd

estimateHerbProd = function(inputDataList,
                            plotType = "tower",
                            plotPriority = "5") {

  options(dplyr.summarise.inform = FALSE)

  #   Check that 'inputDataList' is an object with class 'list'
  if(!methods::is(inputDataList, class = "list" )){
    stop("The inputDataList argument is expected to be a list generated with scaleHerbMass(). A character, data.frame, or NA argument is not allowed.")
    }

  #   Unpack scaleHerbMass() input data to the environment
  list2env(inputDataList ,.GlobalEnv)
    hbp_agb <- inputDataList$hbp_agb # needed to avoid devtools::check() note about visible binding for global variable
    hbp_plot <- inputDataList$hbp_plot # needed to avoid devtools::check() note about visible binding for global variable


  #   Verify that 'inputDataList' contains expected tables
  listExpNames_Hbp <- c("hbp_agb", "hbp_plot", "hbp_site")

  if (length(setdiff(listExpNames_Hbp, names(inputDataList))) > 0) {

    stop(glue::glue("Required tables missing from 'inputDataList' list:",
                    '{paste(setdiff(listExpNames_Hbp, names(inputDataList)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Error if invalid plotType option selected
  if (!plotType %in% c("tower", "all")) {
    stop("The only valid plotType options are 'tower' or 'all'.")
  }

  #   Error if invalid plotPriority option selected
  if (!(as.character(plotPriority)) %in% c("all", "5")) {
    stop("The plotPriority argument should be either 'all' (all plots) or '5' (the 5 highest priority plots).")
  }

  #   Conditionally convert 'plotPriority' to numeric (30 is highest plotPriority)
  plotPriority <- ifelse(plotPriority == "5", 5, 30)



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

  message("Summarizing above-ground herbaceous productivity  ..... ")



  ### Calculate productivity by site and year ####

  # hbp_event_means <- hbp_agb %>%
  #   dplyr::group_by(.data$domainID, .data$siteID, .data$plotID, .data$plotType, .data$eventID, .data$year, .data$bout, .data$nlcdClass, .data$exclosure, .data$peak) %>%
  #   dplyr::summarise(mean_herb_gm2 = mean(.data$AllHerbaceousPlants_gm2, na.rm = TRUE)) # calc the mean biomass of the individual plots within eventID
  # hbp_event_means$exclosure <- ifelse(is.na(hbp_event_means$exclosure), "N", hbp_event_means$exclosure)

  if (nrow(hbp_agb %>% dplyr::filter(.data$exclosure == "Y")) > 0) {

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

    #  hbp_agb_long$exclosure <- dplyr::if_else(hbp_agb_long$plotType == "distributed" & is.na(hbp_agb_long$exclosure), "N", hbp_agb_long$exclosure, hbp_agb_long$exclosure)

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

    #   Calculate consumption as 'gm2_C'
    consumption <- hbp_agb_long %>%
      tidyr::pivot_wider(id_cols = c("domainID",
                                     "siteID",
                                     "plotID",
                                     "plotType",
                                     "nlcdClass",
                                     "subplotID",
                                     "year",
                                     "peak",
                                     "herbGroup",
                                     "eventID",
                                     "bout"),
                         names_from = "exclosure",
                         values_from = "gm2",
                         names_prefix = "gm2_") %>%

      #   Consumption is biomass in exclosure (Y) minus biomass outside of exclosure (N)
      dplyr::mutate(gm2_C = .data$gm2_Y - .data$gm2_N)

  } else {

    #   If no exclosure set consumption to 0
    consumption <- hbp_agb_long %>%
      dplyr::mutate(gm2_C = 0) %>%
      dplyr::rename("gm2_N" = "gm2")

  } #   End exclosure conditional

  # all the same grouping variables as in the pivot_longer except subplotID, so the mean of subplots (typically 2) within plot is calculated
  plot_consumption <- consumption %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$plotType,
                    .data$nlcdClass,
                    .data$year,
                    .data$peak,
                    .data$herbGroup,
                    .data$eventID,
                    .data$bout) %>%
    dplyr::summarise(gm2_Y = mean(stats::na.omit(.data$gm2_Y)),
                     gm2_N = mean(stats::na.omit(.data$gm2_N)),
                     gm2_C = mean(stats::na.omit(.data$gm2_C)) )

  # all the same grouping variables used to create plot_consumption except for eventID and bout, allowing us to count the number of bouts or extract the values of the last bout
  herb_ANPP_total <- plot_consumption %>%
    dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$year,
                    .data$herbGroup) %>%

    # add standing biomass and, if applicable, consumption calculated from exclosures
    dplyr::summarise(totalConsumption_gm2 = sum(stats::na.omit(.data$gm2_C)),
                     n_bouts_used_for_consumption = length(stats::na.omit(.data$gm2_C)),
                     last_bout = dplyr::last(.data$bout),
                     "last_bout_mean_herb_gm2" = dplyr::last(.data$gm2_N),
                     "herbANPP_gm2yr" = .data$last_bout_mean_herb_gm2 + .data$totalConsumption_gm2,
                     .groups = "drop")

  herb_ANPP_total$bout <- "allBouts"

  herb_ANPP_herbGroup <- plot_consumption %>% dplyr::filter(.data$herbGroup != "AllHerbaceousPlants" & .data$peak =="atPeak") %>% dplyr::group_by(.data$domainID, .data$siteID, .data$plotID, .data$nlcdClass, .data$plotType, .data$year, .data$herbGroup) %>%
    dplyr::summarise(
      totalConsumption_gm2 = sum(stats::na.omit(.data$gm2_C)),
      n_bouts_used_for_consumption = length(stats::na.omit(.data$gm2_C)),
      last_bout = dplyr::last(.data$bout),
      "last_bout_mean_herb_gm2" = dplyr::last(.data$gm2_N),
      "herbANPP_gm2yr" = .data$last_bout_mean_herb_gm2 + .data$totalConsumption_gm2  # add standing biomass and, if applicable, consumption calculated from exclosures
      ) %>% dplyr::ungroup() # important note herbGroup stats almost always based on just the one peak bout
  herb_ANPP_herbGroup$bout <- "peakBout"

  herb_ANPP <- rbind(herb_ANPP_total, herb_ANPP_herbGroup)

  herb_ANPP$year <- as.numeric(herb_ANPP$year)
  priority_plots <- priority_plots %>% dplyr::select("plotID", "specificModuleSamplingPriority") # load into environment
  herb_ANPP <- merge(herb_ANPP, priority_plots, by = "plotID", all.x = TRUE)

  if(plotType == "tower") {herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$plotType == "tower")} # if plotType argument is "tower" then remove distributed plots
  if(!is.na(plotPriority)) {herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)} # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))
  herb_ANPP <- herb_ANPP %>% dplyr::filter(!is.na(.data$herbANPP_gm2yr) ) # remove records that are missing productivity

  herb_ANPP <- herb_ANPP %>% dplyr::relocate(dplyr::any_of(c("plotType", "specificModuleSamplingPriority")), .after = "nlcdClass")
  herb_ANPP <- herb_ANPP %>% dplyr::relocate(dplyr::any_of(c("bout", "n_bouts_used_for_consumption")), .after = "herbGroup")
  herb_ANPP$herbANPP_Mghayr <- round(herb_ANPP$herbANPP_gm2yr * 10000 * 0.000001,4) # convert g/m2 to Mg/ha ;   g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha

  herb_ANPP_site <- herb_ANPP %>% dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>% dplyr::group_by(.data$siteID, .data$year, .data$herbGroup) %>% dplyr::summarise(herbPlotNum = dplyr::n(),
    herbANPPSD_gm2yr = round(stats::sd(.data$herbANPP_gm2yr, na.rm = TRUE),2),
    herbANPPMean_gm2yr = round(mean(.data$herbANPP_gm2yr, na.rm = TRUE),4),
    herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,2),  # convert g/m2 to Mg/ha ;   g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
    herbANPPMean_Mghayr = round(.data$herbANPPMean_gm2yr * 10000 * 0.000001,4) )

  herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$n_bouts_used_for_consumption == 0) # remove grazed plots, since exclosures are not close enough to non-exclosures to accomodate a plot-level estimate

  output.list <- list(
    herb_ANPP_plot = herb_ANPP,
    herb_ANPP_site = herb_ANPP_site)

  return(output.list)
}

