#' @title Estimate ANPP (Above-ground Net Primary Productivity) contributed by herbaceous vegetation
#'
#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr

#' @description Estimate above-ground herbaceous productivity using inputs from the companion scaleHerbMass() function.
#'
#' @details Input data should be a list of herbaceous biomass data frames created by the companion scaleHerbMass() function. Ultimately, herbaceous above-ground net primary productivity (ANPP) is estimated from the NEON "Herbaceous clip harvest" (DP1.10023.001) data product.
#'
#' Where herbaceous plants are not subject to grazing management and exclosures are not in use, the estimate of herbaceous ANPP is simply the standing biomass of the last bout of the season.
#'
#' Where grazing exclosures have been established, consumption is calculated for each sampling event as average biomass within exclosures minus average biomass outside of exclosures. Consumption at each sampling bout is summed and then added to the standing biomass of the last bout of the season for an estimate of herbaceous ANPP.
#'
#' Note that the Science Design only supports calculating consumption at the site level. The clip harvests within exclosures are not close enough to ambient clip harvests to support plot-level consumption estimates.
#'
#' @param inputDataList An R list object produced by the companion scaleHerbMass() function. [list]
#'
#' @param plotSubset The options are "all" (all Tower and Distributed plots), the default of "tower" (all plots in the Tower airshed but no Distributed plots), and "distributed" (all Distributed plots, which are sampled on a 5-year interval and are spatially representative of the NLCD classes at at site). [character]
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
#' plotSubset = "all")
#'
#' }
#'
#' @export estimateHerbProd

estimateHerbProd = function(inputDataList,
                            plotSubset = "tower") {

  options(dplyr.summarise.inform = FALSE)


  ### Check inputDataList properties ####
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
  hbp_plot_ExpCols <- c("domainID",
                        "siteID",
                        "plotID",
                        "year",
                        "nlcdClass",
                        "plotType",
                        "herbPeakMassTotal_Mgha")

  if (length(setdiff(hbp_plot_ExpCols, colnames(hbp_plot))) > 0) {
    stop(glue::glue("Required columns missing from 'hbp_plot':", '{paste(setdiff(hbp_plot_ExpCols, colnames(hbp_plot)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(hbp_plot) == 0) {
    stop(glue::glue("Table 'hbp_plot' has no data."))
  }



  ### Error: Stop if not at least 2 years of data ####
  #   Define 'start' and 'end' years for productivity interval
  start <- min(hbp_plot$year)
  end  <- max(hbp_plot$year)

  #   Check for valid interval
  if (as.numeric(end) - as.numeric(start) < 1) {

    stop(glue::glue("At least 2 years of data are needed to calculate productivity (more when the plot sampling interval is longer than annual). Input dataset only has biomass data from: {unique(hbp_plot$year)}"))

  }



  ### Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "tower", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'tower', 'distributed'.")
  }

  # plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50) # convert to numeric (50 is highest plotPriority)
  # plotType <- ifelse(plotSubset == "towerAnnualSubset" | plotSubset == "towerAll", "tower", "distributed")
  # plotType <- ifelse(plotSubset == "all", "all", plotType)



  ### Prepare data for downstream calculations ####
  #   Create long-format table from input 'hbp_agb' data frame; need to begin with sampleID-level data because plot-level output from scaleHerbMass function omits majority of grazed bouts at grazed sites.
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
    hbp_agb_long <- hbp_agb_long[!duplicated(paste0(hbp_agb_long$sampleID,
                                                    hbp_agb_long$eventID,
                                                    hbp_agb_long$herbGroup),
                                             fromLast = TRUE), ]

    #   Calculate mean of subplots (typically 2) within each plot
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
                      .data$exclosure) %>%
      dplyr::summarise(collectDate = as.Date(max(.data$collectDate),
                                             format = "%Y-%m-%d"),
                       gm2 = mean(stats::na.omit(.data$gm2)),
                       .groups = "drop")

    #   Identify grazed sites using exclosure == Y
    grazed_sites <- hbp_agb %>%
      dplyr::select("siteID",
                    "exclosure") %>%
      dplyr::filter(.data$exclosure == "Y")

    grazed_site_list <- unique(as.character(grazed_sites$siteID))

    #   Order by eventID
    hbp_agb_plot <-  hbp_agb_plot[order(hbp_agb_plot$eventID), ]



    ### Determine latest standing ambient biomass within a 'year' for each herbGroup ####
    ### This is productivity for sites with no grazing; custom solution needed for SJER because growing season spans calendar year
    #   Obtain final standing mass for all sites except SJER
    finalStandingMass <- hbp_agb_plot %>%
      dplyr::filter(.data$exclosure == "N",
                    .data$siteID != "SJER") %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$plotType,
                      .data$nlcdClass,
                      .data$year,
                      .data$herbGroup) %>%
      dplyr::summarise(last_bout = dplyr::last(.data$eventID),
                       "finalStandingMass_gm2" = dplyr::last(.data$gm2))

    #   Obtain final standing mass for SJER; assumption is "final" eventID for given growing season is before 15th July
    if ("SJER" %in% hbp_agb_plot$siteID) {

      sjerFinalMass <- hbp_agb_plot %>%
        dplyr::filter(.data$exclosure == "N",
                      .data$siteID == "SJER") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$plotID,
                        .data$plotType,
                        .data$nlcdClass,
                        .data$year,
                        .data$herbGroup) %>%
        dplyr::filter(.data$collectDate < as.Date(glue::glue("{.data$year}-07-15"))) %>%
        dplyr::summarise(last_bout = dplyr::last(.data$eventID),
                         "finalStandingMass_gm2" = dplyr::last(.data$gm2))
    } else {

      sjerFinalMass <- data.frame()

    } # end SJER conditional

    #   Bind SJER data to other sites
    finalStandingMass <- finalStandingMass %>%
      dplyr::bind_rows(sjerFinalMass) %>%
      dplyr::filter(!is.nan(.data$finalStandingMass_gm2)) %>%
      dplyr::mutate(finalStandingMass_gm2 = round(.data$finalStandingMass_gm2,
                                                  digits = 3)) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotID)



    ### Grazed sites: Process exclosure == Y data ####
    if (nrow(hbp_agb %>% dplyr::filter(.data$exclosure == "Y")) > 0) {

      #   Calculate mean exclosure == "Y" and exclosure == "N" mass across plotIDs by eventID
      #--> Verify whether this needs modifying for correct SJER output --> it does.

      exclosure <- hbp_agb_plot %>%
        dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$peak,
                        .data$herbGroup,
                        .data$eventID,
                        .data$exclosure) %>%
        dplyr::summarise(gm2 = round(mean(stats::na.omit(.data$gm2)),
                                     digits = 3),
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
      #--> This does not work for SJER; also strange consumption is reported as "0" at sites where it is not estimated, should be "NA". Maybe fix by preventing non-grazed sites from getting into 'eventConsum' data frame...
      siteConsum <- eventConsum %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$herbGroup) %>%
        dplyr::summarise(consumption_gm2 = round(sum(stats::na.omit(.data$consumption_gm2)),
                                                 digits = 3),
                         bouts = dplyr::n(),
                         .groups = "drop")


      herb_ANPP <- merge(finalStandingMass,
                         siteConsum,
                         by = c("domainID", "siteID", "year", "herbGroup"),
                         all.x = TRUE)

      #   Assign "0" consumption to "NaN" and "NA" rows
      herb_ANPP$consumption_gm2 <- ifelse(is.nan(herb_ANPP$consumption_gm2), 0, herb_ANPP$consumption_gm2)

      herb_ANPP$consumption_gm2 <- ifelse(is.na(herb_ANPP$consumption_gm2), 0, herb_ANPP$consumption_gm2)

      #   Add standing mass and consumption to get productivity
      herb_ANPP$herbANPP_gm2yr <- herb_ANPP$finalStandingMass_gm2 + herb_ANPP$consumption_gm2

    } else {

      herb_ANPP <- finalStandingMass %>%
        dplyr::mutate(consumption_gm2 = 0,
                      bouts = NA,
                      herbANPP_gmn2yr = .data$finalStandingMass_gm2)

    } #   End exclosure conditional


    #--> Avoid using 'priority_plots' and use 'plotType' from input data instead
    # priority_plots <- priority_plots %>% dplyr::select("plotID", "specificModuleSamplingPriority") # load into environment
    # herb_ANPP <- merge(herb_ANPP, priority_plots, by = "plotID", all.x = TRUE)

    #   Filter plotType in output based on user-supplied 'plotSubset' argument
    if (plotType == "tower") {
      herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$plotType == "tower")} # if plotType argument is "tower" then remove distributed plots

    #--> plotPriority argument no longer exists
    #   if(!is.na(plotPriority)) {herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)} # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))
    # #  herb_ANPP <- herb_ANPP %>% dplyr::filter(!is.na(.data$herbANPP_gm2yr) ) # remove records that are missing productivity

    #   Relocate columns in 'herb_ANPP'
    herb_ANPP <- herb_ANPP %>%
      dplyr::relocate(dplyr::any_of(c("plotType", "specificModuleSamplingPriority")),
                      .after = "nlcdClass") %>%
      dplyr::relocate(dplyr::any_of(c("bout", "n_bouts_used_for_consumption")),
                      .after = "herbGroup")

    #   Convert ANPP to Mg/ha/y
    herb_ANPP$herbANPP_Mghayr <- round(herb_ANPP$herbANPP_gm2yr * 10000 * 0.000001,
                                       digits = 3)

    #   Generate site-level herbaceous ANPP estimates
    herb_ANPP_site <- herb_ANPP %>%
      dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
      dplyr::group_by(.data$siteID,
                      .data$year,
                      .data$herbGroup) %>%
      dplyr::summarise(herbPlotNum = dplyr::n(),
                       herbANPPSD_gm2yr = round(stats::sd(.data$herbANPP_gm2yr, na.rm = TRUE),
                                                digits = 2),
                       herbANPPMean_gm2yr = round(mean(.data$herbANPP_gm2yr, na.rm = TRUE),
                                                  digits = 3),
                       herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,
                                                 digits = 2),
                       herbANPPMean_Mghayr = round(.data$herbANPPMean_gm2yr * 10000 * 0.000001,
                                                   digits = 3))

    #   Remove plots from grazed sites because Science Design does not support plot-level productivity estimate
    herb_ANPP <- herb_ANPP %>%
      dplyr::filter(!.data$siteID %in% grazed_site_list)



    ### Output data
    output.list <- list(herb_ANPP_plot = herb_ANPP,
                        herb_ANPP_site = herb_ANPP_site)

    return(output.list)
}

