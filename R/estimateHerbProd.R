#' @title Estimate ANPP (Above-ground Net Primary Productivity) contributed by herbaceous vegetation
#'
#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr

#' @description Estimate above-ground herbaceous productivity using NEON Herbaceous Clip Harvest data tables (DP1.10023.001).
#'
#' @details Input data should be a list of herbaceous biomass (DP1.10023.001) data frames downloaded via the neonUtilities::loadByProduct() function. The companion scaleHerbMass() function is called internally, and outputs from this function are used to estimate herbaceous productivity.
#'
#' Where herbaceous plants are not subject to grazing management and exclosures are not in use, the estimate of herbaceous ANPP is simply the standing biomass of the last bout of the season.
#'
#' Where grazing management occurs in Tower plots and exclosures have been established, consumption is calculated for each Tower plot gsampling event as average biomass within exclosures minus average biomass outside of exclosures. Consumption at each sampling bout is summed and then added to the standing biomass of the last bout of the season for an estimate of herbaceous ANPP.
#'
#' Note that the Science Design only supports calculating consumption at the site level. The clip harvests within exclosures are not close enough to ambient clip harvests to support plot-level consumption estimates.
#'
#' @param inputDataList An R list object produced by the neonUtilities::loadByProduct() function for the NEON Herbaceous Clip Harvest data product. [list]
#'
#' @param plotSubset The options are "all" (all Tower and Distributed plots), the default of "tower" (all plots in the Tower airshed but no Distributed plots), and "distributed" (all Distributed plots, which are sampled on a 5-year interval and are spatially representative of the NLCD classes at a site). [character]
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
#' # example with arguments at default values
#' df <-estimateHerbProd(inputDataList = HbpDat)
#'
#' # example specifying many non-default arguments
#' df <-estimateHerbProd(inputDataList = HbpDat,
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
    stop("The inputDataList argument is expected to be a list generated with neonUtilities::loadByProduct(). A character, data.frame, or NA argument is not allowed.")
  }



  ### Check plotSubset properties
  if (!plotSubset %in% c("all", "tower", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'tower', 'distributed'.")
  }



  ### Generate scaleHerbMass outputs
  scaleHerbMassOutputs <- neonPlants::scaleHerbMass(inputDataList = inputDataList,
                                                    plotSubset = plotSubset)

  #   Unlist outputs
  hbp_agb <- scaleHerbMassOutputs$hbp_agb
  hbp_plot <- scaleHerbMassOutputs$hbp_plot
  hbp_site <- scaleHerbMassOutputs$hbp_site



  ### Verify scaleHerbMassOutputs tables contain required columns and data ####

  ### Verify scaleHerbMass output table 'hbp_agb' contains required data
  #   Check for data
  if (nrow(hbp_agb) == 0) {
    stop(glue::glue("Table from scaleHerbMass() output 'hbp_agb' has no data."))
  }



  ### Verify scaleHerbMass output table 'hbp_plot' contains required data
  #   Check for data
  if (nrow(hbp_plot) == 0) {
    stop(glue::glue("Table from scaleHerbMass() output 'hbp_plot' has no data."))
  }



  ### Error: Stop if not at least 2 years of data ####
  #   Define 'start' and 'end' years for productivity interval
  start <- min(hbp_plot$year)
  end  <- max(hbp_plot$year)

  #   Check for valid interval
  if (as.numeric(end) - as.numeric(start) < 1) {

    stop(glue::glue("At least 2 years of data are needed to calculate productivity (more when the plot sampling interval is longer than annual). Input dataset only has biomass data from: {unique(hbp_plot$year)}"))

  }



  ### Prepare plot-level data for downstream calculations ####
  #   Filter input data using user-supplied 'plotSubset' argument; insurance step since scaleHerbMass() output should already only be produced for user-selected option
  if (plotSubset %in% c("distributed", "tower")) {

    hbp_agb <- hbp_agb %>%
      dplyr::filter(.data$plotType == plotSubset)

  }

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
                          values_to = "agb_gm2")

    #   Populate exclosure == "N" if value is NA
    hbp_agb_long$exclosure <- dplyr::if_else(is.na(hbp_agb_long$exclosure),
                                             "N", hbp_agb_long$exclosure,
                                             hbp_agb_long$exclosure)

    #   Remove duplicates based on primary keys
    hbp_agb_long <- hbp_agb_long[!duplicated(paste0(hbp_agb_long$sampleID,
                                                    hbp_agb_long$eventID,
                                                    hbp_agb_long$herbGroup),
                                             fromLast = TRUE), ]

    #   Calculate mean of subplots within each plot (for large-stature Tower plots); uncertainty ignored at this step
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
                       agb_gm2 = dplyr::case_when(all(is.na(.data$agb_gm2)) ~ NA,
                                                  TRUE ~ mean(stats::na.omit(.data$agb_gm2))),
                       .groups = "drop")

    #   Order by eventID; needed for later use of dplyr::last()
    hbp_agb_plot <-  hbp_agb_plot[order(hbp_agb_plot$eventID), ]

    #   Identify grazed sites using exclosure == Y
    grazed_sites <- hbp_agb %>%
      dplyr::select("siteID",
                    "exclosure") %>%
      dplyr::filter(.data$exclosure == "Y")

    grazed_sites <- unique(as.character(grazed_sites$siteID))



    ### Standard sites: Determine latest standing ambient biomass within a 'year' for each herbGroup ####
    #-->  This is productivity for sites/plots with no grazing exclosures.

    ##  Obtain final standing mass for sites with no grazing management; also bring in Distributed plots at grazed sites
    standardFinalMass <- hbp_agb_plot %>%
      dplyr::filter(!.data$siteID %in% grazed_sites | (.data$siteID %in% grazed_sites &
                                                         .data$plotType == "distributed")) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$plotType,
                      .data$nlcdClass,
                      .data$year,
                      .data$herbGroup) %>%
      dplyr::summarise("finalEventID" = dplyr::last(.data$eventID),
                       "finalAGB_gm2" = dplyr::last(.data$agb_gm2)) %>%
      dplyr::mutate("finalAGB_gm2" = round(.data$finalAGB_gm2,
                                           digits = 2))



    ### Standard sites: Calculate site-level productivity and SD by 'year' and 'herbGroup' ####

    if (nrow(standardFinalMass) > 0) {

      herb_ANPP_site <- standardFinalMass %>%
        dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotType,
                        .data$herbGroup) %>%
        dplyr::summarise(herbPlotCount = dplyr::n(),
                         herbANPP_gm2yr = round(mean(.data$finalAGB_gm2, na.rm = TRUE),
                                                digits = 2),
                         herbANPPSD_gm2yr = round(stats::sd(.data$finalAGB_gm2, na.rm = TRUE),
                                                  digits = 2))

    } else {

      herb_ANPP_site <- data.frame()

    }



    ### Grazed sites: Determine final standing mass needed for ANPP ####

    ##  Obtain final standing mass for sites with grazing management (except SJER); custom solution needed for SJER because growing season spans calendar year
    if (length(setdiff(grazed_sites, "SJER")) > 0) {

      grazedNoSJER <- setdiff(grazed_sites, "SJER")

      grazedFinalMass <- hbp_agb_plot %>%
        dplyr::filter(.data$exclosure == "N",
                      .data$siteID %in% grazedNoSJER,
                      .data$plotType == "tower") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$plotID,
                        .data$plotType,
                        .data$year,
                        .data$herbGroup) %>%
        dplyr::summarise("finalEventID" = dplyr::last(.data$eventID),
                         "finalAGB_gm2" = dplyr::last(.data$agb_gm2))

    } else {

      grazedFinalMass <- data.frame()

    }


    ##  Obtain final standing mass for SJER; assumption is "final" eventID for given growing season is before 15th July
    if ("SJER" %in% grazed_sites) {

      sjerFinalMass <- hbp_agb_plot %>%
        dplyr::filter(.data$exclosure == "N",
                      .data$siteID == "SJER",
                      .data$plotType == "tower") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$plotID,
                        .data$plotType,
                        .data$year,
                        .data$herbGroup) %>%
        dplyr::filter(.data$collectDate < as.Date(glue::glue("{.data$year}-07-15"))) %>%
        dplyr::summarise("finalEventID" = dplyr::last(.data$eventID),
                         "finalAGB_gm2" = dplyr::last(.data$agb_gm2))

    } else {

      sjerFinalMass <- data.frame()

    }


    ##  Bind all grazed data together
    grazedFinalMass <- dplyr::bind_rows(grazedFinalMass,
                                        sjerFinalMass)



    ### Grazed sites: Estimate consumption to add to finalStandingMass ####
    if (nrow(grazedFinalMass) > 0) {


      ### For last eventID of season: Determine mean standing mass by siteID x year
      #   Remove NAs from 'finalAGB_gm2' and arrange data
      grazedFinalMass <- grazedFinalMass %>%
        dplyr::filter(!is.na(.data$finalAGB_gm2)) %>%
        dplyr::arrange(.data$domainID,
                       .data$siteID,
                       .data$year,
                       .data$plotID)

      #   Generate means and SD for siteID x year
      grazedFinalMass <- grazedFinalMass %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotType,
                        .data$herbGroup) %>%
        dplyr::summarise("herbPlotCount" = dplyr::n(),
                         "finalAGBMean_gm2" = round(mean(.data$finalAGB_gm2, na.rm = TRUE),
                                                    digits = 2),
                         "finalAGBSD_gm2" = round(stats::sd(.data$finalAGB_gm2, na.rm = TRUE),
                                                  digits = 2),
                         .groups = "drop")



      ### Calculate consumption productivity component using exclosure data
      #   Calculate mean exclosure == "Y" and exclosure == "N" mass across plotIDs by eventID
      exclosure <- hbp_agb_plot %>%
        dplyr::filter(.data$herbGroup == "AllHerbaceousPlants",
                      .data$siteID %in% grazed_sites) %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$peak,
                        .data$herbGroup,
                        .data$eventID,
                        .data$exclosure) %>%
        dplyr::summarise("agbMean_gm2" = round(mean(stats::na.omit(.data$agb_gm2)),
                                               digits = 2),
                         "agbSD_gm2" = round(stats::sd(.data$agb_gm2, na.rm = TRUE),
                                             digits = 2),
                         .groups = "drop")

      #   Calculate mean consumption per eventID
      eventConsum <- exclosure %>%
        tidyr::pivot_wider(names_from = "exclosure",
                           values_from = c("agbMean_gm2", "agbSD_gm2"),
                           names_prefix = "exclosure") %>%
        dplyr::mutate("consumpMean_gm2" = .data$agbMean_gm2_exclosureY - .data$agbMean_gm2_exclosureN,
                      "consumpSD_gm2" = .data$agbSD_gm2_exclosureY + .data$agbSD_gm2_exclosureN)


      ##  For SJER: Assign sampling eventIDs to 'year' using 15th July cut-off
      if ("SJER" %in% eventConsum$siteID) {

        #   Determine startDate, endDate for each eventID and assign corrected growing season 'year'
        sjerEvents <- hbp_agb_plot %>%
          dplyr::filter(.data$siteID == "SJER") %>%
          dplyr::group_by(.data$eventID,
                          .data$year) %>%
          dplyr::summarise(startDate = min(.data$collectDate),
                           endDate = max(.data$collectDate),
                           .groups = "drop") %>%
          dplyr::mutate(correctedYear = dplyr::case_when(.data$endDate < as.Date(glue::glue("{.data$year}-07-15")) ~
                                                           (.data$year - 1),
                                                         TRUE ~ .data$year))

        #   Join with eventConsum then assign 'correctedYear' for SJER eventIDs
        eventConsum <- dplyr::left_join(eventConsum,
                                        sjerEvents %>%
                                          dplyr::select("eventID",
                                                        "correctedYear"),
                                        by = "eventID") %>%
          dplyr::mutate(year = dplyr::case_when(.data$siteID == "SJER" ~ correctedYear,
                                                TRUE ~ year)) %>%
          dplyr::select(-"correctedYear")

      } # End SJER conditional


      ##  Sum consumption for all events per site and year
      siteConsum <- eventConsum %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$herbGroup) %>%
        dplyr::summarise("consumpEventCount" = dplyr::n(),
                         "consumption_gm2" = round(sum(.data$consumpMean_gm2, na.rm = TRUE),
                                                   digits = 2),
                         "consumptionSD_gm2" = sum(.data$consumpSD_gm2, na.rm = TRUE),
                         .groups = "drop")


      ##  Join with 'grazedFinalMass' to calculate herbaceous ANPP at grazed sites
      herb_ANPP_grazed <- dplyr::left_join(grazedFinalMass,
                                           siteConsum,
                                           by = c("domainID", "siteID", "year", "herbGroup")) %>%
        dplyr::mutate(herbANPP_gm2yr = rowSums(dplyr::across(c("finalAGBMean_gm2", "consumption_gm2")),
                                               na.rm = TRUE),
                      herbANPPSD_gm2yr = rowSums(dplyr::across(c("finalAGBSD_gm2", "consumptionSD_gm2")),
                                                 na.rm = TRUE),
                      .after = "herbGroup")


    } else {

      herb_ANPP_grazed <- data.frame()

    } # end nrow(grazedFinalMass) conditional



    ### Site-level output: Bind 'standard' and 'grazed' data ####

    herb_ANPP_site <- dplyr::bind_rows(herb_ANPP_site,
                                       herb_ANPP_grazed)

    #   Add columns with "Mg/ha/y" units for ANPP and SD
    herb_ANPP_site <- herb_ANPP_site %>%
      dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                            digits = 2),
                    herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,
                                              digits = 2),
                    .after = "herbPlotCount")



    ### Plot-level output: Finalize dataframe ####
    #   Create plot-level output for herbGroup = "AllHerbaceousPlants"
    herb_ANPP_plot <- standardFinalMass %>%
      dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
      dplyr::rename("eventID" = "finalEventID",
                    "herbANPP_gm2yr" = "finalAGB_gm2") %>%
      dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                            digits = 2),
                    .after = "eventID")

    #   Create plot-level output for all herbGroups (but not "AllHerbaceousPlants")
    herb_ANPP_plot_herbgroup <- standardFinalMass %>%
      dplyr::filter(.data$herbGroup != "AllHerbaceousPlants") %>%
      dplyr::rename("eventID" = "finalEventID",
                    "herbANPP_gm2yr" = "finalAGB_gm2") %>%
      dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                            digits = 2),
                    .after = "eventID")



    ### Output data
    output <- list(herb_ANPP_site = herb_ANPP_site,
                   herb_ANPP_plot = herb_ANPP_plot,
                   herb_ANPP_plot_herbgroup = herb_ANPP_plot_herbgroup)

    return(output)
}

