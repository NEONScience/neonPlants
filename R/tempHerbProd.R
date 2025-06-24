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



  ### Prepare plot-level data for downstream calculations ####
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
    #-->  This is productivity for sites with no grazing.

    ##  Obtain final standing mass for sites with no grazing management
    standardFinalMass <- hbp_agb_plot %>%
      dplyr::filter(!.data$siteID %in% grazed_sites) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$plotType,
                      .data$nlcdClass,
                      .data$year,
                      .data$herbGroup) %>%
      dplyr::summarise("finalEventID" = dplyr::last(.data$eventID),
                       "finalAGB_gm2" = dplyr::last(.data$agb_gm2))



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


    ##  Determine mean final mass by siteID x year
    #   Bind other grazed site output to SJER output
    grazedFinalMass <- dplyr::bind_rows(grazedFinalMass,
                                        sjerFinalMass) %>%
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
      dplyr::summarise("finalAGBMean_gm2" = round(mean(.data$finalAGB_gm2, na.rm = TRUE),
                                                  digits = 3),
                       "finalAGBSD_gm2" = round(stats::sd(.data$finalAGB_gm2, na.rm = TRUE),
                                                digits = 2),
                       .groups = "drop")



    ### Grazed sites: Estimate consumption to add to finalStandingMass ####
    if (nrow(grazedFinalMass) > 0) {

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
                                           digits = 3),
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
                                                   digits = 3),
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

    } #   End exclosure conditional



    ### Standard sites: Calculate site-level productivity and SD by 'year' and 'herbGroup' ####
    #--> After standardizing columns with 'grazed' output, consider moving up under other "standard" section
    #--> begin again here, incorporating below as needed





    ##  Filter plotType in output based on user-supplied 'plotSubset' argument
    if (plotSubset %in% c("distributed", "tower")) {

      herb_ANPP <- herb_ANPP %>%
        dplyr::filter(.data$plotType == plotSubset)

    }


    ##  Rename 'herb_ANPP' for output
    herb_ANPP <- herb_ANPP %>%
      dplyr::rename("eventID" = "last_bout",
                    "consumptionEventCount" = "bouts")



    ##  Convert ANPP to Mg/ha/y
    herb_ANPP$herbANPP_Mghayr <- round(herb_ANPP$herbANPP_gm2yr * 10000 * 0.000001,
                                       digits = 3)


    ##  Generate site-level herbaceous ANPP estimates
    herb_ANPP_site <- herb_ANPP %>%
      dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$herbGroup) %>%
      dplyr::summarise(herbPlotNum = dplyr::n(),
                       herbANPPMean_gm2yr = round(mean(.data$herbANPP_gm2yr, na.rm = TRUE),
                                                  digits = 3),
                       herbANPPSD_gm2yr = round(stats::sd(.data$herbANPP_gm2yr, na.rm = TRUE),
                                                digits = 2),
                       herbANPPMean_Mghayr = round(.data$herbANPPMean_gm2yr * 10000 * 0.000001,
                                                   digits = 3),
                       herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,
                                                 digits = 2))
    #--> This uncertainty (SD) may be wrong because uncertainty from all consumption bouts is not accounted for; need to add uncertainty across all bouts...

    #   Add consumption data to site-level output table --> re-work this once consumption uncertainty is properly done
    test <- dplyr::left_join(herb_ANPP_site,
                             siteConsum,
                             by = c("domainID", "siteID", "year", "herbGroup")) %>%
      dplyr::relocate("bouts",
                      "consumption_gm2",
                      .after = "herbPlotNum") %>%
      dplyr::rename("consumptionEventCount" = "bouts",
                    "consumption_gm2yr" = "consumption_gm2")


    ##  Remove plots from grazed sites because Science Design does not support plot-level productivity estimate
    herb_ANPP <- herb_ANPP %>%
      dplyr::filter(!.data$siteID %in% grazed_site_list)



    ### Output data
    output.list <- list(herb_ANPP_plot = herb_ANPP,
                        herb_ANPP_site = herb_ANPP_site)

    return(output.list)
}

