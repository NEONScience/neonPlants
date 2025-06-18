#' @title Scale herbaceous biomass by functional group data to mass per area
#'
#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Join NEON Herbaceous Clip Harvest data tables (DP1.10023.001) to calculate herbaceous biomass by functional group per unit area as well as total herbaceous biomass per unit area. Biomass outputs can be used in the neonPlants estimateMass() function, and the estimateHerbProd() productivity function.
#'
#' Data inputs are "Herbaceous clip harvest" data (DP1.10023.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' @details Input data can be filtered by site, date, and plot subset. Herbaceous biomass data are scaled to an area basis at the hierarchical levels of sampling cell, plot, and site. Input data may be provided either as a list or as individual tables. However, if both list and table inputs are provided at the same time the function will error out. For all output data, columns with the same name as input data have identical units and definitions; where needed, new columns contain new units information.
#'
#' NEON weighs a minimum of 5% of samples a second time so that data users can estimate the uncertainty associated with different technicians weighing dried herbaceous biomass; QA samples of this nature are identified via qaDryMass == "Y". The function calculates the mean when QA masses exist. Samples with Sampling Impractical values other than "OK" are removed prior to generating output data.
#'
#' @param inputDataList A list object comprised of "Herbaceous clip harvest" tables (DP1.10023.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 'inputBout', and 'inputMass' arguments. [list]
#'
#' @param inputBout The 'hbp_perbout' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputMass The 'hbp_massdata' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param plotSubset The options are "all" (all tower and distributed plots), the default of "tower" (all plots in the tower airshed but no distributed plots), and "distributed" (all distributed plots, which are sampled on a 5-year interval and are spatially representative of the NLCD classes at a site). [character]
#'
#' @return A list that includes biomass summary data at multiple scales. Output tables include:
#'   * hbp_agb - Summarizes above-ground herbaceous biomass for each sampleID in the input data ("g/m2").
#'   * hbp_plot - Summarizes above-ground herbaceous biomass for each plot by year combination (both "g/m2" and "Mg/ha").
#'   * hbp_site - Summarizes above-ground herbaceous biomass for each site by year combination (both "g/m2" and "Mg/ha").
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Herbaceous clip harvest data
#' HbpDat <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
#'      package = "basic", check.size = FALSE)
#'
#' # example with arguments at default values
#' df <- neonPlants::scaleHerbMass(
#' inputDataList = HbpDat,
#' inputBout = NA,
#' inputMass = NA
#' )
#'
#' }
#'
#' @export scaleHerbMass


scaleHerbMass = function(inputDataList,
                         inputBout = NA,
                         inputMass = NA,
                         plotSubset = "tower") {

  options(dplyr.summarise.inform = FALSE)



  ### Verify user inputs are correct type and contain expected tables ####

  ### Verify user-supplied 'inputDataList' object contains correct data if not missing
  if (!missing(inputDataList)) {

    #   Check that input is a list
    if (!inherits(inputDataList, "list")) {
      stop(glue::glue("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"))
    }

    #   Check that required tables within list match expected names
    listExpNames <- c("hbp_perbout", "hbp_massdata")


    #   All expected tables required when includeDilution == TRUE
    if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from 'inputDataList':",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
    }

  } else {

    inputDataList <- NULL

  } # end missing conditional



  ### Verify table inputs are NA if 'inputDataList' is supplied
  if (inherits(inputDataList, "list") & (!is.logical(inputBout) | !is.logical(inputMass))) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }



  ### Verify 'inputBout' and 'inputMass' are data frames if 'inputDataList' is missing
  if (is.null(inputDataList) &
      (!inherits(inputBout, "data.frame") | !inherits(inputMass, "data.frame"))) {

    stop("Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
  }


  if (inherits(inputDataList, "list")) {

    inputBout <- inputDataList$hbp_perbout
    inputMass <- inputDataList$hbp_massdata

  }



  ### Verify input tables contain required columns and data ####

  ### Verify 'inputBout' table contains required data
  #   Check for required columns
  boutExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "clipArea", "exclosure", "samplingImpractical", "targetTaxaPresent")

  if (length(setdiff(boutExpCols, colnames(inputBout))) > 0) {
    stop(glue::glue("Required columns missing from 'inputBout':", '{paste(setdiff(boutExpCols, colnames(inputBout)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(inputBout) == 0) {
    stop(glue::glue("Table 'inputBout' has no data."))
  }



   ### Verify 'inputMass' table contains required data
  #   Check for required columns
  massExpCols <- c("domainID", "siteID", "plotID", "plotType", "sampleCondition", "herbGroup", "dryMass", "qaDryMass")

  if (length(setdiff(massExpCols, colnames(inputMass))) > 0) {
    stop(glue::glue("Required columns missing from 'inputMass':", '{paste(setdiff(massExpCols, colnames(inputMass)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(inputMass) == 0) {
    stop(glue::glue("Table 'inputMass' has no data."))
  }

  #   Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "tower", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'tower', or 'distributed'.")
  }

  # #   Assign plotType from user-supplied plotSubset
  # plotType <- dplyr::case_when(plotSubset == "tower" ~ "tower",
  #                              plotSubset == "all" ~ "all",
  #                              TRUE ~ "distributed")



  ###  Generate final columns needed in 'inputBout' data frame
  #   Reduce 'hbp_perbout' columns to subset needed for join
  inputBout <- inputBout %>%
    dplyr::select("namedLocation",
                  "domainID",
                  "siteID",
                  "plotID",
                  "subplotID",
                  "clipID",
                  "nlcdClass",
                  "plotType",
                  "plotSize",
                  "plotManagement",
                  "collectDate",
                  "eventID",
                  "sampleID",
                  "clipArea",
                  "exclosure")

  #   Create 'year' column for grouping output at plot and site scales across data products
  inputBout <- dplyr::mutate(inputBout,
                             year = as.numeric(substr(.data$eventID, start = 5, stop = 8)),
                             .before = "eventID")



  ### Aggregate the herbaceous data ####

  #   Assign 'unknown' to missing herbGroup
  inputMass$herbGroup <- ifelse(is.na(inputMass$herbGroup),
                                "Unknown",
                                inputMass$herbGroup)

  #   Calculate the average of any 'qaDryMass' replicates or duplicates within 'herbGroup'
  inputMass <- inputMass  %>%
    dplyr::filter((.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical)) & .data$herbGroup != "Bryophyte") %>%
    dplyr::group_by(.data$sampleID,
                    .data$subsampleID,
                    .data$herbGroup) %>%
    dplyr::summarise(dryMass = mean(.data$dryMass, na.rm = TRUE),
                     .groups = "drop")



  #   Join tables and calculate mass per area
  hbp <- dplyr::right_join(inputBout,
                           inputMass,
                           by = "sampleID") %>%
    dplyr::mutate(dryMass_gm2 = .data$dryMass / .data$clipArea)

  #   Categorize mass as "peak" biomass or "offPeak"
  hbp$peak <- ifelse(hbp$herbGroup == "All herbaceous plants",
                     "offPeak",
                     "atPeak")

  #   Standardize herbGroups to remove spaces and hyphens
  hbp$herbGroup <- gsub(pattern = "All herbaceous plants",
                        replacement = "AllHerbaceousPlants",
                        hbp$herbGroup)

  hbp$herbGroup <- gsub(pattern = "Cool Season Graminoids",
                        replacement = "CoolSeasonGraminoids",
                        hbp$herbGroup)

  hbp$herbGroup <- gsub(pattern = "Woody-stemmed Plants",
                        replacement = "WoodyStemmedPlants",
                        hbp$herbGroup)

  hbp$herbGroup <- gsub(pattern = "Warm Season Graminoids",
                        replacement = "WarmSeasonGraminoids",
                        hbp$herbGroup)

  hbp$herbGroup <- gsub(pattern = "N-fixing Plants",
                        replacement = "NFixingPlants",
                        hbp$herbGroup)

  hbp$herbGroup <- gsub(pattern = "Annual and Perennial Forbs",
                        replacement = "AnnualAndPerennialForbs",
                        hbp$herbGroup)

  #   Transpose herbGroup rows into separate columns
  hbp_wide <- tidyr::pivot_wider(data = hbp,
                                 id_cols = c("namedLocation",
                                             "domainID",
                                             "siteID",
                                             "plotID",
                                             "subplotID",
                                             "clipID",
                                             "nlcdClass",
                                             "plotType",
                                             "plotSize",
                                             "plotManagement",
                                             "collectDate",
                                             "eventID",
                                             "sampleID",
                                             "clipArea",
                                             "exclosure",
                                             "peak"),
                                 names_from = "herbGroup",
                                 names_glue = "{herbGroup}_gm2",
                                 values_from = "dryMass_gm2")

  #   Aggregate dryMass among herbGroups in peak biomass bouts
  hbp_peak_biomass_herb_groups <- hbp %>%
    dplyr::filter(.data$herbGroup != "AllHerbaceousPlants") %>%
    dplyr::mutate(peak = "atPeak")

  hbp_peak_biomass_sum_groups <- hbp_peak_biomass_herb_groups %>%
    dplyr::group_by(.data$sampleID) %>%
    dplyr::summarise(dryMassSum = sum(.data$dryMass_gm2))

  #   Populate "AllHerbaceousPlants" column for peak biomass bouts
  hbp2 <- merge(hbp_wide,
                hbp_peak_biomass_sum_groups,
                by = "sampleID",
                all.x = TRUE)

  if(!"AllHerbaceousPlants_gm2" %in% names(hbp2)) {hbp2[["AllHerbaceousPlants_gm2"]] <- NA}


  hbp2$AllHerbaceousPlants_gm2 <- ifelse(is.na(hbp2$AllHerbaceousPlants_gm2),
                                                 hbp2$dryMassSum,
                                                 hbp2$AllHerbaceousPlants_gm2)



  ### Cell-level output: Finalize data frame ####
  #   Separate "eventID" into components, relocate and remove columns, set "year" data type, arrange
  hbp_standing_biomass_in_clip_cells <- hbp2 %>%
    dplyr::select(-"dryMassSum") %>%
    tidyr::separate(col = "eventID",
                    into = c("data_prod", "year", "siteID2", "bout"),
                    sep = "\\.",
                    remove = FALSE,
                    extra = "drop") %>%
    dplyr::relocate("sampleID",
                    .after = "peak") %>%
    dplyr::relocate("year",
                    .before = "collectDate") %>%
    dplyr::select(-"siteID2",
                  -"bout") %>%
    dplyr::mutate(year = as.numeric(.data$year)) %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID,
                   .data$clipID)



  ### Filter by plotSubset
  if (plotSubset %in% c("distributed", "tower")) {

    hbp_standing_biomass_in_clip_cells <- hbp_standing_biomass_in_clip_cells %>%
      dplyr::filter(.data$plotType == plotSubset)

  }



  ### Plot-level output: Calculate plot-level means by year ####

  ##  Calculate plot-level peak biomass
  #   Requires filtering out exclosure == "Y"

  hbp_plot <- hbp_standing_biomass_in_clip_cells %>%
    dplyr::filter(.data$peak == "atPeak" & .data$exclosure == "N") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$year,
                    .data$nlcdClass) %>%

    #   Calculate average per plot if there are multiple subplots
    dplyr::summarise(
      herbPeakMassTotal_gm2 = round(mean(.data$AllHerbaceousPlants_gm2, na.rm = TRUE),
                                    digits = 2),
      herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$CoolSeasonGraminoids_gm2, na.rm = TRUE),
                                                   digits = 2),
      herbPeakMassWoodyStemmedPlants_gm2 = round(mean(.data$WoodyStemmedPlants_gm2, na.rm = TRUE),
                                                 digits = 2),
      herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$CoolSeasonGraminoids_gm2, na.rm = TRUE),
                                                   digits = 2),
      herbPeakMassWarmSeasonGraminoids = round(mean(.data$WarmSeasonGraminoids_gm2, na.rm = TRUE),
                                               digits = 2),
      herbPeakMassNFixingPlants_gm2 = round(mean(.data$NFixingPlants_gm2, na.rm = TRUE),
                                            digits = 2),
      herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$CoolSeasonGraminoids_gm2, na.rm = TRUE),
                                                   digits = 2),
      herbPeakMassAnnualAndPerennialForbs_gm2 = round(mean(.data$AnnualAndPerennialForbs_gm2, na.rm = TRUE),
                                                      digits = 2),
      .groups = "drop") %>%

    #   Calculate "Mg/ha" for total herbaceous peak biomass; g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
    dplyr::mutate(herbPeakMassTotal_Mgha = round(.data$herbPeakMassTotal_gm2 * 10000 * 0.000001,
                                                 digits = 2),
                  .before = "herbPeakMassTotal_gm2")



  #   Load 'priority_plots' data frame into environment from 'data' folder and merge with plot-level data
  priority_plots <- priority_plots

  hbp_plot <- dplyr::left_join(hbp_plot,
                               priority_plots %>%
                                 dplyr::select("plotID",
                                               "plotType"),
                               by = "plotID") %>%
    dplyr::relocate("plotType",
                    .before = "nlcdClass")

  # hbp_plot <- merge(hbp_plot,
  #                   priority_plots,
  #                   by = c("plotID"),
  #                   all.x = TRUE) #--> re-work to just bring in "plotType", and relocate before 'nlcdClass'
  #
  #
  # #   Remove plots that do not meet 'priority' thresholds if 'plotPriority' supplied
  #    hbp_plot <- hbp_plot %>%
  #     dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)

  # #   Reorder columns and keep latest year
  # hbp_plot <- hbp_plot %>%
  #   dplyr::relocate("herbPeakMassTotal_gm2",
  #                   .before = "herbPeakMassTotal_Mgha")

  # hbp_plot <- hbp_plot %>%
  #   dplyr::relocate(dplyr::any_of(c("plotType", "specificModuleSamplingPriority")),
  #                   .after = "nlcdClass")

  # hbp_plot$year <- as.numeric(hbp_plot$year)
  # hbp_plot <- hbp_plot[order(hbp_plot$year), ] #--> test and make sure this doesn't filter out any years



  ### Calculate site-level peak biomass by year ####
  hbp_site <- hbp_plot %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::summarise(herbPlotNum = length(stats::na.omit(.data$herbPeakMassTotal_gm2)),
                     herbPeakMassMean_gm2 = round(mean(.data$herbPeakMassTotal_gm2, na.rm = TRUE),
                                                  digits = 3),
                     herbPeakMassSD_gm2 = round(stats::sd(.data$herbPeakMassTotal_gm2, na.rm = TRUE),
                                                digits = 2),
                     herbPeakMassMean_Mgha = round(mean(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),
                                                   digits = 3),
                     herbPeakMassSD_Mgha = round(stats::sd(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),
                                                 digits = 2),
                     .groups = "drop")



  ### Bundle output as list and return
  output.list <- list(hbp_agb = hbp_standing_biomass_in_clip_cells,
                      hbp_plot = hbp_plot,
                      hbp_site = hbp_site)

  return(output.list)
}
