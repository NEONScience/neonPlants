#' @title Scale herbaceous biomass by functional group data to mass per area
#'
#' @author
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Join NEON Herbaceous Clip Harvest data tables (DP1.10023.001) to calculate herbaceous biomass by functional group per unit area, as well as total herbaceous biomass per unit area, at spatial scales of the sampling cell, plot, and site. Biomass outputs can be used with the estimateHerbProd() productivity function.
#'
#' Data inputs are "Herbaceous clip harvest" data (DP1.10023.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' @details Input data can be filtered by plot subset. Herbaceous biomass data are scaled to an area basis at the hierarchical levels of sampling cell, plot, and site. Input data may be provided either as a list or as individual tables. However, if both list and table inputs are provided at the same time the function will error out. For all output data, columns with the same name as input data have identical units and definitions; where needed, new columns contain new units information.
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
#' @param plotSubset The options are the default of "all" (all Tower and Distributed plots), "tower" (all plots in the Tower airshed but no Distributed plots), and "distributed" (all Distributed plots, which are sampled on a 5-year interval and are spatially representative of the NLCD classes at a site, and no Tower plots). [character]
#'
#' @return A list that includes biomass summary data at multiple scales. Output tables include:
#'   * hbp_agb - Above-ground herbaceous biomass for each sampled clip strip in the input data ("g/m2").
#'   * hbp_plot - Plot-level above-ground herbaceous peak standing biomass for all sites not planted with crops in a given year (both "g/m2" and "Mg/ha").
#'   * hbp_plot_extra - Above-ground herbaceous peak standing biomass for plots at grazed sites that were not subject to grazing management (i.e., the Tower plots at a grazed sites were not all managed for grazing) and with peak biomass occurring in a different eventID than the peak biomass eventID associated with the grazed plots (both "g/m2" and "Mg/ha").
#'   * hbp_plot_crop - Above-ground herbaceous peak standing biomass for plots at sites planted with an agricultural crop in at least one plot in a given year (both "g/m2" and "Mg/ha"). Peak biomass is reported on a per plot basis and it is not assumed there is a single sampling eventID that represents "peak biomass".
#'   * hbp_site - Above-ground herbaceous peak standing biomass for each site by year combination (both "g/m2" and "Mg/ha"). Output is derived from the single sampling eventID with the greatest biomass and does not include sites planted with agricultural crops. At grazed sites, the peak biomass estimate does not include those Tower plots not managed for grazing AND that achieve peak biomass in a different eventID than that identified for grazed plots.
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
                         plotSubset = "all") {

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
  boutExpCols <- c("domainID", "siteID", "plotID", "subplotID", "clipID", "nlcdClass", "plotType", "plotSize", "plotManagement", "collectDate", "eventID", "samplingImpractical", "targetTaxaPresent", "sampleID", "clipArea", "exclosure")

  if (length(setdiff(boutExpCols, colnames(inputBout))) > 0) {
    stop(glue::glue("Required columns missing from 'inputBout':", '{paste(setdiff(boutExpCols, colnames(inputBout)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (!nrow(inputBout)) {
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
  if (!nrow(inputMass)) {
    stop(glue::glue("Table 'inputMass' has no data."))
  }



  ### Error if invalid plotSubset option provided
  if (!plotSubset %in% c("all", "tower", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'tower', or 'distributed'.")
  }






  ### Prepare input data frame from 'inputBout' and 'inputMass ####

  ### Prepare 'inputBout' data frame

  ##  Remove exclosure == "Y" records at sites where exclosures were trialed by not used (SRER) and where exclosure == "Y" was mistakenly selected
  exclosureFilter <- c("SERC", "JERC", "OSBS", "TREE", "UKFS", "JORN", "SRER", "TEAK")

  inputBout <- inputBout %>%
    dplyr::filter(!(.data$siteID %in% exclosureFilter & .data$exclosure == "Y") | is.na(.data$exclosure))

  #   At TEAK, remove bouts where only one plot was sampled when exclosure == "Y"; remove all data from these bouts
  teakRemove <- c("HBP.2019.TEAK.02.TOWER",
                  "HBP.2019.TEAK.03.TOWER",
                  "HBP.2019.TEAK.04.TOWER",
                  "HBP.2021.TEAK.23.TOWER",
                  "HBP.2021.TEAK.27.TOWER",
                  "HBP.2021.TEAK.35.TOWER",
                  "HBP.2021.TEAK.43.TOWER")

  inputBout <- inputBout %>%
    dplyr::filter(!.data$eventID %in% teakRemove)


  ##  Reduce 'hbp_perbout' columns to subset needed for join
  inputBout <- inputBout %>%
    dplyr::select("domainID",
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
                  "samplingImpractical",
                  "targetTaxaPresent",
                  "sampleID",
                  "clipArea",
                  "exclosure")


  ##  Set date data type and create 'year' column for grouping output at plot and site scales across data products; 'year' is updated at SJER due to Mediterranean growing season spanning a calendar year.
  inputBout <- inputBout %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(string = .data$eventID,
                                                         pattern = "20[0-9]{2}")),
                  .before = "eventID") %>%

    #   Assign collectDate data type and update 'year' at SJER
    dplyr::mutate(collectDate = as.Date(.data$collectDate),
                  year = dplyr::case_when(.data$siteID == "SJER" &
                                            .data$collectDate < as.Date(glue::glue("{.data$year}-07-15")) ~
                                            (.data$year - 1),
                                          TRUE ~ .data$year))



  ### Prep data for downstream calculations
  ##  Join data frames
  hbp <- dplyr::full_join(inputBout,
                          inputMass %>%
                            dplyr::filter((.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical)) &
                                            .data$herbGroup != "Bryophyte") %>%
                            dplyr::select("sampleID",
                                          "subsampleID",
                                          "herbGroup",
                                          "dryMass",
                                          "qaDryMass"),
                          by = "sampleID")

  #   Find orphaned hbp_massdata records, remove from 'hbp'; mass data records are expected to be orphaned at some sites due to inputBout filtering above, but orphaned records may also exist in input data (though unlikely)
  hbp <- hbp %>%
    dplyr::filter(!is.na(.data$plotID) & !is.na(.data$eventID) & !is.na(.data$collectDate))


  ##  Filter by user-supplied plotSubset
  if (plotSubset %in% c("distributed", "tower")) {

    hbp <- hbp %>%
      dplyr::filter(.data$plotType == plotSubset)

  }


  ##  Calculate the average of any 'qaDryMass' replicates or duplicates within 'herbGroup', scale 'dryMass' to "g/m2" and assign a 0 when tTP = N, and remove spaces and hyphens from 'herbGroup' values

  if (!nrow(hbp)) {

    stop("No data available for specified 'plotSubset'")

  } else {

    hbp <- hbp %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$subplotID,
                      .data$clipID,
                      .data$nlcdClass,
                      .data$plotType,
                      .data$plotSize,
                      .data$plotManagement,
                      .data$collectDate,
                      .data$year,
                      .data$eventID,
                      .data$samplingImpractical,
                      .data$targetTaxaPresent,
                      .data$sampleID,
                      .data$clipArea,
                      .data$exclosure,
                      .data$subsampleID,
                      .data$herbGroup) %>%
      dplyr::summarise(dryMass = dplyr::case_when(all(is.na(.data$dryMass)) ~ NA,
                                                  TRUE ~ mean(.data$dryMass, na.rm = TRUE)),
                       .groups = "drop") %>%

      dplyr::mutate(dryMass = dplyr::case_when(.data$targetTaxaPresent == "N" ~ 0,
                                               TRUE ~ round(.data$dryMass, digits = 2)),
                    dryMass_gm2 = dplyr::case_when(.data$targetTaxaPresent == "N" ~ 0,
                                                   TRUE ~ round(.data$dryMass / .data$clipArea, digits = 2))) %>%

      dplyr::mutate(herbGroup = dplyr::case_when(.data$herbGroup == "All herbaceous plants" ~ "TotalMass",
                                                 .data$herbGroup == "Cool Season Graminoids" ~ "CoolSeasonGram",
                                                 .data$herbGroup == "Woody-stemmed Plants" ~ "WoodyPlants",
                                                 .data$herbGroup == "Warm Season Graminoids" ~ "WarmSeasonGram",
                                                 .data$herbGroup == "Leguminous Forbs" ~ "NFixing",
                                                 .data$herbGroup == "N-fixing Plants" ~ "NFixing",
                                                 .data$herbGroup == "Annual and Perennial Forbs" ~ "Forbs",
                                                 .data$herbGroup == "Orchard Grass" ~ "OrchardGrass",
                                                 TRUE ~ .data$herbGroup))


    ##  Identify sampleIDs containing crop functional groups; easiest to do here when only 'herbGroup' column to consider before the table pivots wider below.
    #   Define all possible crop herbGroups
    allCrops <- c("Barley",
                  "Corn",
                  "Millet",
                  "Oat",
                  "OrchardGrass",
                  "Rye",
                  "Sorghum",
                  "Soybean",
                  "Sunflower",
                  "Wheat")

    #   Determine which sampleIDs contain crops; retain these in separate data frame to enable accurate plot-level peak biomass calculation; create 'siteYear' variable to identify all records in a site-year where cropping occurred (further down).
    cropPlotsDF <- hbp %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$subplotID,
                      .data$plotType,
                      .data$plotManagement,
                      .data$year,
                      .data$eventID,
                      .data$sampleID) %>%
      dplyr::filter(any(.data$herbGroup %in% allCrops)) %>%
      dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"),
                    .before = "plotID")


    ##  Transpose herbGroup rows into separate columns to create one row per clipID
    clipDF <- tidyr::pivot_wider(data = hbp,
                                 id_cols = c("domainID",
                                             "siteID",
                                             "plotID",
                                             "subplotID",
                                             "clipID",
                                             "nlcdClass",
                                             "plotType",
                                             "plotSize",
                                             "plotManagement",
                                             "collectDate",
                                             "year",
                                             "eventID",
                                             "targetTaxaPresent",
                                             "sampleID",
                                             "clipArea",
                                             "exclosure"),
                                 names_from = "herbGroup",
                                 names_glue = "{herbGroup}_gm2",
                                 values_from = "dryMass_gm2") %>%
      dplyr::relocate("TotalMass_gm2",
                      "NA_gm2",
                      .after = "exclosure")


    ##  Add columns for missing herbGroups; can occur when a small number of sites are used and not all herbGroups present at sites (especially crops).
    allHerbGroups <- c("TotalMass_gm2",
                       "Forbs_gm2",
                       "CoolSeasonGram_gm2",
                       "NFixing_gm2",
                       "WarmSeasonGram_gm2",
                       "WoodyPlants_gm2",
                       "Barley_gm2",
                       "Corn_gm2",
                       "Millet_gm2",
                       "Oat_gm2",
                       "OrchardGrass_gm2",
                       "Rye_gm2",
                       "Sorghum_gm2",
                       "Soybean_gm2",
                       "Sunflower_gm2",
                       "Wheat_gm2")

    for (i in 1:length(allHerbGroups)) {

      if (!allHerbGroups[i] %in% names(clipDF)) {

        clipDF[, allHerbGroups[i]] <- NA

      }
    }


    ##  Calculate "TotalMass" biomass for sampling events sorted to herbGroup
    clipDF <- clipDF %>%
      dplyr::mutate(TotalMass_gm2 = dplyr::case_when(is.na(.data$TotalMass_gm2) ~
                                                       rowSums(dplyr::across("NA_gm2":"Wheat_gm2"),
                                                               na.rm = TRUE),
                                                     TRUE ~ .data$TotalMass_gm2)) %>%
      dplyr::select(-"NA_gm2") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$eventID,
                     .data$plotID,
                     .data$subplotID,
                     .data$clipID)

  } #   End !nrow() conditional



  ### Cell-level output: Finalize data frame ####
  #   Assign data types to herbGroup_gm2 columns; can be "logi" if an herbGroup is absent from the dataset for selected sites
  clipDF <- clipDF %>%
    dplyr::mutate(CoolSeasonGram_gm2 = as.numeric(.data$CoolSeasonGram_gm2),
                  Forbs_gm2 = as.numeric(.data$Forbs_gm2),
                  NFixing_gm2 = as.numeric(.data$NFixing_gm2),
                  WarmSeasonGram_gm2 = as.numeric(.data$WarmSeasonGram_gm2),
                  WoodyPlants_gm2 = as.numeric(.data$WoodyPlants_gm2),
                  Barley_gm2 = as.numeric(.data$Barley_gm2),
                  Corn_gm2 = as.numeric(.data$Corn_gm2),
                  Millet_gm2 = as.numeric(.data$Millet_gm2),
                  Oat_gm2 = as.numeric(.data$Oat_gm2),
                  OrchardGrass_gm2 = as.numeric(.data$OrchardGrass_gm2),
                  Rye_gm2 = as.numeric(.data$Rye_gm2),
                  Sorghum_gm2 = as.numeric(.data$Sorghum_gm2),
                  Soybean_gm2 = as.numeric(.data$Soybean_gm2),
                  Sunflower_gm2 = as.numeric(.data$Sunflower_gm2),
                  Wheat_gm2 = as.numeric(.data$Wheat_gm2)) %>%
    dplyr::relocate("Barley_gm2",
                    "Corn_gm2",
                    "Millet_gm2",
                    "Oat_gm2",
                    "OrchardGrass_gm2",
                    "Rye_gm2",
                    "Sorghum_gm2",
                    "Soybean_gm2",
                    "Sunflower_gm2",
                    "Wheat_gm2",
                    .after = "WoodyPlants_gm2")






  ### Plot-level output: Calculate plot-level mean peak herbaceous biomass by year ####

  ### Separate records into grazed, cropped, and standard 'siteID x year' data frames; each requires custom logic to calculate peak standing biomass at the plot level.

  ##  Step 1: Identify records in all 'siteID x year' combinations that supported grazing
  grazedSiteYearDF <- clipDF %>%
    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"),
                  siteYearPlot = paste(.data$siteID, .data$year, .data$plotID, sep = "-"),
                  .before = "plotID") %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$exclosure == "Y"))


  ##  Step 2: Identify records for all 'siteID x year' combinations that contained crops in a plot at any point
  cropSiteYearDF <- clipDF %>%
    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"),
                  .before = "plotID") %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$siteYear %in% cropPlotsDF$siteYear))


  ##  Step 3: Identify all "standard" clips - i.e., no grazing, no crops at any point in a 'siteID x year'
  stdSiteYearDF <- clipDF %>%
    dplyr::filter(!.data$sampleID %in% grazedSiteYearDF$sampleID,
                  !.data$sampleID %in% cropSiteYearDF$sampleID)



  ### Standard sites: Identify peak biomass bout for sites with > 1 bout per year and no grazing; returns single bout for sites with only one bout; Distributed and Tower plots processed separately then combined for output

  ##  Distributed plots
  distStdDF <- stdSiteYearDF %>%
    dplyr::filter(.data$plotType == "distributed")


  ##  Tower plots
  #   For each 'siteID x year', determine peak biomass eventID
  temp <- stdSiteYearDF %>%
    dplyr::filter(.data$plotType == "tower") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID) %>%
    dplyr::summarise(MeanBiomass_gm2 = mean(.data$TotalMass_gm2, na.rm = TRUE),
                     count = n(),
                     .groups = "drop")

  temp <- temp %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::filter(MeanBiomass_gm2 == max(.data$MeanBiomass_gm2, na.rm = TRUE))

  #   Reduce Tower plot data to plots from peak biomass eventID; calculate mean biomass for 40m x 40m Tower plots
  towerStdDF <- stdSiteYearDF %>%
    dplyr::filter(.data$plotType == "tower",
                  .data$eventID %in% temp$eventID) %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$plotID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$plotSize,
                    .data$plotManagement) %>%
    dplyr::summarise(collectDate = min(.data$collectDate),
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
                                             digits = 2),
                     Barley_gm2 = round(mean(.data$Barley_gm2, na.rm = TRUE),
                                        digits = 2),
                     Corn_gm2 = round(mean(.data$Corn_gm2, na.rm = TRUE),
                                      digits = 2),
                     Millet_gm2 = round(mean(.data$Millet_gm2, na.rm = TRUE),
                                        digits = 2),
                     Oat_gm2 = round(mean(.data$Oat_gm2, na.rm = TRUE),
                                     digits = 2),
                     OrchardGrass_gm2 = round(mean(.data$OrchardGrass_gm2, na.rm = TRUE),
                                              digits = 2),
                     Rye_gm2 = round(mean(.data$Rye_gm2, na.rm = TRUE),
                                     digits = 2),
                     Sorghum_gm2 = round(mean(.data$Sorghum_gm2, na.rm = TRUE),
                                         digits = 2),
                     Soybean_gm2 = round(mean(.data$Soybean_gm2, na.rm = TRUE),
                                         digits = 2),
                     Sunflower_gm2 = round(mean(.data$Sunflower_gm2, na.rm = TRUE),
                                           digits = 2),
                     Wheat_gm2 = round(mean(.data$Wheat_gm2, na.rm = TRUE),
                                       digits = 2),
                     .groups = "drop")


  ##  Create plot-level peak biomass data frame for "standard" sites
  stdPlotPeakDF <- dplyr::full_join(distStdDF,
                                    towerStdDF) %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotType,
                   .data$eventID,
                   .data$plotID)

  rm(distStdDF, temp, towerStdDF)



  ### Grazed sites: Identify peak biomass, accounting for the fact that some Tower plots at grazed sites are not grazed

  ##  Distributed plots
  distGrazedDF <- grazedSiteYearDF %>%
    dplyr::filter(.data$plotType == "distributed")


  ##  Tower plots
  #   For each 'siteID x year', find plots that were grazed (exclosure == "Y") at some point in the year and plots that were never grazed in that year; assumes plots with an exclosure = Y clip were grazed for the entire year which may not be correct but it is not possible to tell when a plot with no exclosures goes in/out of grazing management within a year since exclosures can be damaged and the plot still grazed.
  tempGrazed <- grazedSiteYearDF %>%
    dplyr::filter(.data$plotType == "tower") %>%
    dplyr::group_by(.data$siteYearPlot) %>%
    dplyr::filter(any(.data$exclosure == "Y")) %>%
    dplyr::ungroup()

  #   For grazed plots at grazed sites, determine peak biomass eventID; filter out exclosure == "Y" as these clips are not relevant for understanding peak biomass
  temp <- tempGrazed %>%
    dplyr::filter(.data$exclosure != "Y") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID) %>%
    dplyr::summarise(MeanBiomass_gm2 = mean(.data$TotalMass_gm2, na.rm = TRUE),
                     count = n(),
                     .groups = "drop")

  temp <- temp %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::filter(MeanBiomass_gm2 == max(.data$MeanBiomass_gm2, na.rm = TRUE))

  #   Find all Tower plots associated with peak biomass eventID, grazed or ungrazed
  towerGrazedDF <- grazedSiteYearDF %>%
    dplyr::filter(.data$plotType == "tower",
                  .data$eventID %in% temp$eventID,
                  .data$exclosure != "Y") %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$plotID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$plotSize,
                    .data$plotManagement) %>%
    dplyr::summarise(collectDate = min(.data$collectDate),
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
                                             digits = 2),
                     Barley_gm2 = round(mean(.data$Barley_gm2, na.rm = TRUE),
                                        digits = 2),
                     Corn_gm2 = round(mean(.data$Corn_gm2, na.rm = TRUE),
                                      digits = 2),
                     Millet_gm2 = round(mean(.data$Millet_gm2, na.rm = TRUE),
                                        digits = 2),
                     Oat_gm2 = round(mean(.data$Oat_gm2, na.rm = TRUE),
                                     digits = 2),
                     OrchardGrass_gm2 = round(mean(.data$OrchardGrass_gm2, na.rm = TRUE),
                                              digits = 2),
                     Rye_gm2 = round(mean(.data$Rye_gm2, na.rm = TRUE),
                                     digits = 2),
                     Sorghum_gm2 = round(mean(.data$Sorghum_gm2, na.rm = TRUE),
                                         digits = 2),
                     Soybean_gm2 = round(mean(.data$Soybean_gm2, na.rm = TRUE),
                                         digits = 2),
                     Sunflower_gm2 = round(mean(.data$Sunflower_gm2, na.rm = TRUE),
                                           digits = 2),
                     Wheat_gm2 = round(mean(.data$Wheat_gm2, na.rm = TRUE),
                                       digits = 2),
                     .groups = "drop")


  ##  Create plot-level peak biomass data frame for "grazed" sites
  grazedPlotPeakDF <- dplyr::full_join(distGrazedDF,
                                       towerGrazedDF) %>%
    dplyr::select(-"siteYear",
                  -"siteYearPlot") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotType,
                   .data$eventID,
                   .data$plotID)


  ##  Create plot-level peak biomass for "wild" type plots at grazed sites that were not sampled in the same peak biomass eventID as the grazed plots; these plots reported separately.
  #   Identify peak biomass bout for "wild" type plots at grazed sites

  grazedWildDF <- grazedSiteYearDF %>%
    dplyr::filter(.data$plotType == "tower",
                  !.data$plotID %in% grazedPlotPeakDF$plotID,
                  .data$exclosure != "Y")

  temp <- grazedWildDF %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID) %>%
    dplyr::summarise(MeanBiomass_gm2 = mean(.data$TotalMass_gm2, na.rm = TRUE),
                     count = n(),
                     .groups = "drop")

  temp <- temp %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::filter(MeanBiomass_gm2 == max(MeanBiomass_gm2, na.rm = TRUE))

  #   Isolate records for peak biomass "wild" type plots that were not sampled in the peak grazed biomass bout
  grazedWildDF <- grazedWildDF %>%
    dplyr::filter(.data$eventID %in% temp$eventID) %>%
    dplyr::select(-"siteYear",
                  -"siteYearPlot") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$eventID,
                   .data$plotID)


  ##  Grazed cleanup
  rm(temp, distGrazedDF, towerGrazedDF, tempGrazed)



  ### Cropped sites: Identify peak biomass on a per plot basis and report along with the associated eventID.
  #--> Note: Distibuted plots are sampled annually at Ag sites, so all Ag site plots are considered together.

  #   Calculate plot-level average per eventID to account for sites like JERC where crops may be planted in 40m x 40m Tower plots.
  cropDF <- cropSiteYearDF %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$plotID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$plotSize,
                    .data$plotManagement) %>%
    dplyr::summarise(collectDate = min(.data$collectDate),
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
                                                    digits = 2),
                     Barley_gm2 = round(mean(.data$Barley_gm2, na.rm = TRUE),
                                        digits = 2),
                     Corn_gm2 = round(mean(.data$Corn_gm2, na.rm = TRUE),
                                      digits = 2),
                     Millet_gm2 = round(mean(.data$Millet_gm2, na.rm = TRUE),
                                        digits = 2),
                     Oat_gm2 = round(mean(.data$Oat_gm2, na.rm = TRUE),
                                     digits = 2),
                     OrchardGrass_gm2 = round(mean(.data$OrchardGrass_gm2, na.rm = TRUE),
                                              digits = 2),
                     Rye_gm2 = round(mean(.data$Rye_gm2, na.rm = TRUE),
                                     digits = 2),
                     Sorghum_gm2 = round(mean(.data$Sorghum_gm2, na.rm = TRUE),
                                         digits = 2),
                     Soybean_gm2 = round(mean(.data$Soybean_gm2, na.rm = TRUE),
                                         digits = 2),
                     Sunflower_gm2 = round(mean(.data$Sunflower_gm2, na.rm = TRUE),
                                           digits = 2),
                     Wheat_gm2 = round(mean(.data$Wheat_gm2, na.rm = TRUE),
                                       digits = 2),
                     .groups = "drop")


  ##  Calculate peak biomass per plot without regard to eventID
  cropDF <- cropDF %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$plotID) %>%
    dplyr::filter(TotalMass_gm2 == max(.data$TotalMass_gm2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::relocate("eventID",
                    .before = "collectDate") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID)



  ### Plot-level data frames: Clean up for output

  ##  Clean and combine "standard" and "grazed" plot-level peak biomass
  plotDF <- dplyr::full_join(grazedPlotPeakDF,
                           stdPlotPeakDF) %>%

    #   Remove columns not relevant to plot-level peak biomass output
    dplyr::select(-"subplotID",
                  -"clipID",
                  -"targetTaxaPresent",
                  -"sampleID",
                  -"clipArea",
                  -"exclosure",
                  -"Barley_gm2",
                  -"Corn_gm2",
                  -"Millet_gm2",
                  -"Oat_gm2",
                  -"OrchardGrass_gm2",
                  -"Rye_gm2",
                  -"Sorghum_gm2",
                  -"Soybean_gm2",
                  -"Sunflower_gm2",
                  -"Wheat_gm2") %>%

    #   Replace "NaN" with NA
    dplyr::mutate(dplyr::across("TotalMass_gm2":"WoodyPlants_gm2", ~dplyr::na_if(., NaN))) %>%

    #   Rename columns with "herb" prefix and simplify
    dplyr::rename("herbTotalMass_gm2" = "TotalMass_gm2",
                  "herbCoolSeasonGram_gm2" = "CoolSeasonGram_gm2",
                  "herbForbs_gm2" = "Forbs_gm2",
                  "herbNFixing_gm2" = "NFixing_gm2",
                  "herbWarmSeasonGram_gm2" = "WarmSeasonGram_gm2",
                  "herbWoodyPlants_gm2" = "WoodyPlants_gm2") %>%

    #   Calculate "Mg/ha" for total herbaceous peak biomass; g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
    dplyr::mutate(herbTotalMass_Mgha = round(.data$herbTotalMass_gm2 * 10000 * 0.000001,
                                             digits = 2),
                  .before = "herbTotalMass_gm2")


  ##  Clean "wild-type" peak biomass plot data at grazed sites
  grazedWildDF <- grazedWildDF %>%
    dplyr::select(-"subplotID",
                  -"clipID",
                  -"targetTaxaPresent",
                  -"sampleID",
                  -"clipArea",
                  -"exclosure",
                  -("Barley_gm2":"Wheat_gm2")) %>%
    dplyr::rename("herbTotalMass_gm2" = "TotalMass_gm2",
                  "herbCoolSeasonGram_gm2" = "CoolSeasonGram_gm2",
                  "herbForbs_gm2" = "Forbs_gm2",
                  "herbNFixing_gm2" = "NFixing_gm2",
                  "herbWarmSeasonGram_gm2" = "WarmSeasonGram_gm2",
                  "herbWoodyPlants_gm2" = "WoodyPlants_gm2") %>%
    dplyr::mutate(herbTotalMass_Mgha = round(.data$herbTotalMass_gm2 * 10000 * 0.000001,
                                             digits = 2),
                  .before = "herbTotalMass_gm2")


  ##  Clean "cropped" plot-level peak biomass output
  cropDF <- cropDF %>%

    #   Replace "NaN" with NA
    dplyr::mutate(dplyr::across("TotalMass_gm2":"Wheat_gm2", ~dplyr::na_if(., NaN))) %>%

    #   Rename columns with "herb" prefix and simplify
    dplyr::rename("herbTotalMass_gm2" = "TotalMass_gm2",
                  "herbCoolSeasonGram_gm2" = "CoolSeasonGram_gm2",
                  "herbForbs_gm2" = "Forbs_gm2",
                  "herbNFixing_gm2" = "NFixing_gm2",
                  "herbWarmSeasonGram_gm2" = "WarmSeasonGram_gm2",
                  "herbWoodyPlants_gm2" = "WoodyPlants_gm2") %>%

    #   Calculate "Mg/ha" for total herbaceous peak biomass; g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
    dplyr::mutate(herbTotalMass_Mgha = round(.data$herbTotalMass_gm2 * 10000 * 0.000001,
                                             digits = 2),
                  .before = "herbTotalMass_gm2")







  ### Site-level output: Calculate site-level mean peak herbaceous biomass by year ####
  siteDF <- plotDF %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::summarise(herbPlotNum = length(stats::na.omit(.data$herbTotalMass_gm2)),
                     herbPlotType = dplyr::case_when(dplyr::n_distinct(.data$plotType, na.rm = TRUE) == 1 ~
                                                       paste(unique(.data$plotType), collapse = ", "),
                                                     TRUE ~ paste(unique(.data$plotType), collapse = ", ")),
                     herbStartDate = min(.data$collectDate),
                     herbEndDate = max(.data$collectDate),
                     herbTotalMean_Mgha = round(mean(.data$herbTotalMass_Mgha, na.rm = TRUE),
                                                digits = 2),
                     herbTotalSD_Mgha = round(stats::sd(.data$herbTotalMass_Mgha, na.rm = TRUE),
                                              digits = 2),
                     herbTotalSEM_Mgha = round(stats::sd(.data$herbTotalMass_Mgha, na.rm = TRUE) / sqrt(.data$herbPlotNum),
                                               digits = 2),
                     herbTotalMean_gm2 = round(mean(.data$herbTotalMass_gm2, na.rm = TRUE),
                                               digits = 2),
                     herbTotalSD_gm2 = round(stats::sd(.data$herbTotalMass_gm2, na.rm = TRUE),
                                             digits = 2),
                     herbTotalSEM_gm2 = round(stats::sd(.data$herbTotalMass_gm2, na.rm = TRUE) / sqrt(.data$herbPlotNum),
                                              digits = 2),
                     .groups = "drop")






  ### Return results: Bundle output as list and return ####

  output <- list(hbp_agb = clipDF,
                 hbp_plot = plotDF,
                 hbp_plot_extra = grazedWildDF,
                 hbp_plot_crop = cropDF,
                 hbp_site = siteDF)

  return(output)
}
