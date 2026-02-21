#' @title Scale herbaceous biomass by functional group data to mass per area
#'
#' @author
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Join NEON Herbaceous Clip Harvest data tables (DP1.10023.001) to calculate herbaceous biomass by functional group per unit area as well as total herbaceous biomass per unit area. Biomass outputs can be used in the neonPlants estimateMass() function, and the estimateHerbProd() productivity function.
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
#'   * hbp_agb - Summarizes above-ground herbaceous biomass for each sampled clip strip in the input data ("g/m2").
#'   * hbp_plot - Summarizes above-ground herbaceous peak standing biomass for each plot by year combination (both "g/m2" and "Mg/ha").
#'   * hbp_site - Summarizes above-ground herbaceous peak standing biomass for each site by year combination (both "g/m2" and "Mg/ha").
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

  ##  Remove exclosure == "Y" records at sites where exclosures were trialed by not used (SRER) and where
  ##  exclosure == "Y" was mistakenly selected
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

  #   Find orphaned hbp_massdata records, remove from 'hbp'; mass data records are expected to be orphaned due to inputBout filtering above, but orphaned records may also exist in input data (though unlikely)
  huerfanoDF <- hbp %>%
    dplyr::filter(is.na(.data$plotID) & is.na(.data$eventID) & is.na(.data$collectDate))

  hbp <- hbp %>%
    dplyr::filter(!.data$sampleID %in% huerfanoDF$sampleID)

  rm(huerfanoDF)


  ##  Filter by user-supplied plotSubset
  if (plotSubset %in% c("distributed", "tower")) {

    hbp <- hbp %>%
      dplyr::filter(.data$plotType == plotSubset)

  }


  ##  Calculate the average of any 'qaDryMass' replicates or duplicates within 'herbGroup', scale 'dryMass' to "g/m2" and assign a 0 when tTP = N, and remove spaces and hyphens from 'herbGroup' values
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

    dplyr::mutate(herbGroup = dplyr::case_when(.data$herbGroup == "All herbaceous plants" ~ "AllHerbaceousPlants",
                                               .data$herbGroup == "Cool Season Graminoids" ~ "CoolSeasonGraminoids",
                                               .data$herbGroup == "Woody-stemmed Plants" ~ "WoodyStemmedPlants",
                                               .data$herbGroup == "Warm Season Graminoids" ~ "WarmSeasonGraminoids",
                                               .data$herbGroup == "Leguminous Forbs" ~ "NFixingPlants",
                                               .data$herbGroup == "N-fixing Plants" ~ "NFixingPlants",
                                               .data$herbGroup == "Annual and Perennial Forbs" ~ "AnnualAndPerennialForbs",
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
  hbp_wide <- tidyr::pivot_wider(data = hbp,
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
    dplyr::relocate("AllHerbaceousPlants_gm2",
                    "NA_gm2",
                    .after = "exclosure")


  ##  Add columns for missing herbGroups; can occur when a small number of sites are used and not all herbGroups present at sites (especially crops).
  allHerbGroups <- c("AllHerbaceousPlants_gm2",
                     "AnnualAndPerennialForbs_gm2",
                     "CoolSeasonGraminoids_gm2",
                     "NFixingPlants_gm2",
                     "WarmSeasonGraminoids_gm2",
                     "WoodyStemmedPlants_gm2",
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

    if (!allHerbGroups[i] %in% names(hbp_wide)) {

      hbp_wide[, allHerbGroups[i]] <- NA

    }
  }


  ##  Calculate "AllHerbaceousPlants" biomass for sampling events sorted to herbGroup
  hbp_wide <- hbp_wide %>%
    dplyr::mutate(AllHerbaceousPlants_gm2 = dplyr::case_when(is.na(.data$AllHerbaceousPlants_gm2) ~
                                                               rowSums(dplyr::across("NA_gm2":"Wheat_gm2"),
                                                                       na.rm = TRUE),
                                                             TRUE ~ .data$AllHerbaceousPlants_gm2)) %>%
    dplyr::select(-"NA_gm2") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$eventID,
                   .data$plotID,
                   .data$subplotID,
                   .data$clipID)



  ### Cell-level output: Finalize data frame ####
  #   Assign data types to herbGroup_gm2 columns; can be "logi" if an herbGroup is absent from the dataset for selected sites
  hbp_wide <- hbp_wide %>%
    dplyr::mutate(AnnualAndPerennialForbs_gm2 = as.numeric(.data$AnnualAndPerennialForbs_gm2),
                  CoolSeasonGraminoids_gm2 = as.numeric(.data$CoolSeasonGraminoids_gm2),
                  NFixingPlants_gm2 = as.numeric(.data$NFixingPlants_gm2),
                  WarmSeasonGraminoids_gm2 = as.numeric(.data$WarmSeasonGraminoids_gm2),
                  WoodyStemmedPlants_gm2 = as.numeric(.data$WoodyStemmedPlants_gm2),
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
    dplyr::relocate("AnnualAndPerennialForbs_gm2",
                    .after = "AllHerbaceousPlants_gm2") %>%
    dplyr::relocate("Oat_gm2",
                    "OrchardGrass_gm2",
                    .after = "Millet_gm2")






  ### Plot-level output: Calculate plot-level mean peak herbaceous biomass by year ####

  ### Separate records into grazed, cropped, and standard 'siteID x year' data frames; each requires custom logic to calculate peak standing biomass at the plot level.

  ##  Step 1: Identify records in all 'siteID x year' combinations that supported grazing
  grazedSiteYearDF <- hbp_wide %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$exclosure == "Y"))


  ##  Step 2: Identify records for all 'siteID x year' combinations that contained crops in a plot at any point
  cropSiteYearDF <- hbp_wide %>%
    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"),
                  .before = "plotID") %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$siteYear %in% cropPlotsDF$siteYear))


  ##  Step 3: Identify all "standard" clips - i.e., no grazing, no crops at any point in a 'siteID x year'
  stdSiteYearDF <- hbp_wide %>%
    dplyr::filter(!.data$sampleID %in% grazedSiteYearDF$sampleID,
                  !.data$sampleID %in% cropSiteYearDF$sampleID)



  ### Std sites: Identify peak biomass bout for sites with > 1 bout per year and no grazing; returns single bout for sites with only one bout; Distributed and Tower plots processed separately then combined for output

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
    dplyr::summarise(MeanBiomass_gm2 = mean(.data$AllHerbaceousPlants_gm2, na.rm = TRUE),
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
                     AllHerbaceousPlants_gm2 = round(mean(.data$AllHerbaceousPlants_gm2, na.rm = TRUE),
                                                     digits = 2),
                     AnnualAndPerennialForbs_gm2 = round(mean(.data$AnnualAndPerennialForbs_gm2, na.rm = TRUE),
                                                         digits = 2),
                     CoolSeasonGraminoids_gm2 = round(mean(.data$CoolSeasonGraminoids_gm2, na.rm = TRUE),
                                                      digits = 2),
                     NFixingPlants_gm2 = round(mean(.data$NFixingPlants_gm2, na.rm = TRUE),
                                               digits = 2),
                     WarmSeasonGraminoids_gm2 = round(mean(.data$WarmSeasonGraminoids_gm2, na.rm = TRUE),
                                                      digits = 2),
                     WoodyStemmedPlants_gm2 = round(mean(.data$WoodyStemmedPlants_gm2, na.rm = TRUE),
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
  #--> Consider removing columns: subplotID, clipID, targetTaxaPresent, sampleID, clipArea, exclosure as these are minimally relevant to plot-level peak biomass. Clean up of "NaN" will come later once grazed and cropped estimates are joined.

  rm(distStdDF, temp, towerStdDF)




#---> Begin again here with finding peak biomass at grazed sites








  ##  Calculate plot-level peak biomass
  hbp_plot <- hbp_wide %>%

    #   Filter out exclosure == "Y" records
    dplyr::filter(.data$peak == "atPeak" & (.data$exclosure == "N" | is.na(.data$exclosure))) %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$plotID,
                    .data$nlcdClass,
                    .data$plotType) %>%

    #   Calculate average per plot if there are multiple subplots
    dplyr::summarise(
      herbPeakMassTotal_gm2 = round(mean(.data$AllHerbaceousPlants_gm2, na.rm = TRUE),
                                    digits = 2),
      herbPeakMassAnnualAndPerennialForbs_gm2 = round(mean(.data$AnnualAndPerennialForbs_gm2, na.rm = TRUE),
                                                      digits = 2),
      herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$CoolSeasonGraminoids_gm2, na.rm = TRUE),
                                                   digits = 2),
      herbPeakMassWarmSeasonGraminoids_gm2 = round(mean(.data$WarmSeasonGraminoids_gm2, na.rm = TRUE),
                                               digits = 2),
      herbPeakMassNFixingPlants_gm2 = round(mean(.data$NFixingPlants_gm2, na.rm = TRUE),
                                            digits = 2),
      herbPeakMassWoodyStemmedPlants_gm2 = round(mean(.data$WoodyStemmedPlants_gm2, na.rm = TRUE),
                                                 digits = 2),
      herbPeakMassCorn_gm2 = round(mean(.data$Corn_gm2, na.rm = TRUE),
                                   digits = 2),
      herbPeakMassBarley_gm2 = round(mean(.data$Barley_gm2, na.rm = TRUE),
                                     digits = 2),
      herbPeakMassMillet_gm2 = round(mean(.data$Millet_gm2, na.rm = TRUE),
                                     digits = 2),
      herbPeakMassOrchardGrass_gm2 = round(mean(.data$OrchardGrass_gm2, na.rm = TRUE),
                                           digits = 2),
      herbPeakMassSoybean_gm2 = round(mean(.data$Soybean_gm2, na.rm = TRUE),
                                      digits = 2),
      herbPeakMassSorghum_gm2 = round(mean(.data$Sorghum_gm2, na.rm = TRUE),
                                      digits = 2),
      herbPeakMassWheat_gm2 = round(mean(.data$Wheat_gm2, na.rm = TRUE),
                                    digits = 2),
      .groups = "drop") %>%

    #   Remove 'NaN' values introduced when herbGroup is absent from all subplots (usually crops)
    dplyr::mutate(dplyr::across("herbPeakMassTotal_gm2":"herbPeakMassWheat_gm2", ~dplyr::na_if(., NaN))) %>%

    #   Calculate "Mg/ha" for total herbaceous peak biomass; g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
    dplyr::mutate(herbPeakMassTotal_Mgha = round(.data$herbPeakMassTotal_gm2 * 10000 * 0.000001,
                                                 digits = 2),
                  .before = "herbPeakMassTotal_gm2")



  ### Calculate site-level peak biomass by year ####
  hbp_site <- hbp_plot %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year) %>%
    dplyr::summarise(herbPlotNum = length(stats::na.omit(.data$herbPeakMassTotal_gm2)),
                     herbPlotType = dplyr::case_when(dplyr::n_distinct(plotType, na.rm = TRUE) == 1 ~
                                                       paste(unique(plotType), collapse = ", "),
                                                     TRUE ~ paste(unique(plotType), collapse = ", ")),
                     herbPeakMassMean_gm2 = round(mean(.data$herbPeakMassTotal_gm2, na.rm = TRUE),
                                                  digits = 3),
                     herbPeakMassSD_gm2 = round(stats::sd(.data$herbPeakMassTotal_gm2, na.rm = TRUE),
                                                digits = 2),
                     herbPeakMassMean_Mgha = round(mean(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),
                                                   digits = 3),
                     herbPeakMassSD_Mgha = round(stats::sd(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),
                                                 digits = 2),
                     .groups = "drop")



  ### Return results: Bundle output as list and return ####

  output.list <- list(hbp_agb = hbp_wide,
                      hbp_plot = hbp_plot,
                      hbp_site = hbp_site)

  return(output.list)
}
