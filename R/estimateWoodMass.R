#' @title Estimate above-ground biomass of woody vegetation at NEON sites
#'
#' @author
#' Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#' Samuel M Simkin \email{samuel.simkin@gmail.com} \cr
#'
#' @description Allometric equations are used to estimate above-ground biomass for woody individuals reported in the NEON "Vegetation structure" data product (DP1.10098.001). Results are summarized as mass per unit area at scales of the plotID and siteID. Biomass outputs can be used in the companion estimateWoodProd() function.
#'
#' Data inputs are either "Vegetation structure" data (DP1.10098.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' @details Input data can be filtered via the 'plotSubset' argument if output for only certain types of plots or sampling intervals is desired. Consult the companion getVegStructureEvents() function for a report of which plot types were sampled in which years at a given NEON site. Input data are combined with taxon specific characteristics (e.g., wood density), and biomass is estimated for each individual using allometric equations. Taxon-specific equations are applied if available, and generalized allometries are used otherwise. The 'growthFormSubset' argument enables biomass estimation for "tree" individuals only (i.e., woody individuals with DBH ≥ 10 cm) or for "all" growth forms excluding "cactus", "ferns", and "yucca". Biomass is summarized on an areal basis at the hierarchical levels of the plotID and siteID.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. Expected input table names are "vst_perplotperyear", "vst_mappingandtagging", and "vst_apparentindividual"; it is optional to include the "vst_non-woody" table in the list. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for the 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments. [list]
#'
#' @param inputIndividual The 'vst_apparentindividual' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputMapTag The 'vst_mappingandtagging' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputNonWoody (Optional) The 'vst_non-woody' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputPerPlot The 'vst_perplotperyear' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param plotSubset Options are the default of "all" (all Tower and Distributed plots), "towerAll" (all plots in the Tower airshed but no Distributed plots), "towerAnnualSubset" (the subset of n=5 Tower plots that are sampled annually), and "distributed" (all Distributed plots, which are sampled at 5-yr intervals and are spatially representative of the NLCD classes at a site). [character]
#'
#' @param growthFormSubset Select Vegetation Structure growth forms for biomass estimation. The options are "tree", which enables biomass estimation only for single- and multi-bole trees, palm trees, and large tree ferns with a DBH ≥ 10 cm, and the default of "all", which includes "tree" individuals, and also small trees, single shrubs, small shrubs, lianas, small palms, small tree ferns, ocotillo, and xerophyllum individuals. Consult the Vegetation Structure Quick Start Guide and/or the Data Product User Guide for more growth form information. [character]
#'
#' @return A list that includes individual-level biomass, plot-level summary biomass, site-level summary biomass, and a table of individuals for which biomass could not be estimated for various reasons. Output tables include:
#'   * vst_agb_kg - Above-ground live and dead standing woody biomass reported for each individual ("kg").
#'   * vst_missing - Individuals with 'plantStatus' values of "removed", "lost" of some type, "no longer qualifies", and "downed", and also those individuals for which an allometry was missing or for which an above-ground biomass estimate is missing for some other reason.
#'   * vst_plot_Mgha - Summary of above-ground total, live, and dead standing woody biomass for each plotID x eventID combination ("Mg/ha"). If argument growthFormSubset == "tree", plots with smaller woody biomass but no trees will have zero biomass in this data frame.
#'   * vst_site_Mgha - Summary of above-ground total, live, and dead standing woody biomass for each siteID x year combination in the data ("Mg/ha").
#'
#' @examples
#' \dontrun{
#' #  Obtain NEON Vegetation structure data
#' vstDF <- neonUtilities::loadByProduct(
#' dpID = "DP1.10098.001",
#' package = "basic",
#' check.size = FALSE
#' )
#'
#' #  Example with arguments at default values
#' df <- estimateWoodMass(inputDataList = vstDF)
#'
#' #  Example specifying non-default arguments for 'plotSubset' and 'growthFormSubset'
#' df <- estimateWoodMass(
#' inputDataList = vstDF,
#' plotSubset = "towerAnnualSubset",
#' growthFormSubset = "tree"
#' )
#'
#' }
#'
#' @importFrom utils data
#'
#' @export estimateWoodMass


estimateWoodMass = function(inputDataList,
                            inputIndividual = NA,
                            inputMapTag = NA,
                            inputNonWoody = NA,
                            inputPerPlot = NA,
                            plotSubset = "all",
                            growthFormSubset = "all") {



  ### Set session behavior for 'dplyr::summarise' ###########################################################
  sessionInform <- getOption("dplyr.summarise.inform", default = TRUE)
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = sessionInform), add = TRUE)



  ### Input verification: Check that input arguments meet assumptions #######################################

  ### Verify user-supplied 'inputDataList' object contains correct data if not missing
  if (!missing(inputDataList)) {

    #   Check that input is a list
    if (!inherits(inputDataList, "list")) {
      stop(glue::glue("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"))
    }

    #   Check that required tables within list match expected names
    listExpNames <- c("vst_apparentindividual", "vst_mappingandtagging", "vst_perplotperyear")


    #   All expected tables required
    if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from 'inputDataList':",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
    }

  } else {

    inputDataList <- NULL

  } # end missing conditional



  ### Verify table inputs are NA if 'inputDataList' is supplied
  if (inherits(inputDataList, "list") &
      (!is.logical(inputIndividual) | !is.logical(inputMapTag) | !is.logical(inputPerPlot)  | !is.logical(inputNonWoody) )) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }



  ### Verify 'inputIndividual', 'inputMapTag', and 'inputPerPlot' are data frames if 'inputDataList' is missing
  if (is.null(inputDataList) &
      (!inherits(inputIndividual, "data.frame") | !inherits(inputMapTag, "data.frame") | !inherits(inputPerPlot, "data.frame"))) {

    stop("Data frames must be supplied for 'inputIndividual', 'inputMapTag', and 'inputPerPlot' if 'inputDataList' is not provided")
  }



  ### Assign standardized names to input data frames
  if (inherits(inputDataList, "list")) {

    map <- inputDataList$vst_mappingandtagging
    perPlot <- inputDataList$vst_perplotperyear
    appInd <- inputDataList$vst_apparentindividual

    #   Account for optional input of vst_non-woody
    if ("vst_non-woody" %in% names(inputDataList)) {
      nonWoody <- inputDataList$`vst_non-woody`
    } else {
      nonWoody <- NA
    }


  } else {

    map <- inputMapTag
    perPlot <- inputPerPlot
    appInd <- inputIndividual
    nonWoody <- inputNonWoody

  }



  ### Verify input tables contain required columns and data #############################################################

  ### Verify 'vst_mappingandtagging' table contains required data
  #   Check for required columns
  mapExpCols <- c("siteID", "plotID", "individualID", "taxonID")

  if (length(setdiff(mapExpCols, colnames(map))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_mappingandtagging':", '{paste(setdiff(mapExpCols, colnames(map)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(map) == 0) {
    stop(glue::glue("Table 'vst_mappingandtagging' has no data."))
  }


  ### Verify 'vst_perplotperyear' table contains required data
  #   Check for required columns
  plotExpCols <- c("date", "nonwoodyCollectDate", "domainID", "siteID", "plotID", "plotType", "nlcdClass", "samplingImpractical", "eventID", "eventType", "dataCollected", "targetTaxaPresent", "treesPresent", "shrubsPresent", "lianasPresent", "palmsPresent", "treeFernsPresent", "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana", "totalSampledAreaFerns", "totalSampledAreaOther")

  if (length(setdiff(plotExpCols, colnames(perPlot))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_perplotperyear':", '{paste(setdiff(plotExpCols, colnames(perPlot)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(perPlot) == 0) {
    stop(glue::glue("Table 'vst_perplotperyear' has no data."))
  }


  ### Verify 'vst_apparentindividual' table contains required data
  #   Check for required columns
  appIndExpCols <- c("domainID", "siteID","plotID", "individualID", "growthForm", "plantStatus", "date", "eventID", "stemDiameter", "basalStemDiameter", "height", "maxCrownDiameter", "ninetyCrownDiameter")

  if (length(setdiff(appIndExpCols, colnames(appInd))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_apparentindividual':", '{paste(setdiff(appIndExpCols, colnames(appInd)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(appInd) == 0) {
    stop(glue::glue("Table 'vst_apparentindividual' has no data."))
  }


  ### Verify vst_nonWoody table contains required data
  #   Check for required columns
  nonwoodyExpCols <- c("domainID", "siteID", "plotID", "individualID", "growthForm", "plantStatus", "date", "stemDiameter", "basalStemDiameter", "taxonID", "height", "stemLength", "leafNumber", "meanLeafLength", "meanPetioleLength", "meanBladeLength")

  if (methods::is(nonWoody, class = "data.frame" )) {

    if (length(setdiff(nonwoodyExpCols, colnames(nonWoody))) > 0) {
      stop(glue::glue("Required columns missing from vst_nonWoody:", '{paste(setdiff(nonwoodyExpCols, colnames(nonWoody), collapse = ", ")}',
                      .sep = " "))
    }
  }



  ### Verify optional input arguments meet requirements
  # Error if invalid growthFormSubset option selected
  if (!growthFormSubset %in% c("all", "tree")) {
    stop("The growthFormSubset argument must be one of: 'all', 'tree'")
  }

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The plotSubset argument must be one of: 'all', 'towerAll', 'towerAnnualSubset', 'distributed'")
  }

  #   Assign plotType needed in output based on 'plotSubset' argument
  plotType <- dplyr::case_when(plotSubset == "all" ~ "all",
                               plotSubset == "distributed" ~ "distributed",
                               plotSubset %in% c("towerAll", "towerAnnualSubset") ~ "tower")




  ### Prepare 'perPlot' data: Retrieve and join ancillary data and filter before analysis with supplied user arguments ##################

  ##  Extract year from eventID, create 'plotID x eventID' identifier
  perPlot <- perPlot %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$eventID, "20[0-9]{2}$")),
                  .before = "eventID") %>%
    dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                  .before = "plotID")


  ##  Join with plot priority data; the 'specificModuleSamplingPriority' field is used to optionally filter only to plots with priority 1-5 when user-supplied 'plotSubset' == "towerAnnualSubset"
  data("priority_plots", envir = environment())

  priority_plots <- priority_plots %>%
    dplyr::select("plotID",
                  "specificModuleSamplingPriority")

  perPlot <- dplyr::left_join(perPlot,
                              priority_plots,
                              by = "plotID")

  #   Filter according to user-supplied 'plotSubset' argument and derived 'plotPriority' variable
  if(plotSubset %in% c("towerAll", "towerAnnualSubset")) {
    perPlot <- perPlot[which(perPlot$plotType == "tower"),]
  }

  if(plotSubset == "distributed") {
    perPlot <- perPlot[which(perPlot$plotType == "distributed"),]
  }

  #   Retain only Tower subset if user-supplied filter is provided, otherwise keep all plots
  perPlot <- perPlot %>%
    dplyr::filter(as.logical(dplyr::case_when(
      plotSubset == "towerAnnualSubset" & .data$specificModuleSamplingPriority <= 5 ~ TRUE,
      plotSubset != "towerAnnualSubset" &
        (is.na(.data$specificModuleSamplingPriority) | .data$specificModuleSamplingPriority <= 50) ~ TRUE,
      TRUE ~ FALSE
    )))


  ##  Remove duplicates: Sort by date before removing duplicates so that if duplicates are from different dates the record from latest date will be retained. Sorting by date and then using fromLast = TRUE retains the most recent version of duplicates.
  perPlot <- perPlot[order(perPlot$date), ]
  perPlot <- perPlot[!duplicated(perPlot$plot_eventID, fromLast = TRUE), ]


  ##  Prepare 'plot_eventID' lists: These lists are used in subsequent steps to filter input and output tables
  #   Create list of plotID x eventID combinations from the vst_perplotperyear table and identify plot_eventIDs where sampling took place; resulting 'plot_eventID_list' includes plot_events with dataCollected == "dendrometerOnly" which is needed because "vst_agb_kg" table output should provide biomass for banded trees on individual level (but not plot level).
  plot_eventID_list <- perPlot %>%
    dplyr::filter(.data$samplingImpractical %in% c("", "OK") | is.na(.data$samplingImpractical)) %>%
    dplyr::distinct(.data$plot_eventID)

  plot_eventID_list <- plot_eventID_list$plot_eventID

  #   Identify 'plot_eventIDs' where *full* sampling took place; need to conditionally account for fact that dataCollected == "partial" is effectively full sampling when argument growthFormSubset == "tree".
  plot_eventID_full <- perPlot

  if (growthFormSubset == "all") {
    plot_eventID_full <- plot_eventID_full[which((plot_eventID_full$samplingImpractical %in% c("", "OK") |
                                                   is.na(plot_eventID_full$samplingImpractical)) &
                                                   !plot_eventID_full$dataCollected %in% c("dendrometerOnly", "partial")),]
  }

  if (growthFormSubset == "tree") {
    plot_eventID_full <- plot_eventID_full[which((plot_eventID_full$samplingImpractical %in% c("", "OK") |
                                                    is.na(plot_eventID_full$samplingImpractical)) &
                                                   plot_eventID_full$dataCollected != "dendrometerOnly"),]
  }

  plot_eventID_full <- plot_eventID_full %>%
    dplyr::distinct(.data$plot_eventID)

  plot_eventID_full <- plot_eventID_full$plot_eventID

  #   Identify 'plot_eventIDs' for dataCollected == "dendrometerOnly | partial"; list needed to remove these records from 'appInd' table and identify plots that are true "zeros" for woody biomass. Need to conditionally account for fact that dataCollected == "partial" is effectively full sampling when argument growthFormSubset == "tree"
  plot_eventID_partial <- perPlot

  if (growthFormSubset == "all") {
    plot_eventID_partial <- plot_eventID_partial[which((plot_eventID_partial$samplingImpractical %in% c("", "OK") |
                                                    is.na(plot_eventID_partial$samplingImpractical)) &
                                                   plot_eventID_partial$dataCollected %in% c("dendrometerOnly", "partial")),]
  }

  if (growthFormSubset == "tree") {
    plot_eventID_partial <- plot_eventID_partial[which((plot_eventID_partial$samplingImpractical %in% c("", "OK") |
                                                    is.na(plot_eventID_partial$samplingImpractical)) &
                                                   plot_eventID_partial$dataCollected == "dendrometerOnly"),]
  }

  plot_eventID_partial <- plot_eventID_partial %>%
    dplyr::distinct(.data$plot_eventID)

  plot_eventID_partial <- plot_eventID_partial$plot_eventID


  ##  Retain subset of columns in "perPlot" data; 'dataCollected' needed to identify plots for which biomass cannot be accurately estimated on an areal basis because not all trees were sampled.
  perPlot <- perPlot %>%
    dplyr::select("plot_eventID",
                  "domainID",
                  "siteID",
                  "plotID",
                  "eventID",
                  "year",
                  "nlcdClass",
                  "plotType",
                  "eventType",
                  "dataCollected",
                  "targetTaxaPresent",
                  "treesPresent",
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana",
                  "totalSampledAreaFerns",
                  "totalSampledAreaOther")



  ### Prepare 'map' data ######################################################################################

  ##  Retain most recent record from vst_mappingandtagging
  map <- map %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::slice_tail() %>%
    dplyr::ungroup()


  ##  Retain data for those plots in 'perPlot' table; 'map' output is effectively filtered to user-supplied 'plotSubset', then reduce to needed columns
  map <- map %>%
    dplyr::filter(.data$plotID %in% perPlot$plotID) %>%
    dplyr::select("individualID",
                  "taxonID",
                  "scientificName",
                  "genus",
                  "family")



  ### Prepare 'appInd' data ###################################################################################

  ##  Remove apparentIndividual records without necessary perplot data, rename 'basalMeasurementHeight' column
  appInd <- appInd %>%
    dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                  .before = "plotID") %>%
    dplyr::filter(.data$plot_eventID %in% plot_eventID_list) %>%
    dplyr::select(-"plot_eventID") %>%
    dplyr::rename("basalMeasurementHeight" = "basalStemDiameterMsrmntHeight")


  ##  Merge vst_apparentindividual table with 'map' to obtain taxonID fields
  #   Add taxonID to appInd table
  appInd <- dplyr::left_join(appInd,
                             map,
                             by = "individualID") %>%
    dplyr::relocate("taxonID",
                    "scientificName",
                    "genus",
                    "family",
                    .before = "growthForm")


  ##  Filter by user-supplied 'growthFormSubset'
  if (growthFormSubset == "tree") {

    appInd <- appInd %>%
      dplyr::filter(.data$growthForm %in% c("single bole tree", "multi-bole tree"))

  }


  ##  Create 'liveDeadStatus' field to parse standing biomass unambiguously
  #   Define plantStatus values to identify standing individuals that are unambiguously live/dead
  standingLiveDead <- c("Live",
                        "Live, insect damaged",
                        "Live, disease damaged",
                        "Live, physically damaged",
                        "Live, other damage",
                        "Live, broken bole",
                        "Standing dead",
                        "Dead, broken bole")

  #   Define plantStatus values to identify absent individuals that are definitely dead but for which we have no stemDiameter data, and individuals absent, lost, or with ambiguous fate
  lostDowned <- c("Downed",
                  "Removed",
                  "No longer qualifies",
                  "Lost, burned",
                  "Lost, herbivory",
                  "Lost, presumed dead",
                  "Lost, fate unknown")

  #   Assign liveDeadStatus values
  #--> 'downedDead' is equivalent to 'dead' for productivity purposes but is specified explicitly here because wood mass outputs are needed for *standing* live and *standing* dead.
  appInd <- appInd %>%
    dplyr::mutate(liveDeadStatus = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                    .data$plantStatus %in% tail(standingLiveDead, 2) ~ "dead",
                                                    .data$plantStatus %in% head(lostDowned, 2) ~ "downedDead",
                                                    .data$plantStatus %in% tail(lostDowned, 5) ~ "lost",
                                                    TRUE ~ NA_character_),
                  .after = "plantStatus")



  ### Create 'lostDownedDF' table for later output
  lostDownedDF <- appInd %>%

    #   Join to get plot-level data
    dplyr::left_join(perPlot %>%
                       dplyr::select("plotID", "eventID", "nlcdClass", "plotType", "year", "eventType", "dataCollected",
                                     "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana"),
                     by = c("plotID", "eventID")) %>%

    #   Simplify totalSampledArea data to a single column
    dplyr::mutate(

      sampledArea_m2 = dplyr::case_when(

        .data$growthForm %in% c("single bole tree", "multi-bole tree") ~ .data$totalSampledAreaTrees,
        .data$growthForm %in% c("single shrub", "small shrub", "small tree", "sapling") ~ .data$totalSampledAreaShrubSapling,
        .data$growthForm == "liana" ~ .data$totalSampledAreaLiana,
        TRUE ~ NA_integer_)

    ) %>%

    #   Remove unneeded 'totalSampledArea' columns
    dplyr::select(-"totalSampledAreaTrees",
                  -"totalSampledAreaShrubSapling",
                  -"totalSampledAreaLiana") %>%

    #   Retain only "downedDead" and "missing" individuals, and those with ambiguous status or no growthForm
    dplyr::filter(.data$liveDeadStatus %in% c("downedDead", "lost") | is.na(.data$liveDeadStatus) | is.na(.data$growthForm)) %>%

    #   Select columns for output and arrange
    dplyr::select("domainID", "siteID", "plotID", "subplotID", "nlcdClass", "plotType",
                  "year", "date", "eventID", "eventType", "dataCollected",
                  "individualID", "tempStemID", "taxonID", "scientificName", "genus", "family", "growthForm", "liveDeadStatus",
                  "sampledArea_m2", "stemDiameter", "basalStemDiameter", "height", "maxCrownDiameter", "ninetyCrownDiameter",
                  "measurementHeight", "basalMeasurementHeight", "changedMeasurementLocation", "heightQualifier",
                  "initialGapMeasurementDate", "initialBandStemDiameter", "initialDendrometerGap", "dendrometerGap", "dendrometerCondition",
                  "bandStemDiameter", "remarks", "dataQF") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID,
                   .data$individualID,
                   .data$tempStemID)



  ### Isolate records with liveDeadStatus of "live" or "dead" to send to allometric wood mass function
  appInd <- appInd %>%
    dplyr::filter(.data$liveDeadStatus %in% c("live", "dead") & !is.na(.data$growthForm))



  ### Estimate woody biomass: Calculate biomass for individuals in vst_apparentindividual table ############################

  ### Get allometric estimates of woody biomass
  agbWoody_kg <- estimateAllometricWoodyMass(appIndTable = appInd,
                                             growthFormSubset = growthFormSubset)



  ### Process woody data

  ##  Add 'totalSampledArea' and other metadata from 'perPlot' table
  agbWoody_kg <- dplyr::left_join(agbWoody_kg,
                                  perPlot,
                                  by = c("domainID", "siteID", "plotID", "eventID"))


  ##  Sampled area: Consolidate 'totalSampledArea[growthForm]' columns into single column
  #   Assign total sampled area by growthForm
  agbWoody_kg <- agbWoody_kg %>%

    dplyr::mutate(

      sampledArea_m2 = dplyr::case_when(

        .data$growthForm %in% c("single bole tree", "multi-bole tree") ~ .data$totalSampledAreaTrees,
        .data$growthForm %in% c("single shrub", "small shrub", "small tree", "sapling") ~ .data$totalSampledAreaShrubSapling,
        .data$growthForm == "liana" ~ .data$totalSampledAreaLiana,
        TRUE ~ NA_integer_)

      ) %>%

    #   Remove unneeded 'totalSampledArea' columns
    dplyr::select(-"totalSampledAreaTrees",
                  -"totalSampledAreaShrubSapling",
                  -"totalSampledAreaLiana",
                  -"totalSampledAreaFerns",
                  -"totalSampledAreaOther")


  ##  Aggregate woody biomass data by 'year' x 'individualID' x 'liveDeadStatus'
  #--> Assumes that multiple instances of same individualID are true multiple boles and not accidental duplicates. Output is used for both annual biomass summaries and NPP calculations for specified consecutive years.
  #--> Affects 'small tree', 'liana' individuals, all other woody multi-stems are aggregated by estimateAllometricWoodyMass function
  agbWoody_kg <- agbWoody_kg %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$individualID,
                    .data$date,
                    .data$plotID,
                    .data$subplotID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$eventType,
                    .data$dataCollected,
                    .data$taxonID,
                    .data$family,
                    .data$genus,
                    .data$scientificName,
                    .data$liveDeadStatus,
                    .data$growthForm,
                    .data$sampledArea_m2) %>%

    dplyr::summarise(

      #   Aggregrate references in 'source' column; insurance, should only be one
      source = ifelse(!all(is.na(.data$source)),
                      paste(unique(.data$source), collapse = ", "),
                      NA_character_),

      #   Sum 'agb_kg' across stems to get total biomass of individual
      agb_kg = ifelse(!all(is.na(.data$agb_kg)),
                      round(sum(.data$agb_kg, na.rm = TRUE), digits = 2),
                      NA_real_),

      #   Calculate single equivalent diameter for multiple stems
      stemDiameter = ifelse(!all(is.na(.data$stemDiameter)),
                            round(sqrt(sum(.data$stemDiameter^2, na.rm = TRUE)), digits = 1),
                            NA_real_),

      basalStemDiameter = ifelse(!all(is.na(.data$basalStemDiameter)),
                                 round(sqrt(sum(.data$basalStemDiameter^2, na.rm = TRUE)), digits = 1),
                                 NA_real_),

      #   Take maximum height, crownDiameters
      height = ifelse(!all(is.na(.data$height)),
                      round(max(.data$height, na.rm = TRUE), digits = 1),
                      NA_real_),

      maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                round(max(.data$maxCrownDiameter, na.rm = TRUE), digits = 1),
                                NA_real_),

      ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                   round(max(.data$ninetyCrownDiameter, na.rm = TRUE), digits = 1),
                                   NA_real_),

    .groups = "drop")



  ### Prepare 'nonWoody' data ##########################################################################

  if (methods::is(nonWoody, class = "data.frame")) {

    ### Remove nonWoody records without necessary perPlot data (incorporates 'plotSubset' filtering) and remove records based on user-supplied 'growthFormSubset'
    nonWoody <- nonWoody %>%
      dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                    .before = "plotID") %>%
      dplyr::filter(.data$plot_eventID %in% plot_eventID_list) %>%
      dplyr::select(-"plot_eventID")


    if (growthFormSubset == "tree") {

      nonWoody <- nonWoody %>%
        dplyr::filter(.data$growthForm %in% c("palm tree", "large tree fern"))

    }



    ### Assign liveDeadStatus values
    #--> 'downedDead' is equivalent to 'dead' for productivity purposes but is specified explicitly here because nonwoody mass outputs are needed for *standing* live and *standing* dead.
    nonWoody <- nonWoody %>%
      dplyr::mutate(liveDeadStatus = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                      .data$plantStatus %in% tail(standingLiveDead, 2) ~ "dead",
                                                      .data$plantStatus %in% head(lostDowned, 2) ~ "downedDead",
                                                      .data$plantStatus %in% tail(lostDowned, 5) ~ "lost",
                                                      TRUE ~ NA_character_),
                    .after = "plantStatus")



    ### Create unified 'lostDownedDF' data frame for output

    ##  Collate lost nonwoody individuals (removed, lost, downed) and those with no growthForm similar to woody filtering
    temp <- nonWoody %>%

      #   Join to get plot-level data
      dplyr::left_join(perPlot %>%
                         dplyr::select("plotID", "eventID", "nlcdClass", "plotType", "year", "eventType", "dataCollected",
                                       "totalSampledAreaTrees", "totalSampledAreaFerns", "totalSampledAreaOther"),
                       by = c("plotID", "eventID")) %>%

      #   Simplify totalSampledArea data to a single column
      dplyr::mutate(

        sampledArea_m2 = dplyr::case_when(

          .data$growthForm %in% c("large tree fern", "palm tree") ~ .data$totalSampledAreaTrees,
          .data$growthForm == "fern" ~ .data$totalSampledAreaFerns,
          .data$growthForm %in% c("cactus", "ocotillo", "small palm", "small tree fern",
                                  "xerophyllum", "yucca") ~ .data$totalSampledAreaOther,
          TRUE ~ NA_integer_)

      ) %>%

      #   Remove unneeded 'totalSampledArea' columns
      dplyr::select(-"totalSampledAreaTrees",
                    -"totalSampledAreaFerns",
                    -"totalSampledAreaOther") %>%

      #   Retain only "downedDead" and "lost" individuals, and those with ambiguous status or no growthForm
      dplyr::filter(.data$liveDeadStatus %in% c("downedDead", "lost") | is.na(.data$liveDeadStatus) | is.na(.data$growthForm)) %>%

      #   Select columns for output and arrange
      dplyr::select("domainID", "siteID", "plotID", "subplotID", "nlcdClass", "plotType",
                    "year", "date", "eventID", "eventType", "dataCollected",
                    "individualID", "taxonID", "scientificName", "growthForm", "liveDeadStatus",
                    "sampledArea_m2", "stemDiameter", "basalStemDiameter", "height", "maxCrownDiameter", "ninetyCrownDiameter",
                    "measurementHeight", "leafNumber", "meanLeafLength", "meanPetioleLength", "meanBladeLength", "meanBasalDiameter",
                    "stemLength", "oldPadCount", "newPadCount", "stemCount", "branchCount", "meanBranchLength",
                    "remarks") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotID,
                     .data$individualID)


    ##  Bind 'other' lostDowned with 'woody' lostDowned
    lostDownedDF <- dplyr::bind_rows(lostDownedDF,
                                     temp)



    ### Isolate records with liveDeadStatus of "live" or "dead" to send to allometric "other" mass function
    nonWoody <- nonWoody %>%
      dplyr::filter(.data$liveDeadStatus %in% c("live", "dead") & !is.na(.data$growthForm))

  } # end nonWoody data frame conditional



  ### Estimate non-woody biomass: Calculate biomass from vst_non-woody table ######################################

  ##  Get allometric estimates of 'other' non-woody biomass
  if (methods::is(nonWoody, class = "data.frame" )) {

    agbOther_kg <- estimateAllometricOtherMass(nonWoodyTable = nonWoody,
                                               growthFormSubset = growthFormSubset)

  }


  ##  Process non-woody data
  if (exists("agbOther_kg")) {

    ##  Merge with perPlot data to add total sampled areas
    agbOther_kg <- dplyr::left_join(agbOther_kg,
                                    perPlot,
                                    by = c("domainID", "siteID", "plotID", "eventID"))


    ##  Sampled area: Consolidate 'totalSampledArea[growthForm]' columns into single column
    #   Assign total sampled area by growthForm
    agbOther_kg <- agbOther_kg %>%

      dplyr::mutate(

        sampledArea_m2 = dplyr::case_when(
          .data$growthForm %in% c("palm tree", "large tree fern") ~ .data$totalSampledAreaTrees,
          .data$growthForm == "fern" ~ .data$totalSampledAreaFerns,
          .data$growthForm %in% c("cactus", "ocotillo", "small palm", "small tree fern", "xerophyllum", "yucca") ~ .data$totalSampledAreaOther,
          TRUE ~ NA_integer_
        )
      ) %>%

      #   Remove unneeded 'totalSampledArea' columns
      dplyr::select(-"totalSampledAreaTrees",
                    -"totalSampledAreaShrubSapling",
                    -"totalSampledAreaLiana",
                    -"totalSampledAreaFerns",
                    -"totalSampledAreaOther")


    ##  Aggregate non-herbaceous perennial (other) biomass data (multiple records associated with multi-stem individuals)
    agbOther_kg <- agbOther_kg %>%
      dplyr::group_by(.data$plot_eventID,
                      .data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$eventID,
                      .data$individualID,
                      .data$date,
                      .data$plotID,
                      .data$subplotID,
                      .data$nlcdClass,
                      .data$plotType,
                      .data$eventType,
                      .data$dataCollected,
                      .data$taxonID,
                      .data$scientificName,
                      .data$liveDeadStatus,
                      .data$growthForm,
                      .data$sampledArea_m2) %>%

      dplyr::summarise(

        #   Aggregrate references in 'source' column; insurance, should only be one
        source = ifelse(!all(is.na(.data$source)),
                        paste(unique(.data$source), collapse = ", "),
                        NA_character_),

        #   Sum 'agb_kg' across stems to get total biomass of individual
        agb_kg = ifelse(!all(is.na(.data$agb_kg)),
                        round(sum(.data$agb_kg, na.rm = TRUE), digits = 4),
                        NA_real_),

        #   Calculate single equivalent diameter for multiple stems
        stemDiameter = ifelse(!all(is.na(.data$stemDiameter)),
                              round(sqrt(sum(.data$stemDiameter^2, na.rm = TRUE)), digits = 1),
                              NA_real_),

        basalStemDiameter = ifelse(!all(is.na(.data$basalStemDiameter)),
                                   round(sqrt(sum(.data$basalStemDiameter^2, na.rm = TRUE)), digits = 1),
                                   NA_real_),

        #   Take maximum height, crownDiameters
        height = ifelse(!all(is.na(.data$height)),
                        round(max(.data$height, na.rm = TRUE), digits = 1),
                        NA_real_),

        maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                  round(max(.data$maxCrownDiameter, na.rm = TRUE), digits = 1),
                                  NA_real_),

        ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                     round(max(.data$ninetyCrownDiameter, na.rm = TRUE), digits = 1),
                                     NA_real_),

        #   Calculate means for 'stemLength' and 'meanLeafLength'
        stemLength = ifelse(!all(is.na(.data$stemLength)),
                            round(mean(.data$stemLength, na.rm = TRUE), digits = 1),
                            NA_real_),

        meanLeafLength = ifelse(!all(is.na(.data$meanLeafLength)),
                                round(mean(.data$meanLeafLength, na.rm = TRUE), digits = 1),
                                NA_real_),

        #   Calculate total 'stemCount' and 'leafNumber'
        stemCount = ifelse(!all(is.na(.data$stemCount)),
                           sum(.data$stemCount, na.rm = TRUE),
                           NA_integer_),

        leafNumber = ifelse(!all(is.na(.data$leafNumber)),
                            sum(.data$leafNumber, na.rm = TRUE),
                            NA_integer_),

        .groups = "drop")

  } else {

    agbOther_kg <- data.frame()

  }



  ###  Combine woody and non-woody AGB estimates into single data frame ##########################################
  #   Conditionally bind 'woody' and 'other' outputs
  if (nrow(agbOther_kg) > 0) {

    agbDF <- dplyr::bind_rows(agbWoody_kg,
                              agbOther_kg)

  } else {
    agbDF <- agbWoody_kg
  }

  #   Arrange output
  agbDF <- agbDF %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID,
                   .data$individualID)



  ### Scaling: Determine biomass per unit area (Mg/ha) #############################################################

  ### Retain records corresponding to full-plot sampling (scaling not sensible when plot not fully sampled)
  #--> Note: Previously defined 'plot_eventID_full' accounts for user-supplied 'growthFormSubset' == "all" or "tree"
  agbPlotDF <- agbDF %>%
    dplyr::filter(.data$plot_eventID %in% plot_eventID_full,
                  .data$sampledArea_m2 > 0)



  ### Calculate AGB in "Mg/ha" for each individual
  agbPlotDF <- agbPlotDF %>%
    dplyr::mutate(agb_Mgha = round(.data$agb_kg * 0.001 * (10000 / .data$sampledArea_m2), digits = 4),
                  .before = "agb_kg")



  ### Identify sampled plots with zero qualifying biomass
  #--> Plots with zero biomass are conditionally identified via user-supplied 'growthFormSubset'
  if (growthFormSubset == "all") {

    agbZeroDF <- perPlot %>%
      dplyr::filter(.data$plot_eventID %in% plot_eventID_full,
                    .data$targetTaxaPresent == "N")
  }

  if (growthFormSubset == "tree") {

    agbZeroDF <- perPlot %>%
      dplyr::filter(.data$plot_eventID %in% plot_eventID_full,
                    .data$treesPresent == "N")
  }

  agbZeroDF <- agbZeroDF %>%
    dplyr::select(-("totalSampledAreaTrees":"totalSampledAreaOther")) %>%
    dplyr::mutate(agbLive_Mgha = 0,
                  agbDead_Mgha = 0)



  ### Plot-level summary of AGB data ##################################################################

  ### Sum biomass per unit area for each 'year' x 'plotID' x 'liveDeadStatus': Aggregate mass from all individualIDs
  agbPlotDF <- agbPlotDF %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$plotID,
                    .data$liveDeadStatus,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$eventType,
                    .data$dataCollected) %>%

    dplyr::summarise(agb_Mgha = ifelse(!all(is.na(.data$agb_Mgha)),
                                       round(sum(.data$agb_Mgha, na.rm = TRUE), digits = 2),
                                       NA_real_),
                     .groups = "drop") %>%
    dplyr::relocate(("nlcdClass":"dataCollected"),
                    .after = "plotID")



  ### Within a year, transpose live and dead AGB into separate columns
  agbPlotDF <- agbPlotDF %>%
    tidyr::pivot_wider(id_cols = c("plot_eventID",
                                   "domainID",
                                   "siteID",
                                   "year",
                                   "eventID",
                                   "plotID",
                                   "nlcdClass",
                                   "plotType",
                                   "eventType",
                                   "dataCollected"),
                       names_from = "liveDeadStatus",
                       values_from = "agb_Mgha")

  #   Add 'live' and 'dead' columns if missing; may happen if all standing biomass in input plots is dead or no dead
  if (!"live" %in% names(agbPlotDF)) {
    agbPlotDF$live <- 0
  }

  if (!"dead" %in% names(agbPlotDF)) {
    agbPlotDF$dead <- 0
  }

  #   Rename live/dead columns
  agbPlotDF <- agbPlotDF %>%
    dplyr::rename("agbLive_Mgha" = "live",
                  "agbDead_Mgha" = "dead")

  #   Assumption: Replace NAs created during transpose with zeroes; assume both live and dead were sampled in a plot
  agbPlotDF$agbDead_Mgha[is.na(agbPlotDF$agbDead_Mgha)] <- 0
  agbPlotDF$agbLive_Mgha[is.na(agbPlotDF$agbLive_Mgha)] <- 0



  ### Finalize plot-level biomass summary: Add 'agbZeroDF' data and calculate total AGB
  agbPlotDF <- dplyr::bind_rows(agbPlotDF,
                                agbZeroDF %>%
                                  dplyr::select(-"targetTaxaPresent",
                                                -"treesPresent")
  ) %>%
    dplyr::relocate("agbLive_Mgha",
                    .before = "agbDead_Mgha") %>%
    dplyr::mutate(agb_Mgha = rowSums(dplyr::across(c("agbLive_Mgha", "agbDead_Mgha")), na.rm = TRUE),
                  .before = "agbLive_Mgha") %>%
    dplyr::select(-"plot_eventID") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$plotID,
                   .data$year)



  ### Site-level summary of AGB data #######################################################################

  if (nrow(agbPlotDF) > 0) {

    agbSiteDF <- agbPlotDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year) %>%
      dplyr::summarise(woodPlotNum = dplyr::n(),
                       woodPlotType = paste(unique(.data$plotType), collapse = ", "),
                       woodEventType = paste(unique(.data$eventType), collapse = ", "),
                       woodMassMean_Mgha = round(mean(.data$agb_Mgha, na.rm = TRUE), digits = 1),
                       woodMassSD_Mgha = round(stats::sd(.data$agb_Mgha, na.rm = TRUE), digits = 1),
                       woodLiveMassMean_Mgha = round(mean(.data$agbLive_Mgha, na.rm = TRUE), digits = 1),
                       woodLiveMassSD_Mgha = round(stats::sd(.data$agbLive_Mgha, na.rm = TRUE), digits = 1),
                       woodDeadMassMean_Mgha = round(mean(.data$agbDead_Mgha, na.rm = TRUE), digits = 1),
                       woodDeadMassSD_Mgha = round(stats::sd(.data$agbDead_Mgha, na.rm = TRUE), digits = 1),
                       .groups = "drop") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year)
  } else {

    agbSiteDF <- data.frame()
    message("Could not create site-level output table: Insufficient plot-level data")

  }



  ### Bundle and return output ############################################################################
  agbDF <- agbDF %>%
    dplyr::select(-"plot_eventID")

  output <- list(vst_agb_kg = agbDF,
                 vst_lost_downed = lostDownedDF,
                 vst_plot_Mgha = agbPlotDF,
                 vst_site_Mgha = agbSiteDF)

  return(output)

}
