#' @title Estimate above-ground biomass of woody vegetation
#'
#' @author
#' Samuel M Simkin \email{ssimkin@BattelleEcology.org} \cr
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Allometric equations are used to estimate above-ground biomass for woody individuals reported in the NEON "Vegetation structure" data product (DP1.10098.001). Results are summarized as mass per unit area at scales of the plotID and siteID. Biomass outputs can be used in the neonPlants estimateMass() and estimateWoodProd() functions.
#'
#' Data inputs are "Vegetation structure" data (DP1.10098.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' @details Input data can be filtered via the 'plotSubset' argument if output for only certain types of plots or sampling intervals is desired. Input data are combined with taxon specific characteristics (e.g., wood density), and biomass is estimated for each individual using allometric equations. Taxon-specific equations are applied if available, and generalized allometries are used otherwise. The 'growthFormSubset' argument enables biomass estimation only for "tree" individuals (i.e., woody individuals with DBH ≥ 10 cm) or for "all" growth forms excluding "cactus", "ferns", and "yucca". Biomass is summarized on an areal basis at the hierarchical level of the plotID and siteID.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. It is optional to include the "vst_non-woody" table in the list. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for the 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments.  [list]
#'
#' @param inputIndividual The 'vst_apparentindividual' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputMapTag The 'vst_mappingandtagging' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputNonWoody (Optional) The 'vst_non-woody' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputPerPlot The 'vst_perplotperyear' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param plotSubset The options are the default of "all" (all Tower and Distributed plots), "towerAll" (all plots in the Tower airshed but no Distributed plots), "towerAnnualSubset" (the subset of n=5 Tower plots that are sampled annually), and "distributed" (all Distributed plots, which are sampled at 5-yr intervals and are spatially representative of the NLCD classes at a site). [character]
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
#' @export estimateWoodMass


estimateWoodMass = function(inputDataList,
                            inputIndividual = NA,
                            inputMapTag = NA,
                            inputNonWoody = NA,
                            inputPerPlot = NA,
                            plotSubset = "all",
                            growthFormSubset = "all") {

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
      (!inherits(inputIndividual, "data.frame") | !inherits(inputMapTag, "data.frame")  |
       !inherits(inputPerPlot, "data.frame") | !inherits(inputNonWoody, "data.frame"))) {

    stop("Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
  }


  #   Assign standardized names to input data frames
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



  ### Verify input tables contain required columns and data ####

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
  plotExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana", "totalSampledAreaFerns", "totalSampledAreaOther")

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
  appIndExpCols <- c("plotID", "individualID", "growthForm", "plantStatus", "date", "eventID", "stemDiameter", "basalStemDiameter", "height", "maxCrownDiameter", "ninetyCrownDiameter")

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

  #  if (class(vst_nonWoody) == "data.frame"){if(length(setdiff(nonwoodyExpCols, colnames(vst_nonWoody))) > 0) {
  if (methods::is(nonWoody, class = "data.frame" )) {

    if (length(setdiff(nonwoodyExpCols, colnames(nonWoody))) > 0) {
      stop(glue::glue("Required columns missing from vst_nonWoody:", '{paste(setdiff(nonwoodyExpCols, colnames(nonWoody), collapse = ", ")}',
                      .sep = " "))
    }
  }

  # Error if invalid growthFormSubset option selected
  if (!growthFormSubset %in% c("all", "tree")) {
    stop("The growthFormSubset argument must be one of: 'all', 'tree'")
  }

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The plotSubset argument must be one of: 'all', 'towerAll', 'towerAnnualSubset', 'distributed'")
  }

  #   For plotPriority: 50 is highest possible value
  plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50)

  #   Assign plotType needed in output based on 'plotSubset' argument
  plotType <- dplyr::case_when(plotSubset == "all" ~ "all",
                               plotSubset == "distributed" ~ "distributed",
                               plotSubset %in% c("towerAll", "towerAnnualSubset") ~ "tower")




  ### Prepare data frames: Retrieve and join ancillary data and filter before analysis with supplied user arguments ####

  ### Prepare 'perPlot' table
  #   Extract year from eventID, create 'plotID x eventID' identifier
  perPlot <- perPlot %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$eventID, "20[0-9]{2}$")),
                  .before = "eventID") %>%
    dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                  .before = "plotID")


  ##  Join with plot priority data; the 'specificModuleSamplingPriority' field is used to optionally filter only to plots with priority 1-5 when user-supplied 'plotSubset' == "towerAnnualSubset"
  priority_plots <- priority_plots %>%
    dplyr::select("plotID",
                  "specificModuleSamplingPriority")

  perPlot <- merge(perPlot,
                   priority_plots,
                   by = "plotID",
                   all.x = TRUE)

  #   Filter according to user-supplied 'plotSubset' argument and derived 'plotPriority' variable
  perPlot <- perPlot %>%
    dplyr::filter(dplyr::case_when(plotSubset %in% c("towerAll", "towerAnnualSubset") ~ .data$plotType == "tower",
                                   plotSubset == "distributed" ~ .data$plotType == "distributed",
                                   TRUE ~ .data$plotType %in% c("distributed", "tower"))) %>%
    dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)


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
  plot_eventID_full <- perPlot %>%
    dplyr::filter(dplyr::case_when(growthFormSubset == "all" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                  is.na(.data$samplingImpractical)) &
                                     !.data$dataCollected %in% c("dendrometerOnly", "partial"),
                                   growthFormSubset == "tree" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                   is.na(.data$samplingImpractical)) &
                                     .data$dataCollected != "dendrometerOnly")) %>%
    dplyr::distinct(.data$plot_eventID)

  plot_eventID_full <- plot_eventID_full$plot_eventID

  #   Identify 'plot_eventIDs' for dataCollected == "dendrometerOnly | partial"; list needed to remove these records from 'appInd' table and identify plots that are true "zeros" for woody biomass. Need to conditionally account for fact that dataCollected == "partial" is effectively full sampling when argument growthFormSubset == "tree"
plot_eventID_partial <- perPlot %>%
  dplyr::filter(dplyr::case_when(growthFormSubset == "all" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                is.na(.data$samplingImpractical)) &
                                   .data$dataCollected %in% c("dendrometerOnly", "partial"),
                                 growthFormSubset == "tree" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                 is.na(.data$samplingImpractical)) &
                                   .data$dataCollected == "dendrometerOnly")) %>%
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
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana",
                  "totalSampledAreaOther")



  ### Prepare 'map' table
  #   Retain most recent record from vst_mappingandtagging
  map <- map[order(map$date),]
  map <- map[!duplicated(map$individualID, fromLast = TRUE), ]

  #   Retain data for those plots in 'perPlot' table; 'map' output is effectively filtered to user-supplied 'plotSubset'
  map <- map %>%
    dplyr::filter(.data$plotID %in% perPlot$plotID)

  #   Find unique taxonIDs
  taxonID_df <- map %>%
    dplyr::distinct(.data$taxonID,
                    .data$scientificName,
                    .data$family,
                    .data$genus)

  vst_taxonIDs <- sort(taxonID_df$taxonID)

  map <- map %>%
    dplyr::select("individualID",
                  "taxonID")



  ### Prepare 'appInd' table
  ##  Remove apparentIndividual records without necessary perplot data
  appInd$plot_eventID <- paste0(appInd$plotID, "_", appInd$eventID)

  appInd <- appInd %>%
    dplyr::filter(appInd$plot_eventID %in% plot_eventID_list)


  ##  Merge vst_apparentindividual table with map and perplot to obtain taxonID field and sampling area fields
  #   Add taxonID to appInd table
  appInd <- merge(appInd,
                  map,
                  by = "individualID",
                  all.x = TRUE)

  #   Add total sampled area fields
  appInd <- merge(appInd,
                  perPlot,
                  by = c("domainID", "siteID", "plotID", "eventID", "plot_eventID"),
                  all.x = TRUE)

  #   Resolve missing taxonIDs and Betula slash species issue
  appInd$taxonID <- ifelse(is.na(appInd$taxonID),
                           "2PLANT",
                           appInd$taxonID)

  appInd$taxonID <- ifelse(appInd$taxonID == "BEGL/BENA",
                           "BEGL",
                           appInd$taxonID)


  ##  Filter by user-supplied 'growthFormSubset'
  if (growthFormSubset == "tree") {

    appInd <- appInd %>%
      dplyr::filter(.data$growthForm %in% c("single bole tree", "multi-bole tree"))

  }


  ##  Remove unneeded columns and reorganize column order
  appInd <- appInd %>%
    dplyr::select(-"uid",
                  -"namedLocation",
                  -"dendrometerInstallationDate",
                  -"initialGapMeasurementDate",
                  -"initialBandStemDiameter",
                  -"initialDendrometerGap",
                  -"dendrometerHeight",
                  -"dendrometerGap",
                  -"dendrometerCondition",
                  -"bandStemDiameter",
                  -"publicationDate",
                  -"measuredBy",
                  -"recordedBy",
                  -"dataEntryRecordID",
                  -"release",
                  -"dataQF") %>%
    dplyr::relocate(c("plotID", "eventID"),
                    .after = "siteID")



  ### Prepare 'nonWoody' table
  if (methods::is(nonWoody, class = "data.frame")) {

    ##  Create additional required columns
    nonWoody <- nonWoody %>%
      dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                    .before = "date") %>%
      dplyr::mutate(year = as.numeric(stringr::str_extract(.data$eventID, "20[0-9]{2}")),
                    .before = "eventID")


    ##  Remove nonWoody records without necessary perplot data (incorporates 'plotSubset' filtering) and remove records based on user-supplied 'growthFormSubset'
    nonWoody <- nonWoody %>%
      dplyr::filter(.data$plot_eventID %in% plot_eventID_list)


    if (growthFormSubset == "tree") {

      nonWoody <- nonWoody %>%
        dplyr::filter(.data$growthForm %in% c("palm tree", "large tree fern"))

    }


    ##  Remove unneeded columns
    nonWoody <- nonWoody %>%
      dplyr::select(-"uid",
                    -"namedLocation",
                    -"publicationDate",
                    -"stemCount",
                    -"branchCount",
                    -"meanBranchLength",
                    -"identificationReferences",
                    -"identificationQualifier",
                    -"morphospeciesID",
                    -"measuredBy",
                    -"recordedBy",
                    -"dataEntryRecordID",
                    -"release")


    ##  Merge with perplot data to add total sampled areas
    nonWoody <- merge(nonWoody,
                      perPlot,
                      by = c("plot_eventID", "year", "domainID", "siteID", "plotID", "eventID"),
                      all.x = TRUE)

  } # end nonWoody data frame conditional



  ###  Define plantStatus groups to identify standing individuals that are live/dead and individuals absent, missing, or with ambiguous fate
  standingLiveDead <- c("Live",
                        "Live, insect damaged",
                        "Live, disease damaged",
                        "Live, physically damaged",
                        "Live, other damage",
                        "Live, broken bole",
                        "Standing dead",
                        "Dead, broken bole")

  missingDowned <- c("Removed",
                     "No longer qualifies",
                     "Lost, burned",
                     "Lost, herbivory",
                     "Lost, presumed dead",
                     "Lost, fate unknown",
                     "Downed")

  #   Create 'missingDownedDF" table for later output
  missingDownedDF <- data.frame()



  ### Estimate non-woody biomass: Calculate biomass from vst_non-woody table ####

  ### Conditionally generate non-woody biomass estimates from vst_nonWoody table
  if (methods::is(nonWoody, class = "data.frame" )) {

    #   Create 'source' and 'agb' columns to track allometry citations and record aboveground biomass, respectively
    vst_agb_other <- nonWoody %>%
      dplyr::mutate(source = "missingAllometry",
                    agb = NA)


    ##  Estimate ocotillo biomass: Bobich, E.G., and T.E. Huxman. 2009. Dry mass partitioning and gas exhange for young ocotillos (Fouquieria splendends) in the Sonoran Desert. International Journal of Plant Science 170:283-289. Equations:
    #   log(height_m) = 0.13 + 0.45 * log(total above and below ground biomass in kg)
    #   log(total above and below ground biomass in kg) = (log(height_m) - 0.13)/0.45 = -0.2889 +  (2.2222 * log(height_m))
    #   log(root/shoot) = -0.63 + 0.18 * log(total above and below ground biomass in kg)
    #   aboveground biomass in kg = 1(1+exp(log(root/shoot))) * exp(log(total above and below ground biomass in kg)) = fraction aboveground * total biomass

    #   Estimate total ocotillo mass: aboveground + belowground
    vst_agb_other$tot_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
                                         exp(-0.2889 + 2.2222 * log(vst_agb_other$height)),
                                         NA)

    #   Estimate aboveground ocotillo mass
    vst_agb_other$agb_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
                                         round(1/(exp(-0.63 + 0.18 * log(vst_agb_other$tot_ocotillo)) + 1) *
                                                 vst_agb_other$tot_ocotillo,
                                               digits = 3),
                                         NA)

    #   Remove total ocotillo mass: Belowground estimate not needed
    vst_agb_other$tot_ocotillo <- NULL

    #   Update "agb" column with ocotillo mass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(source = dplyr::case_when(!is.na(.data$agb_ocotillo) ~ "Bobich_Huxman_2009",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_ocotillo) ~ .data$agb_ocotillo,
                                           TRUE ~ .data$agb))


    ##  Estimate Xerophyllum tenax (bear grass) biomass: Gholz, H.L., C.C. Grier, A.G. Campbell, and A.T. Brown. 1979. Equations for estimating biomass and leaf area of plants in the pacific northwest. Research paper 41. Forest Research Laboratory, School of Forestry at Oregon State University, Corvallis. Divide by 1000 to convert output to "kg".

    vst_agb_other$agb_xer <- ifelse(vst_agb_other$growthForm == "xerophyllum",
                                    round((18.873 + (0.0280*((vst_agb_other$basalStemDiameter^2) *
                                                               vst_agb_other$meanLeafLength)))/1000,
                                          digits = 3),
                                    NA)

    #   Update "agb" column with xerophyllum mass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(source = dplyr::case_when(!is.na(.data$agb_xer) ~ "Gholz_etal_1979",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_xer) ~ .data$agb_xer,
                                           TRUE ~ .data$agb))


    ##  Estimate small palm biomass (primarily Serenoa repens): Gholz, H.L., D.N. Guerin, and W.P. Cropper. 1999. Phenology and productivity of saw palmetto (Serenoa repens) in a north Florida slash pine plantation. Canadian Journal of Forest Research 29:1248-1253.
    #   Use separate equations for rachis/petiole biomass (g) and blade/leaf biomass (g). Add together and multiply by leafNumber, then divide by 1000 to get total biomass (kg).
    #   Alexis et al. 2007 Biogeochemistry add petiole length and blade length together to get rachis biomass.

    vst_agb_other$agb_palm <- ifelse(vst_agb_other$growthForm == "small palm" & !is.na(vst_agb_other$meanPetioleLength) &
                                       !is.na(vst_agb_other$meanBladeLength) & !is.na(vst_agb_other$leafNumber),
                                     round((exp(-10.38 + 2.72 * log(vst_agb_other$meanPetioleLength +
                                                                      vst_agb_other$meanBladeLength)) +
                                              (-13.31 + 0.85 * vst_agb_other$meanBladeLength)) *
                                             vst_agb_other$leafNumber / 1000,
                                           digits = 3),
                                     NA)

    #   Update "agb" column with small palm biomass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(source = dplyr::case_when(!is.na(.data$agb_palm) ~ "Gholz_etal_1999",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_palm) ~ .data$agb_palm,
                                           TRUE ~ .data$agb))


    ##  Estimate Cibotium biomass (tree fern): Asner, GP, RF Hughes, J Mascaro, AL Uowolo, DE Knapp, J Jacobson, T Kennedy-Bowdoin, JK Clark. 2011. High-resolution carbon mapping on the million-hectare Island of Hawaii. Frontiers in Ecology and the Environment. Vol 9(8), pp. 434-439; Cibotium and Sadleria wood density (spg_gcm3) also comes from Asner et al. 2011.

    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(agb_Cibotium = dplyr::case_when(grepl("Cibotium", .data$scientificName) &
                                                      .data$growthForm == "large tree fern" ~
                                                      round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.22/1000,
                                                            digits = 2),
                                                    grepl("Cibotium", .data$scientificName) &
                                                      .data$growthForm == "small tree fern" ~
                                                      round(pi * (.data$basalStemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.22/1000,
                                                            digits = 2),
                                                    grepl("Sadleria", .data$scientificName) &
                                                      .data$growthForm == "large tree fern" ~
                                                      round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.5/1000,
                                                            digits = 2),
                                                    grepl("Sadleria", .data$scientificName) &
                                                      .data$growthForm == "small tree fern" ~
                                                      round(pi * (.data$basalStemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.5/1000,
                                                            digits = 2),
                                                    TRUE ~ NA),
                    source = dplyr::case_when(!is.na(.data$agb_Cibotium) ~ "Asner_etal_2011",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_Cibotium) ~ .data$agb_Cibotium,
                                           TRUE ~ .data$agb))



    ### Clean-up of nonWoody data

    ##  Collate missing individuals (removed, lost, downed) and those with agb = "NA" for separate output table
    missingDownedDF <- vst_agb_other %>%
      dplyr::filter(.data$plantStatus %in% missingDowned | is.na(.data$agb)) %>%
      dplyr::select("plot_eventID",
                    "domainID",
                    "siteID",
                    "year",
                    "eventID",
                    "date",
                    "nlcdClass",
                    "plotID",
                    "subplotID",
                    "taxonID",
                    "scientificName",
                    "individualID",
                    "plantStatus",
                    "growthForm",
                    "measurementHeight",
                    "stemDiameter",
                    "basalStemDiameter",
                    "height",
                    "stemLength",
                    "maxCrownDiameter",
                    "ninetyCrownDiameter",
                    "source",
                    "agb") %>%
      dplyr::bind_rows(missingDownedDF) %>%
      dplyr::rename("agb_kg" = "agb")


    ##  Retain standing individuals in the plot with agb != NA that are unambiguously 'alive' or 'dead' according to plantStatus and create 'simplePlantStatus' variable. Removing NA records avoids misinterpreting as "0" mass in later steps. Cactus and ferns removed because no allometries are applied to these individuals.
    vst_agb_other <- vst_agb_other %>%
      dplyr::filter(.data$plantStatus %in% standingLiveDead,
                    !is.na(.data$agb),
                    !.data$growthForm %in% c("cactus", "fern")) %>%
      dplyr::mutate(simplePlantStatus = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                         TRUE ~ "dead"))


    ##  Aggregate vst non-herbaceous perennial (other) biomass data (multiple records associated with multi-stem individuals)
    if (nrow(vst_agb_other) > 0) {

      vst_agb_final_other <- vst_agb_other %>%
        dplyr::group_by(.data$plot_eventID,
                        .data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$eventID,
                        .data$date,
                        .data$nlcdClass,
                        .data$plotID,
                        .data$subplotID,
                        .data$taxonID,
                        .data$scientificName,
                        .data$individualID,
                        .data$simplePlantStatus,
                        .data$growthForm,
                        .data$totalSampledAreaTrees,
                        .data$totalSampledAreaOther) %>%
        dplyr::summarise(source = paste(unique(.data$source), collapse = ", "),
                         agb_kg = sum(.data$agb, na.rm = TRUE),
                         .groups = "drop")


      ##  Assign total sampled area for each individual based on growthForm
      #--> palm tree and large tree fern individuals sampled throughout plot like trees.
      vst_agb_final_other <- vst_agb_final_other %>%
        dplyr::mutate(sampledArea_m2 = dplyr::case_when(.data$growthForm %in% c("palm tree", "large tree fern") ~
                                                          .data$totalSampledAreaTrees,
                                                        TRUE ~ .data$totalSampledAreaOther))


      ##  Remove unneeded totalSampledArea columns
      vst_agb_final_other <- vst_agb_final_other %>%
        dplyr::select(-"totalSampledAreaTrees",
                      -"totalSampledAreaOther")

    } else {

      vst_agb_final_other <- data.frame()

    } # end nrow(vst_agb_other) conditional

  } #   end non-woody conditional



  ### Estimate woody biomass: Calculate biomass for individuals in vst_apparentindividual table ####

  ##  Read in the Chojnacky et al 2014 parameters for each of their 35 defined allometric groups
  parameters <- parameters %>%
    dplyr::select("allometry_ID",
                  "b0",
                  "b1",
                  "minDiameter",
                  "maxDiameter")


  ##  Load wood density, veg type, and other data needed to assign species to Chojnacky allometry groups
  taxon_fields <- taxon_fields
  taxon_fields_list <- unique(taxon_fields$taxonID)


  ##  Load USDA Plants characteristics to get PLANTS.Floristic.Area and Native.Status: Filtered to records that have PLANTS.Floristic.Area, Native.Status, or both
  plantIntTrop <- plantIntTrop

  #   Add tropical floristic area and/or introduced status to taxa derived from vst_mappingandtagging data
  plant_char <- merge(taxonID_df,
                      plantIntTrop,
                      by = "taxonID",
                      all.x = TRUE)


  ##  Programatically assign a Chojnacky allometry_ID based on genus, family, specific gravity, deciduous vs. evergreen, and/or woodland vs. forest habit
  Choj <- merge(taxon_fields,
                plant_char,
                by = "taxonID",
                all = TRUE)

  #   Retain only taxonIDs found in the vst_mappingandtagging data
  Choj <- Choj[Choj$taxonID %in% vst_taxonIDs, ]

  #   Stanardize 'nativeStatus' and 'tropical' LOV elements
  Choj$nativeStatus <- dplyr::if_else(Choj$nativeStatus == "int",
                                      "introduced",
                                      "native",
                                      "native")

  Choj$tropical <- dplyr::if_else(Choj$tropical == "trop",
                                  "tropical",
                                  "temperate",
                                  "temperate")


  ##  Assign Chojnacky allometric equation IDs
  Choj <- Choj %>%
    dplyr::mutate(allometry_ID = dplyr::case_when(

      .data$woodland_vs_forest == "forest" & .data$genus == "Abies" & .data$spg_gcm3 < 0.35 ~ "C1",
      .data$woodland_vs_forest == "forest" & .data$genus == "Abies" & .data$spg_gcm3 >= 0.35 ~ "C2",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" & .data$spg_gcm3 < 0.30 ~ "C3",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" &
        .data$spg_gcm3 >= 0.30 & .data$spg_gcm3 < 0.40 ~ "C4",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" & .data$spg_gcm3 >= 0.40 ~ "C5",
      .data$woodland_vs_forest == "forest" & .data$genus == "Larix" ~ "C6",
      .data$woodland_vs_forest == "forest" & .data$genus == "Picea" & .data$spg_gcm3 < 0.35 ~ "C7",
      .data$woodland_vs_forest == "forest" & .data$genus == "Picea" & .data$spg_gcm3 >= 0.35 ~ "C8",
      .data$woodland_vs_forest == "forest" & .data$genus == "Pinus" & .data$spg_gcm3 < 0.45 ~ "C9",
      .data$woodland_vs_forest == "forest" & .data$genus == "Pinus" & .data$spg_gcm3 >= 0.45 ~ "C10",
      .data$woodland_vs_forest == "forest" & .data$genus %in% c("Pseudotsuga", "Taxus") ~ "C11",
      .data$woodland_vs_forest == "forest" & .data$genus == "Tsuga" & .data$spg_gcm3 < 0.40 ~ "C12",
      .data$woodland_vs_forest == "forest" & .data$genus == "Tsuga" & .data$spg_gcm3 >= 0.40 ~ "C13",
      .data$woodland_vs_forest  %in% c("forest", "") & .data$family == "Aceraceae" & .data$spg_gcm3 < 0.50 ~ "H1",
      .data$woodland_vs_forest %in% c("forest", "") & .data$family == "Aceraceae" & .data$spg_gcm3 >= 0.50 ~ "H2",
      .data$family == "Betulaceae" & .data$spg_gcm3 < 0.40 ~ "H3",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.40 & .data$spg_gcm3 < 0.50 ~ "H4",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.50 & .data$spg_gcm3 < 0.60 ~ "H5",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.60 ~ "H6",
      .data$family %in% c("Cornaceae", "Ericaceae", "Lauraceae", "Platanaceae", "Rosaceae", "Ulmaceae") ~ "H7",
      .data$woodland_vs_forest == "forest" & .data$genus == "Carya" ~ "H8",
      .data$woodland_vs_forest == "forest" & .data$family %in% c("Fabaceae", "Juglandaceae") & .data$genus != "Carya" ~ "H9",
      .data$woodland_vs_forest == "forest" & .data$family == "Fagaceae" & .data$decid_vs_ever == "decid" ~ "H10",
      .data$woodland_vs_forest == "forest" & .data$family == "Fagaceae" & .data$decid_vs_ever == "ever" ~ "H11",
      .data$family == "Hamamelidaceae" ~ "H12",
      .data$family %in% c("Hippocastanaceae", "Tiliaceae") ~ "H13",
      .data$family == "Magnoliaceae" ~ "H14",
      .data$family == "Oleaceae" & .data$spg_gcm3 < 0.55 ~ "H15",
      .data$family == "Oleaceae" & .data$spg_gcm3 >= 0.55 ~ "H16",
      .data$family == "Salicaceae" & .data$spg_gcm3 < 0.35 ~ "H17",
      .data$family == "Salicaceae" & .data$spg_gcm3 >= 0.35 ~ "H18",
      .data$woodland_vs_forest == "woodland" & .data$family == "Cupressaceae" ~ "W1",
      .data$woodland_vs_forest == "woodland" & .data$family %in% c("Fabaceae", "Rosaceae") ~ "W2",
      .data$woodland_vs_forest == "woodland" & .data$family == "Fagaceae" ~ "W3",
      .data$woodland_vs_forest == "woodland" & .data$family == "Pinaceae" ~ "W4",
      #   Arbitrarily picked C9 (forest) over C10 (forest spg_gcm3>=0.45) or W4 (woodland)
      .data$taxonID == "PINACE" ~ "C9",
      #   Arbitrarily picked H9 (forest) over W2 (woodland)
      .data$taxonID == "FABACE" ~ "H9",
      TRUE ~ NA

    )) %>%
    dplyr::relocate("allometry_ID",
                    "family",
                    "genus",
                    .before = "taxonID") %>%

    #   Identify taxa not in Chojnacky
    dplyr::mutate(source = ifelse(!is.na(.data$allometry_ID),
                                  "Chojnacky_etal_2014",
                                  "missingAllometry")) %>%

    #   Reduce 'Choj' to desired columns
    dplyr::select("allometry_ID",
                  "family",
                  "genus",
                  "taxonID",
                  "spg_gcm3",
                  "scientificName",
                  "nativeStatus",
                  "tropical",
                  "source")


  ##  Merge 'Choj' to associate taxonIDs in data with allometric parameters
  Choj <- merge(parameters,
                Choj,
                by = "allometry_ID",
                all.y = TRUE)



  ### Prepare 'vst_agb' for biomass estimation by taxonID
  vst_agb <- merge(appInd,
                   Choj,
                   by = "taxonID",
                   all.x = TRUE)

  #   Manually assign 'tropical' and 'temperate' status for a subset of taxonIDs
  vst_agb <- vst_agb %>%
    dplyr::mutate(tropical = dplyr::case_when(.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("2PLANT", "2PLANT-H", "ANAL12", "BOURR", "BUMI6",
                                                                     "CONVOL", "CROSS", "FABACE", "JACQU", "JACQU2",
                                                                     "COPRO", "HYDRAN") ~ "tropical",
                                              TRUE ~ .data$tropical))

  vst_agb <- vst_agb %>%
    dplyr::mutate(tropical = dplyr::case_when(!.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("AMAR5", "CELTI", "DAWR2", "LIJA", "MEAZ", "OPUNT",
                                                                     "RHUS", "SAMBU", "SMSM", "SYMPL2", "VITIS")
                                              ~ "temperate",
                                              TRUE ~ .data$tropical))

  #   Assign specific gravity data type
  vst_agb$spg_gcm3 <- as.numeric(vst_agb$spg_gcm3)

  #   Correct negative ninetyCrownDiameter: Meaningless and generates NaN warnings with some allometric equations
  vst_agb$ninetyCrownDiameter <- dplyr::if_else(vst_agb$ninetyCrownDiameter < 0,
                                                NA,
                                                vst_agb$ninetyCrownDiameter)

  #   Assumption: For tropical species, if specific gravity is not known then assume it is 0.5 g/cm3 to permit usage of Chave et al 2014, following precedent of Asner et al 2011
  vst_agb$spg_gcm3 <- dplyr::if_else(is.na(vst_agb$spg_gcm3) & vst_agb$tropical == "tropical",
                                     0.5,
                                     vst_agb$spg_gcm3,
                                     vst_agb$spg_gcm3)

  #   Select columns to remove unneeded data
  vst_agb <- vst_agb %>%
    dplyr::select("plot_eventID",
                  "domainID",
                  "siteID",
                  "plotID",
                  "subplotID",
                  "taxonID",
                  "family",
                  "genus",
                  "scientificName",
                  "individualID",
                  "year",
                  "eventID",
                  "date",
                  "growthForm",
                  "nlcdClass",
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana",
                  "plantStatus",
                  "height",
                  "measurementHeight",
                  "stemDiameter",
                  "basalStemDiameter",
                  "basalStemDiameterMsrmntHeight",
                  "maxCrownDiameter",
                  "ninetyCrownDiameter",
                  "allometry_ID",
                  "b0",
                  "b1",
                  "minDiameter",
                  "maxDiameter",
                  "spg_gcm3",
                  "nativeStatus",
                  "tropical",
                  "source")



  ### Multi-bole trees: Assume that 'height' of individual that is measured for primary bole applies to secondary boles. Secondary 'mbt' boles at PUUM with no 'height' would otherwise have AGB = NA since Chave "E" parameter unavailable for PUUM and "E" is needed to estimate 'height' required by Chave allometry when 'height' is missing.
  #   Separate 'mbt' from other growth forms
  nonMbt <- vst_agb %>%
    dplyr::filter(.data$growthForm != "multi-bole tree" | is.na(.data$growthForm))

  mbt <- vst_agb %>%
    dplyr::filter(.data$growthForm == "multi-bole tree") %>%
    dplyr::mutate(tempIndivID = stringr::str_extract(string = .data$individualID,
                                                     pattern = "^NEON.PLA.D[0-9]{2}.[A-Z]{4}.[0-9]{5}"),
                  .before = "individualID")

  #   Assign height and crown dimensions from primary bole to secondary boles
  heightCrownMBT <- mbt %>%
    dplyr::group_by(.data$tempIndivID) %>%
    dplyr::summarise(height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA),
                     maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                               max(.data$maxCrownDiameter, na.rm = TRUE),
                                               NA),
                     ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                  max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                  NA))

  mbt <- dplyr::left_join(mbt %>%
                            dplyr::select(-"height",
                                          -"maxCrownDiameter",
                                          -"ninetyCrownDiameter"),
                          heightCrownMBT,
                          by = "tempIndivID") %>%
    dplyr::relocate("height",
                    .after = "plantStatus") %>%
    dplyr::relocate("maxCrownDiameter":"ninetyCrownDiameter",
                    .before = "allometry_ID") %>%
    dplyr::select(-"tempIndivID")

  vst_agb <- dplyr::bind_rows(nonMbt,
                              mbt)



  ### Shrubs: Combine emergent boles for use with Conti et al. 2019 allometries
  #   Separate shrubs from other growthForms to calculate aggregated basalStemDiameter inputs for Conti
  nonShrub <- vst_agb %>%
    dplyr::filter(!.data$growthForm %in% c("single shrub", "small shrub") | is.na(.data$growthForm))

  shrub <- vst_agb %>%
    dplyr::filter(.data$growthForm %in% c("single shrub", "small shrub"))

  #   For shrubs, height and crown dimensions are measured once per individualID; apply these measurements to all emergent boles so that separate live and dead biomass estimates can be generated. Crown dimensions are particularly important for some taxon-specific allometries (e.g., ARTR2).
  heightCrownShrub <- shrub %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA),
                     maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                               max(.data$maxCrownDiameter, na.rm = TRUE),
                                               NA),
                     ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                  max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                  NA))

  #   Group multiple stems belonging to same individualID x plantStatus combination, and calculate equivalent stemDiameter and basalStemDiameter.
  shrub <- shrub %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$subplotID,
                    .data$taxonID,
                    .data$family,
                    .data$genus,
                    .data$scientificName,
                    .data$individualID,
                    .data$year,
                    .data$eventID,
                    .data$date,
                    .data$growthForm,
                    .data$nlcdClass,
                    .data$totalSampledAreaTrees,
                    .data$totalSampledAreaShrubSapling,
                    .data$totalSampledAreaLiana,
                    .data$plantStatus,
                    .data$allometry_ID,
                    .data$b0,
                    .data$b1,
                    .data$minDiameter,
                    .data$maxDiameter,
                    .data$spg_gcm3,
                    .data$nativeStatus,
                    .data$tropical,
                    .data$source) %>%
    dplyr::summarise(stemDiameter = ifelse(!all(is.na(.data$stemDiameter)),
                                           round(sqrt(sum(.data$stemDiameter^2)),
                                                 digits = 1),
                                           NA),
                     basalStemDiameter = ifelse(!all(is.na(.data$basalStemDiameter)),
                                                round(sqrt(sum(.data$basalStemDiameter^2)),
                                                      digits = 1),
                                                NA),
                     measurementHeight = ifelse(!all(is.na(.data$measurementHeight)),
                                                round(mean(.data$measurementHeight, na.rm = TRUE),
                                                      digits = 0),
                                                NA),
                     basalStemDiameterMsrmntHeight = ifelse(!all(is.na(.data$basalStemDiameterMsrmntHeight)),
                                                            round(mean(.data$basalStemDiameterMsrmntHeight, na.rm = TRUE),
                                                                  digits = 0),
                                                            NA),
                     .groups = "drop")

  ##  Join with 'heightCrownShrub' to assign crown dimensions based on individualID
  shrub <- dplyr::left_join(shrub,
                            heightCrownShrub,
                            by = "individualID")

  rm(heightCrownShrub)


  ##  Bind 'nonShrub' and 'shrub' together into simplified dataframe
  vst_agb <- dplyr::bind_rows(nonShrub,
                              shrub)

  rm(nonShrub, shrub)



  ### Calculate AGB for each VST appInd record using Choj allometry_ID and Choj parameters

  # Assumption: Chojnacky et al 2014 allometric equations are the best first estimate of biomass
  vst_agb$agb <- round(exp(vst_agb$b0 + vst_agb$b1 * log(vst_agb$stemDiameter)),
                       digits = 2)

  #   Assign Chojnacky AGB estimates to specific column; needed to preserve Chojnacky estimates when alternate is used for tropical or introduced species.
  vst_agb$agb_Chojnacky  <- vst_agb$agb

  #   Assumption: When the necessary ancillary variables are available for tropical species, replace the Chojnacky et al 2014 biomass estimates with the Chave et al 2014 biomass estimates.
    # Update tropical species records based on Chave et al 2014 if wood specific gravity is available (or an approximation based on congeners).
    # Instructions on extracting environmental stress value E at http://chave.ups-tlse.fr/pantropical_allometry.html; Chave et al 2014 has pantropical allometric equations for tree biomass that require tree height. If tree height is not available, estimate it using their value E.
  #   Chave et al 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20:3177-3190
  # install.packages("raster"); install.packages("ncdf4"); library("raster"); library("ncdf4")
  # source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
  # coord <- data.frame(siteID = c("GUAN", "LAJA", "PUUM"), longitude = c(-66.8687, -67.07689, -155.31731), latitude = c(17.96955, 18.02126, 19.55309) );  rownames(coord) <- coord$siteID; coord$siteID <- NULL
  # Chave_et_al_2014_E <- retrieve_raster("E",coord,plot=TRUE,format="nc") returns an E of 0.5074847 for GUAN, 0.4440793 for LAJA, and NA for PUUM

  #   Assign Chave et al 2014 "E" values needed for site-specific height estimation when height is missing
  vst_agb$Chave_E <- ifelse(vst_agb$siteID == "GUAN",
                            0.5074847,
                            NA)

  vst_agb$Chave_E <- ifelse(vst_agb$siteID == "LAJA",
                            0.4440793,
                            vst_agb$Chave_E)


  ##  Estimate AGB for tropical species: Different equations with 'height' and without 'height'
  vst_agb <- vst_agb %>%
    dplyr::mutate(agb_trop = dplyr::case_when(!dplyr::if_any(c("height", "stemDiameter", "spg_gcm3"), is.na) &
                                                .data$tropical == "tropical" ~
                                                round(0.0673 * (vst_agb$spg_gcm3 * (vst_agb$stemDiameter^2) *
                                                                  vst_agb$height)^0.976,
                                                      digits = 2),

                                              # Estimate when 'height' missing
                                              is.na(.data$height) & !dplyr::if_any(c("stemDiameter", "spg_gcm3"), is.na) &
                                                .data$tropical == "tropical" ~
                                                round(exp(-1.803 - (0.976 * vst_agb$Chave_E) +
                                                            (0.976 * log(vst_agb$spg_gcm3)) +
                                                            (2.673 * log(vst_agb$stemDiameter)) -
                                                            (0.0299 * (log(vst_agb$stemDiameter))^2)),
                                                      digits = 2),

                                              TRUE ~ NA)) %>%

    #   Assign allometry source for tropical species
    dplyr::mutate(source = dplyr::case_when(is.na(.data$agb_trop) ~ .data$source,
                                            TRUE ~ "Chave_etal_2014")) %>%

    #   Update "agb" biomass column with Chave estimates for tropical species
    dplyr::mutate(agb = dplyr::case_when(is.na(.data$agb_trop) ~ .data$agb,
                                         TRUE ~ .data$agb_trop))



  ### Apply shrub-specific biomass equations from Conti et al. 2019 to shrub growth forms
  # Note: Conti et al. 2019 assume that multiple stems of same individual have been aggregated into a single equivalent basalStemDiameter for all stems; this calculation was performed above on the 'shrub' dataframe subset.

  #   Calculate mean crown diameter for shrubs using max/ninetyCrownDiameter inputs; confirmed with G. Conti that geometric mean might be more appropriate but arithmetic mean was used to construct allometries.
  vst_agb <- vst_agb %>%
    dplyr::mutate(meanCrownDiameter = dplyr::case_when(.data$growthForm %in% c("single shrub", "small shrub") &
                                                         !is.na(.data$maxCrownDiameter) &
                                                         !is.na(.data$ninetyCrownDiameter) ~
                                                         round(rowMeans(dplyr::across(c("maxCrownDiameter",
                                                                                        "ninetyCrownDiameter")),
                                                                        na.rm = TRUE),
                                                               digits = 1),
                                                       TRUE ~ NA),
                  .after = "ninetyCrownDiameter")


  #   Estimate shrub biomass: Case when basalStemDiameter is missing and mean crownDiameter and height are available (biomass estimate with most uncertainty)
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$meanCrownDiameter) & !is.na(vst_agb$height),
                              round(exp(-0.370 + 1.903 * log(vst_agb$meanCrownDiameter) +
                                          0.652 * log(vst_agb$height)) * 1.403,
                                    digits = 2),
                              NA)

  #   Estimate shrub biomass: Improved output when basalStemDiameter is available --> less uncertainty
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$basalStemDiameter),
                              round(exp(-2.869 + 2.584 * log(vst_agb$basalStemDiameter)),
                                    digits = 2),
                              vst_agb$agb_shrub)

  #   Estimate shrub biomass: Even better output when basalStemDiameter AND mean crownDiameter available (compared to basalStemDiameter alone)
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$meanCrownDiameter) & !is.na(vst_agb$basalStemDiameter),
                              round(exp(-2.057 + 1.741 * log(vst_agb$basalStemDiameter) + 0.945 *
                                          log(vst_agb$meanCrownDiameter)),
                                    digits = 2),
                              vst_agb$agb_shrub)

  #   Estimate shrub biomass: Best output when basalStemDiameter, mean crownDiameter, AND height are all available
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$meanCrownDiameter) & !is.na(vst_agb$basalStemDiameter) &
                                !is.na(vst_agb$height),
                              round(exp(-2.281 + 1.525 * log(vst_agb$basalStemDiameter) + 0.831 *
                                          log(vst_agb$meanCrownDiameter) + 0.523 * log(vst_agb$height)),
                                    digits = 2),
                              vst_agb$agb_shrub)

  #   Assign AGB allometry source for shrubs. Citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.
  vst_agb$source <- ifelse(!is.na(vst_agb$agb_shrub),
                           "Conti_etal_2019",
                           vst_agb$source)

  #   Update AGB column with shrub biomass from Conti
  vst_agb$agb <- dplyr::if_else(vst_agb$source == "Conti_etal_2019",
                                vst_agb$agb_shrub,
                                vst_agb$agb,
                                vst_agb$agb)



  ### Assumption: Where available, species-specific allometric equations are preferable to more generic ones; update AGB estimates for taxa for which species-specific allometric equations exist

  ##  Species: Metrosideros polymorpha (MEPO5) - first estimate AGB for all MEPO5 with a stemDiameter (Litton and Kauffman 2008), then in subsequent steps update the AGB estimate with Selmants et al 2014; approach retains Litton and Kauffman 2008 estimate for those MEPO5 with DBH >= 33 cm and that do not have 'height' recorded. All other individuals have an AGB estimate via Selmants et al 2014.

  #   Allometry for all MEPO5 individuals; citation: Litton and Kauffman 2008. Allometric Models for Predicting Aboveground Biomass in Two Widespread WoodyPlants in Hawaii. BIOTROPICA 40(3): 313-320.
  vst_agb$agb_MEPO5_Litton <- ifelse(vst_agb$taxonID == "MEPO5" & !is.na(vst_agb$stemDiameter),
                                     round(0.88 * (vst_agb$stemDiameter^1.86),
                                           digits = 2),
                                     NA)

  vst_agb$source <- ifelse(!is.na(vst_agb$agb_MEPO5_Litton),
                           "Litton_Kauffman_2008_MEPO5",
                           vst_agb$source)

  vst_agb$agb <- ifelse(vst_agb$source == "Litton_Kauffman_2008_MEPO5",
                        vst_agb$agb_MEPO5_Litton,
                        vst_agb$agb)

  #   Update MEPO5 AGB estimate for individuals with DBH <= 33 cm, or > 33 cm AND with 'height' data; citation: Selmants, PC, CM Litton, CP Giardina, and GP Asner. 2014. Global Change Biology 20:2927-2937.
  vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter <= 33,
                              round(0.2085 * (vst_agb$stemDiameter^2.318),
                                    digits = 2),
                              NA)

  vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter > 33 &
                                !is.na(vst_agb$height) & !is.na(vst_agb$spg_gcm3),
                              round(0.0776 * ((vst_agb$spg_gcm3 * (vst_agb$stemDiameter^2) * vst_agb$height)^0.94),
                                    digits = 2),
                              vst_agb$agb_MEPO5)

  #   Update AGB allometry for MEPO5 that have a new value in "agb_MEPO5" column
  vst_agb$source <- ifelse(!is.na(vst_agb$agb_MEPO5),
                           "Selmants_etal_2014_MEPO5",
                           vst_agb$source)

  #   Update "agb" column with Selmants et al 2014 estimates
  vst_agb$agb <- ifelse(vst_agb$source == "Selmants_etal_2014_MEPO5",
                        vst_agb$agb_MEPO5,
                        vst_agb$agb)


  ##  Species: Rhamnus davurica (RHDA); citation: Zhang et al 2012. Sexual dimorphism in reproductive and vegetative allometry for two dioecious Rhamnus plants in north-eastern China. Eur J Forest Res (2012) 131:1287-1296.
   # The taxonID RHDA is the most frequent introduced species in NEON VST dataset, and Zhang et al 2012 have a specific equation for RHDA. There is one equation for males and another for females; here, we take the average because NEON does not record sex of RHDA. Output is divided by 1000 to convert to "kg".
  vst_agb <- vst_agb %>%
    dplyr::mutate(agb_RHDA = dplyr::case_when(.data$taxonID == "RHDA" & !is.na(.data$stemDiameter) ~
                                                round(0.001 * ((exp(5.237 + 1.996 * log(.data$stemDiameter)) +
                                                                  exp(5.016 + 2.306 * log(.data$stemDiameter))) / 2),
                                                      digits = 2),
                                              TRUE ~ NA)) %>%

    #   Update AGB allometry citation for RHDA individuals
    dplyr::mutate(source = ifelse(!is.na(.data$agb_RHDA),
                                  "Zhang_etal_2012_RHDA",
                                  .data$source)) %>%

    #   Update "agb" column with Zhang et al 2012 estimates
    dplyr::mutate(agb = ifelse(.data$source == "Zhang_etal_2012_RHDA",
                               .data$agb_RHDA,
                               .data$agb))


  ##  Species: Cornus spp; citation: Lutz, J.A., K.A. Schwindt, T.J. Furniss, J.A. Freund, M.E Swanson, K.J. Hogan, G.E. Kenagy, and A.J. Larson. 2014. Community composition and allometry of Leucothoe davisiae, Cornus sericea, and Chrysolepis sempervirens. Canadian Journal of Forest Research 44:677-683. Output divided by 1000 to convert to "kg".

  #   Estimate AGB for individuals with a basalStemDiameter; most emergent shrub stems have basalStemDiameter but a small number of are occluded from measurement.
  vst_agb <- vst_agb %>%
    dplyr::mutate(agb_Cornus = dplyr::case_when(grepl("Cornus", .data$scientificName) & !is.na(.data$basalStemDiameter) &
                                                  .data$growthForm %in% c("single shrub", "small shrub") &
                                                  is.na(.data$stemDiameter) ~
                                                  round(exp(3.315 + 2.647 * log(.data$basalStemDiameter)) / 1000,
                                                        digits = 3),
                                                grepl("Cornus", .data$scientificName) & !is.na(.data$stemDiameter) &
                                                  .data$growthForm == "single shrub" ~
                                                  round(exp(5.089 + 1.883 * log(.data$stemDiameter)) / 1000,
                                                        digits = 3),
                                                TRUE ~ NA),

                  #   Update AGB allometry citation for Cornus individuals
                  source = dplyr::case_when(!is.na(.data$agb_Cornus) ~ "Lutz_etal_2014_Cornus",
                                                TRUE ~ .data$source),

                  #   Update "agb" column with Lutz et al 2014 estimates
                  agb = dplyr::case_when(!is.na(.data$agb_Cornus) ~ .data$agb_Cornus,
                                         TRUE ~ .data$agb))


  ##  Assumption: Allometric equations developed specifically for lianas are better than generic allometric equations used above for trees and shrubs. Citation: Schnitzer, SA, SJ DeWalt, and J Chave. 2006. Censusing and measuring lianas: A quantitative comparison of the common methods. Biotropica 38:581-591.
  #   Update AGB for lianas with equations from Schnitzer_et_al_2006 (Chojnacky is not intended for lianas, or for introduced or tropical species, and there are numerous introduced and tropical liana species, see below). Equation for tropical lianas is used for temperate liana species.
  vst_agb$agb_liana <- ifelse(vst_agb$growthForm == "liana" & !is.na(vst_agb$stemDiameter),
                              round(exp(-1.484 + 2.657 * log(vst_agb$stemDiameter)),
                                    digits = 3),
                              NA)

  #   Update AGB allometry citation for lianas
  vst_agb$source <- ifelse(!is.na(vst_agb$agb_liana),
                           "Schnitzer_etal_2006",
                           vst_agb$source)

  #   Update AGB column with Schnitzer et al 2006 estimates
  vst_agb$agb <- ifelse(!is.na(vst_agb$agb_liana),
                        vst_agb$agb_liana,
                        vst_agb$agb)


  ##  Retain only those records with unambiguous live or dead plantStatus values that contribute to standing AGB
  #   Identify missing and downed individuals and bind to missing/downed from vst_non-woody
  missingDownedDF <- dplyr::bind_rows(missingDownedDF,
                           vst_agb %>%
                             dplyr::filter(.data$plantStatus %in% missingDowned | is.na(.data$growthForm) |
                                             is.na(.data$agb)) %>%
                             dplyr::rename("agb_kg" = "agb") %>%
                             dplyr::select("plot_eventID",
                                           "domainID",
                                           "siteID",
                                           "year",
                                           "eventID",
                                           "date",
                                           "nlcdClass",
                                           "plotID",
                                           "subplotID",
                                           "taxonID",
                                           "scientificName",
                                           "individualID",
                                           "plantStatus",
                                           "growthForm",
                                           "measurementHeight",
                                           "stemDiameter",
                                           "basalStemDiameter",
                                           "height",
                                           "maxCrownDiameter",
                                           "ninetyCrownDiameter",
                                           "source",
                                           "agb_kg"))

  #   Add 'eventType', 'plotType', and 'dataCollected' from perPlot table
  missingDownedDF <- dplyr::left_join(missingDownedDF,
                                      perPlot %>%
                                        dplyr::select("plot_eventID",
                                                      "plotType",
                                                      "eventType",
                                                      "dataCollected"),
                                      by = "plot_eventID") %>%
    dplyr::relocate("plotType":"dataCollected",
                    .before = "plotID") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID,
                   .data$individualID)

  #   Retain live and dead individuals that contribute to standing AGB; create 'simplePlantStatus' == "live|dead"
  vst_agb <- vst_agb %>%
    dplyr::filter(.data$plantStatus %in% standingLiveDead,
                  !is.na(.data$agb)) %>%
    dplyr::mutate(simplePlantStatus = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                       TRUE ~ "dead"),
                  .after = "plantStatus")


   ##  Aggregate woody biomass data by "individualID' x 'simplePlantStatus' x 'year'; assumes that multiple instances of same individualID are true multiple boles and not accidental duplicates. Output is used for both annual biomass summaries and NPP calculations for specified consecutive years.
  vst_agb_kg <- vst_agb %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$date,
                    .data$nlcdClass,
                    .data$plotID,
                    .data$subplotID,
                    .data$taxonID,
                    .data$family,
                    .data$genus,
                    .data$scientificName,
                    .data$individualID,
                    .data$simplePlantStatus,
                    .data$growthForm,
                    .data$totalSampledAreaTrees,
                    .data$totalSampledAreaShrubSapling,
                    .data$totalSampledAreaLiana) %>%
    dplyr::summarise(stemDiameter = round(sqrt(sum(.data$stemDiameter^2)),
                                          digits = 1),
                     basalStemDiameter = round(sqrt(sum(.data$basalStemDiameter^2)),
                                               digits = 1),
                     height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA),
                     maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                               max(.data$maxCrownDiameter, na.rm = TRUE),
                                               NA),
                     ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                  max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                  NA),
                     source = paste(unique(.data$source), collapse = ", "),
                     agb_kg = sum(.data$agb, na.rm = TRUE),
                     .groups = "drop")


  ##  Assign total sampled area by growthForm into a single column
  vst_agb_kg <- vst_agb_kg %>%
    dplyr::mutate(sampledArea_m2 = dplyr::case_when(growthForm %in% c("single bole tree",
                                                                      "multi-bole tree") ~ .data$totalSampledAreaTrees,
                                                    growthForm %in% c("single shrub",
                                                                      "small shrub",
                                                                      "small tree",
                                                                      "sapling") ~ .data$totalSampledAreaShrubSapling,
                                                    growthForm == "liana" ~ .data$totalSampledAreaLiana,
                                                    TRUE ~ NA))


  ##  Remove unneeded totalSampledArea columns
  vst_agb_kg <- vst_agb_kg %>%
    dplyr::select(-"totalSampledAreaTrees",
                  -"totalSampledAreaShrubSapling",
                  -"totalSampledAreaLiana")


  ##  Combine AGB for vst_apparentindividual and vst_nonWoody
  if (nrow(vst_agb_final_other) > 0) {

    vst_agb_kg <- dplyr::bind_rows(vst_agb_kg,
                                   vst_agb_final_other)

  }


  ##  Join with perPlot to obtain 'plotType', 'eventType', and 'dataCollected' fields and arrange for output
  vst_agb_kg <- dplyr::left_join(vst_agb_kg,
                           perPlot %>%
                             dplyr::select("plot_eventID",
                                           "plotType",
                                           "eventType",
                                           "dataCollected"),
                           by = "plot_eventID") %>%
    dplyr::relocate("plotType":"dataCollected",
                    .before = "plotID") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID,
                   .data$individualID)



  ### Scaling: Determine biomass per unit area (Mg/ha) ####

  ##  Identify records with full sampling of each plot --> remove records that cannot be scaled to a per area basis, and create "Mg/ha" estimate for downstream plot- and site-level outputs; filtering only on is.na(sampledArea_m2) should be sufficient, and "dataCollected" filter is insurance.

  vst_agb_Mgha <- vst_agb_kg %>%

    #   Conditional growthFormSubset filtering: Include dataCollected == "partial" when only estimating "trees", as "partial" means trees were sampled throughout the plot but no smaller growth forms.
    dplyr::filter(dplyr::case_when(growthFormSubset == "tree" ~
                                     (.data$dataCollected != "dendrometerOnly" | is.na(.data$dataCollected)) &
                                     !is.na(.data$sampledArea_m2) & .data$sampledArea_m2 > 0,
                                   growthFormSubset == "all" ~ !.data$dataCollected %in% c("dendrometerOnly", "partial") &
                                     !is.na(.data$sampledArea_m2) & .data$sampledArea_m2 > 0)) %>%

    #   Calculate "Mg/ha" AGB for each individual with full plot sampling
    dplyr::mutate(agb_Mgha = round(.data$agb_kg * 0.001 * (10000 / .data$sampledArea_m2),
                                   digits = 4),
                  .after = "agb_kg")


  ##  Identify 'plot_eventIDs' with full sampling and no qualifying biomass
  #   Create list of 'plot_eventID' values from fully sampled plots that have downstream 'appInd' data
  plot_eventID_appInd <- unique(vst_agb_Mgha$plot_eventID)

  #   Create list of 'plot_eventID' values for fully sampled plots with NO downstream 'appInd' data
  plot_eventID_zeros <- setdiff(plot_eventID_full, plot_eventID_appInd)

  #   Create data frame of zeros via filtering 'perPlot' table
  if (length(plot_eventID_zeros) > 0) {

    vst_agb_zeros <- perPlot %>%
      dplyr::filter(.data$plot_eventID %in% plot_eventID_zeros) %>%
      dplyr::select(-"totalSampledAreaTrees",
                    -"totalSampledAreaShrubSapling",
                    -"totalSampledAreaLiana",
                    -"totalSampledAreaOther") %>%
      dplyr::mutate(live_Mgha = 0,
                    dead_Mgha = 0)

  } else {

    vst_agb_zeros <- data.frame()

  }



  ### Generate plot-level biomass summary ####

  #   Sum biomass per unit area for each 'plotID' x 'year' x 'simplePlantStatus' x 'nlcdClass': Aggregate across individualIDs
  vst_plot_summary <- vst_agb_Mgha %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$eventType,
                    .data$dataCollected,
                    .data$plotID,
                    .data$simplePlantStatus) %>%
    dplyr::summarise(agb_Mgha = round(sum(.data$agb_Mgha, na.rm = TRUE),
                                      digits = 2),
                     .groups = "drop")

  #   Within a given year, transpose live and dead AGB into separate columns
  vst_plot_summary <- tidyr::pivot_wider(vst_plot_summary,
                                      id_cols = c("plot_eventID",
                                                  "domainID",
                                                  "siteID",
                                                  "year",
                                                  "eventID",
                                                  "nlcdClass",
                                                  "plotType",
                                                  "eventType",
                                                  "dataCollected",
                                                  "plotID"),
                                      names_from = "simplePlantStatus",
                                      names_glue = "{simplePlantStatus}_Mgha",
                                      values_from = "agb_Mgha")

  #   Add column "live_Mgha' if missing; may happen if all standing biomass in plot is dead
  if (!"live_Mgha" %in% names(vst_plot_summary)) {

    vst_plot_summary$live_Mgha <- NA

  }

  #   Add column 'dead_Mgha' if missing; may happen if all standing biomass plot is live
  if (!"dead_Mgha" %in% names(vst_plot_summary)) {

    vst_plot_summary$dead_Mgha <- NA

  }

  #   Assumption: Replace NAs created during transpose with zeroes; assume both live and dead were sampled in a plot
  vst_plot_summary$dead_Mgha[is.na(vst_plot_summary$dead_Mgha)] <- 0
  vst_plot_summary$live_Mgha[is.na(vst_plot_summary$live_Mgha)] <- 0

  #   Add rows for plots with zero biomass to plots with AGB, calculate total "agb_Mgha"
  vst_plot_summary <- dplyr::bind_rows(vst_plot_summary,
                                       vst_agb_zeros) %>%
    dplyr::relocate("live_Mgha",
                    .before = "dead_Mgha") %>%
    dplyr::mutate(agb_Mgha = rowSums(dplyr::across(c("live_Mgha", "dead_Mgha")), na.rm = TRUE),
                  .before = "live_Mgha") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID)



  ### Generate site-level biomass summary ####

  ##  Create site-level summary table: mean, sd, n()
  if(nrow(vst_plot_summary) > 0) {

    vst_site_summary <- vst_plot_summary %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year) %>%
      dplyr::summarise(woodPlotNum = dplyr::n(),
                       woodPlotType = paste(unique(.data$plotType), collapse = ", "),
                       woodMassMean_Mgha = round(mean(.data$agb_Mgha, na.rm = TRUE),
                                                 digits = 1),
                       woodMassSD_Mgha = round(stats::sd(.data$agb_Mgha, na.rm = TRUE),
                                               digits = 1),
                       woodLiveMassMean_Mgha = round(mean(.data$live_Mgha, na.rm = TRUE),
                                                     digits = 1),
                       woodLiveMassSD_Mgha = round(stats::sd(.data$live_Mgha, na.rm = TRUE),
                                                   digits = 1),
                       woodDeadMassMean_Mgha = round(mean(.data$dead_Mgha, na.rm = TRUE),
                                                     digits = 1),
                       woodDeadMassSD_Mgha = round(stats::sd(.data$dead_Mgha, na.rm = TRUE),
                                                   digits = 1),
                       .groups = "drop") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year)

  } else {

    vst_site_summary <- "Could not create site-level output table: Insufficient plot-level data"

  } # end nrow() conditional



  ### Bundle and return output ####
  output.list <- list(vst_agb_kg = vst_agb_kg,
                      vst_missing = missingDownedDF,
                      vst_plot_Mgha = vst_plot_summary,
                      vst_site_Mgha = vst_site_summary)

  return(output.list)
}
=======
#' @title Estimate above-ground biomass of woody vegetation
#'
#' @author
#' Samuel M Simkin \email{ssimkin@BattelleEcology.org} \cr
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Allometric equations are used to estimate above-ground biomass for woody individuals reported in the NEON "Vegetation structure" data product (DP1.10098.001). Results are summarized as mass per unit area at scales of the plotID and siteID. Biomass outputs can be used in the neonPlants estimateMass() and estimateWoodProd() functions.
#'
#' Data inputs are "Vegetation structure" data (DP1.10098.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' @details Input data can be filtered via the 'plotSubset' argument if output for only certain types of plots or sampling intervals is desired. Input data are combined with taxon specific characteristics (e.g., wood density), and biomass is estimated for each individual using allometric equations. Taxon-specific equations are applied if available, and generalized allometries are used otherwise. The 'growthFormSubset' argument enables biomass estimation only for "tree" individuals (i.e., woody individuals with DBH ≥ 10 cm) or for "all" growth forms excluding "cactus", "ferns", and "yucca". Biomass is summarized on an areal basis at the hierarchical level of the plotID and siteID.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. It is optional to include the "vst_non-woody" table in the list. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for the 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments.  [list]
#'
#' @param inputIndividual The 'vst_apparentindividual' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputMapTag The 'vst_mappingandtagging' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputNonWoody (Optional) The 'vst_non-woody' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputPerPlot The 'vst_perplotperyear' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param plotSubset The options are the default of "all" (all Tower and Distributed plots), "towerAll" (all plots in the Tower airshed but no Distributed plots), "towerAnnualSubset" (the subset of n=5 Tower plots that are sampled annually), and "distributed" (all Distributed plots, which are sampled at 5-yr intervals and are spatially representative of the NLCD classes at a site). [character]
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
#' @export estimateWoodMass


estimateWoodMass = function(inputDataList,
                            inputIndividual = NA,
                            inputMapTag = NA,
                            inputNonWoody = NA,
                            inputPerPlot = NA,
                            plotSubset = "all",
                            growthFormSubset = "all") {

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
      (!inherits(inputIndividual, "data.frame") | !inherits(inputMapTag, "data.frame")  |
       !inherits(inputPerPlot, "data.frame") | !inherits(inputNonWoody, "data.frame"))) {

    stop("Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
  }


  #   Assign standardized names to input data frames
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



  ### Verify input tables contain required columns and data ####

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
  plotExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana", "totalSampledAreaFerns", "totalSampledAreaOther")

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
  appIndExpCols <- c("plotID", "individualID", "growthForm", "plantStatus", "date", "eventID", "stemDiameter", "basalStemDiameter", "height", "maxCrownDiameter", "ninetyCrownDiameter")

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

  #  if (class(vst_nonWoody) == "data.frame"){if(length(setdiff(nonwoodyExpCols, colnames(vst_nonWoody))) > 0) {
  if (methods::is(nonWoody, class = "data.frame" )) {

    if (length(setdiff(nonwoodyExpCols, colnames(nonWoody))) > 0) {
      stop(glue::glue("Required columns missing from vst_nonWoody:", '{paste(setdiff(nonwoodyExpCols, colnames(nonWoody), collapse = ", ")}',
                      .sep = " "))
    }
  }

  # Error if invalid growthFormSubset option selected
  if (!growthFormSubset %in% c("all", "tree")) {
    stop("The growthFormSubset argument must be one of: 'all', 'tree'")
  }

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The plotSubset argument must be one of: 'all', 'towerAll', 'towerAnnualSubset', 'distributed'")
  }

  #   For plotPriority: 50 is highest possible value
  plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50)

  #   Assign plotType needed in output based on 'plotSubset' argument
  plotType <- dplyr::case_when(plotSubset == "all" ~ "all",
                               plotSubset == "distributed" ~ "distributed",
                               plotSubset %in% c("towerAll", "towerAnnualSubset") ~ "tower")




  ### Prepare data frames: Retrieve and join ancillary data and filter before analysis with supplied user arguments ####

  ### Prepare 'perPlot' table
  #   Extract year from eventID, create 'plotID x eventID' identifier
  perPlot <- perPlot %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$eventID, "20[0-9]{2}$")),
                  .before = "eventID") %>%
    dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                  .before = "plotID")


  ##  Join with plot priority data; the 'specificModuleSamplingPriority' field is used to optionally filter only to plots with priority 1-5 when user-supplied 'plotSubset' == "towerAnnualSubset"
  priority_plots <- priority_plots %>%
    dplyr::select("plotID",
                  "specificModuleSamplingPriority")

  perPlot <- merge(perPlot,
                   priority_plots,
                   by = "plotID",
                   all.x = TRUE)

  #   Filter according to user-supplied 'plotSubset' argument and derived 'plotPriority' variable
  perPlot <- perPlot %>%
    dplyr::filter(dplyr::case_when(plotSubset %in% c("towerAll", "towerAnnualSubset") ~ .data$plotType == "tower",
                                   plotSubset == "distributed" ~ .data$plotType == "distributed",
                                   TRUE ~ .data$plotType %in% c("distributed", "tower"))) %>%
    dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)


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
  plot_eventID_full <- perPlot %>%
    dplyr::filter(dplyr::case_when(growthFormSubset == "all" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                  is.na(.data$samplingImpractical)) &
                                     !.data$dataCollected %in% c("dendrometerOnly", "partial"),
                                   growthFormSubset == "tree" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                   is.na(.data$samplingImpractical)) &
                                     .data$dataCollected != "dendrometerOnly")) %>%
    dplyr::distinct(.data$plot_eventID)

  plot_eventID_full <- plot_eventID_full$plot_eventID

  #   Identify 'plot_eventIDs' for dataCollected == "dendrometerOnly | partial"; list needed to remove these records from 'appInd' table and identify plots that are true "zeros" for woody biomass. Need to conditionally account for fact that dataCollected == "partial" is effectively full sampling when argument growthFormSubset == "tree"
plot_eventID_partial <- perPlot %>%
  dplyr::filter(dplyr::case_when(growthFormSubset == "all" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                is.na(.data$samplingImpractical)) &
                                   .data$dataCollected %in% c("dendrometerOnly", "partial"),
                                 growthFormSubset == "tree" ~ (.data$samplingImpractical %in% c("", "OK") |
                                                                 is.na(.data$samplingImpractical)) &
                                   .data$dataCollected == "dendrometerOnly")) %>%
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
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana",
                  "totalSampledAreaOther")



  ### Prepare 'map' table
  #   Retain most recent record from vst_mappingandtagging
  map <- map[order(map$date),]
  map <- map[!duplicated(map$individualID, fromLast = TRUE), ]

  #   Retain data for those plots in 'perPlot' table; 'map' output is effectively filtered to user-supplied 'plotSubset'
  map <- map %>%
    dplyr::filter(.data$plotID %in% perPlot$plotID)

  #   Find unique taxonIDs
  taxonID_df <- map %>%
    dplyr::distinct(.data$taxonID,
                    .data$scientificName,
                    .data$family,
                    .data$genus)

  vst_taxonIDs <- sort(taxonID_df$taxonID)

  map <- map %>%
    dplyr::select("individualID",
                  "taxonID")



  ### Prepare 'appInd' table
  ##  Remove apparentIndividual records without necessary perplot data
  appInd$plot_eventID <- paste0(appInd$plotID, "_", appInd$eventID)

  appInd <- appInd %>%
    dplyr::filter(appInd$plot_eventID %in% plot_eventID_list)


  ##  Merge vst_apparentindividual table with map and perplot to obtain taxonID field and sampling area fields
  #   Add taxonID to appInd table
  appInd <- merge(appInd,
                  map,
                  by = "individualID",
                  all.x = TRUE)

  #   Add total sampled area fields
  appInd <- merge(appInd,
                  perPlot,
                  by = c("domainID", "siteID", "plotID", "eventID", "plot_eventID"),
                  all.x = TRUE)

  #   Resolve missing taxonIDs and Betula slash species issue
  appInd$taxonID <- ifelse(is.na(appInd$taxonID),
                           "2PLANT",
                           appInd$taxonID)

  appInd$taxonID <- ifelse(appInd$taxonID == "BEGL/BENA",
                           "BEGL",
                           appInd$taxonID)


  ##  Filter by user-supplied 'growthFormSubset'
  if (growthFormSubset == "tree") {

    appInd <- appInd %>%
      dplyr::filter(.data$growthForm %in% c("single bole tree", "multi-bole tree"))

  }


  ##  Remove unneeded columns and reorganize column order
  appInd <- appInd %>%
    dplyr::select(-"uid",
                  -"namedLocation",
                  -"dendrometerInstallationDate",
                  -"initialGapMeasurementDate",
                  -"initialBandStemDiameter",
                  -"initialDendrometerGap",
                  -"dendrometerHeight",
                  -"dendrometerGap",
                  -"dendrometerCondition",
                  -"bandStemDiameter",
                  -"publicationDate",
                  -"measuredBy",
                  -"recordedBy",
                  -"dataEntryRecordID",
                  -"release",
                  -"dataQF") %>%
    dplyr::relocate(c("plotID", "eventID"),
                    .after = "siteID")



  ### Prepare 'nonWoody' table
  if (methods::is(nonWoody, class = "data.frame")) {

    ##  Create additional required columns
    nonWoody <- nonWoody %>%
      dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                    .before = "date") %>%
      dplyr::mutate(year = as.numeric(stringr::str_extract(.data$eventID, "20[0-9]{2}")),
                    .before = "eventID")


    ##  Remove nonWoody records without necessary perplot data (incorporates 'plotSubset' filtering) and remove records based on user-supplied 'growthFormSubset'
    nonWoody <- nonWoody %>%
      dplyr::filter(.data$plot_eventID %in% plot_eventID_list)


    if (growthFormSubset == "tree") {

      nonWoody <- nonWoody %>%
        dplyr::filter(.data$growthForm %in% c("palm tree", "large tree fern"))

    }


    ##  Remove unneeded columns
    nonWoody <- nonWoody %>%
      dplyr::select(-"uid",
                    -"namedLocation",
                    -"publicationDate",
                    -"stemCount",
                    -"branchCount",
                    -"meanBranchLength",
                    -"identificationReferences",
                    -"identificationQualifier",
                    -"morphospeciesID",
                    -"measuredBy",
                    -"recordedBy",
                    -"dataEntryRecordID",
                    -"release")


    ##  Merge with perplot data to add total sampled areas
    nonWoody <- merge(nonWoody,
                      perPlot,
                      by = c("plot_eventID", "year", "domainID", "siteID", "plotID", "eventID"),
                      all.x = TRUE)

  } # end nonWoody data frame conditional



  ###  Define plantStatus groups to identify standing individuals that are live/dead and individuals absent, missing, or with ambiguous fate
  standingLiveDead <- c("Live",
                        "Live, insect damaged",
                        "Live, disease damaged",
                        "Live, physically damaged",
                        "Live, other damage",
                        "Live, broken bole",
                        "Standing dead",
                        "Dead, broken bole")

  missingDowned <- c("Removed",
                     "No longer qualifies",
                     "Lost, burned",
                     "Lost, herbivory",
                     "Lost, presumed dead",
                     "Lost, fate unknown",
                     "Downed")

  #   Create 'missingDownedDF" table for later output
  missingDownedDF <- data.frame()



  ### Estimate non-woody biomass: Calculate biomass from vst_non-woody table ####

  ### Conditionally generate non-woody biomass estimates from vst_nonWoody table
  if (methods::is(nonWoody, class = "data.frame" )) {

    #   Create 'source' and 'agb' columns to track allometry citations and record aboveground biomass, respectively
    vst_agb_other <- nonWoody %>%
      dplyr::mutate(source = "missingAllometry",
                    agb = NA)


    ##  Estimate ocotillo biomass: Bobich, E.G., and T.E. Huxman. 2009. Dry mass partitioning and gas exhange for young ocotillos (Fouquieria splendends) in the Sonoran Desert. International Journal of Plant Science 170:283-289. Equations:
    #   log(height_m) = 0.13 + 0.45 * log(total above and below ground biomass in kg)
    #   log(total above and below ground biomass in kg) = (log(height_m) - 0.13)/0.45 = -0.2889 +  (2.2222 * log(height_m))
    #   log(root/shoot) = -0.63 + 0.18 * log(total above and below ground biomass in kg)
    #   aboveground biomass in kg = 1(1+exp(log(root/shoot))) * exp(log(total above and below ground biomass in kg)) = fraction aboveground * total biomass

    #   Estimate total ocotillo mass: aboveground + belowground
    vst_agb_other$tot_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
                                         exp(-0.2889 + 2.2222 * log(vst_agb_other$height)),
                                         NA)

    #   Estimate aboveground ocotillo mass
    vst_agb_other$agb_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
                                         round(1/(exp(-0.63 + 0.18 * log(vst_agb_other$tot_ocotillo)) + 1) *
                                                 vst_agb_other$tot_ocotillo,
                                               digits = 3),
                                         NA)

    #   Remove total ocotillo mass: Belowground estimate not needed
    vst_agb_other$tot_ocotillo <- NULL

    #   Update "agb" column with ocotillo mass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(source = dplyr::case_when(!is.na(.data$agb_ocotillo) ~ "Bobich_Huxman_2009",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_ocotillo) ~ .data$agb_ocotillo,
                                           TRUE ~ .data$agb))


    ##  Estimate Xerophyllum tenax (bear grass) biomass: Gholz, H.L., C.C. Grier, A.G. Campbell, and A.T. Brown. 1979. Equations for estimating biomass and leaf area of plants in the pacific northwest. Research paper 41. Forest Research Laboratory, School of Forestry at Oregon State University, Corvallis. Divide by 1000 to convert output to "kg".

    vst_agb_other$agb_xer <- ifelse(vst_agb_other$growthForm == "xerophyllum",
                                    round((18.873 + (0.0280*((vst_agb_other$basalStemDiameter^2) *
                                                               vst_agb_other$meanLeafLength)))/1000,
                                          digits = 3),
                                    NA)

    #   Update "agb" column with xerophyllum mass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(source = dplyr::case_when(!is.na(.data$agb_xer) ~ "Gholz_etal_1979",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_xer) ~ .data$agb_xer,
                                           TRUE ~ .data$agb))


    ##  Estimate small palm biomass (primarily Serenoa repens): Gholz, H.L., D.N. Guerin, and W.P. Cropper. 1999. Phenology and productivity of saw palmetto (Serenoa repens) in a north Florida slash pine plantation. Canadian Journal of Forest Research 29:1248-1253.
    #   Use separate equations for rachis/petiole biomass (g) and blade/leaf biomass (g). Add together and multiply by leafNumber, then divide by 1000 to get total biomass (kg).
    #   Alexis et al. 2007 Biogeochemistry add petiole length and blade length together to get rachis biomass.

    vst_agb_other$agb_palm <- ifelse(vst_agb_other$growthForm == "small palm" & !is.na(vst_agb_other$meanPetioleLength) &
                                       !is.na(vst_agb_other$meanBladeLength) & !is.na(vst_agb_other$leafNumber),
                                     round((exp(-10.38 + 2.72 * log(vst_agb_other$meanPetioleLength +
                                                                      vst_agb_other$meanBladeLength)) +
                                              (-13.31 + 0.85 * vst_agb_other$meanBladeLength)) *
                                             vst_agb_other$leafNumber / 1000,
                                           digits = 3),
                                     NA)

    #   Update "agb" column with small palm biomass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(source = dplyr::case_when(!is.na(.data$agb_palm) ~ "Gholz_etal_1999",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_palm) ~ .data$agb_palm,
                                           TRUE ~ .data$agb))


    ##  Estimate Cibotium biomass (tree fern): Asner, GP, RF Hughes, J Mascaro, AL Uowolo, DE Knapp, J Jacobson, T Kennedy-Bowdoin, JK Clark. 2011. High-resolution carbon mapping on the million-hectare Island of Hawaii. Frontiers in Ecology and the Environment. Vol 9(8), pp. 434-439; Cibotium and Sadleria wood density (spg_gcm3) also comes from Asner et al. 2011.

    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(agb_Cibotium = dplyr::case_when(grepl("Cibotium", .data$scientificName) &
                                                      .data$growthForm == "large tree fern" ~
                                                      round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.22/1000,
                                                            digits = 2),
                                                    grepl("Cibotium", .data$scientificName) &
                                                      .data$growthForm == "small tree fern" ~
                                                      round(pi * (.data$basalStemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.22/1000,
                                                            digits = 2),
                                                    grepl("Sadleria", .data$scientificName) &
                                                      .data$growthForm == "large tree fern" ~
                                                      round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.5/1000,
                                                            digits = 2),
                                                    grepl("Sadleria", .data$scientificName) &
                                                      .data$growthForm == "small tree fern" ~
                                                      round(pi * (.data$basalStemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.5/1000,
                                                            digits = 2),
                                                    TRUE ~ NA),
                    source = dplyr::case_when(!is.na(.data$agb_Cibotium) ~ "Asner_etal_2011",
                                                  TRUE ~ .data$source),
                    agb = dplyr::case_when(!is.na(.data$agb_Cibotium) ~ .data$agb_Cibotium,
                                           TRUE ~ .data$agb))



    ### Clean-up of nonWoody data

    ##  Collate missing individuals (removed, lost, downed) and those with agb = "NA" for separate output table
    missingDownedDF <- vst_agb_other %>%
      dplyr::filter(.data$plantStatus %in% missingDowned | is.na(.data$agb)) %>%
      dplyr::select("plot_eventID",
                    "domainID",
                    "siteID",
                    "year",
                    "eventID",
                    "date",
                    "nlcdClass",
                    "plotID",
                    "subplotID",
                    "taxonID",
                    "scientificName",
                    "individualID",
                    "plantStatus",
                    "growthForm",
                    "measurementHeight",
                    "stemDiameter",
                    "basalStemDiameter",
                    "height",
                    "stemLength",
                    "maxCrownDiameter",
                    "ninetyCrownDiameter",
                    "source",
                    "agb") %>%
      dplyr::bind_rows(missingDownedDF) %>%
      dplyr::rename("agb_kg" = "agb")


    ##  Retain standing individuals in the plot with agb != NA that are unambiguously 'alive' or 'dead' according to plantStatus and create 'simplePlantStatus' variable. Removing NA records avoids misinterpreting as "0" mass in later steps. Cactus and ferns removed because no allometries are applied to these individuals.
    vst_agb_other <- vst_agb_other %>%
      dplyr::filter(.data$plantStatus %in% standingLiveDead,
                    !is.na(.data$agb),
                    !.data$growthForm %in% c("cactus", "fern")) %>%
      dplyr::mutate(simplePlantStatus = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                         TRUE ~ "dead"))


    ##  Aggregate vst non-herbaceous perennial (other) biomass data (multiple records associated with multi-stem individuals)
    if (nrow(vst_agb_other) > 0) {

      vst_agb_final_other <- vst_agb_other %>%
        dplyr::group_by(.data$plot_eventID,
                        .data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$eventID,
                        .data$date,
                        .data$nlcdClass,
                        .data$plotID,
                        .data$subplotID,
                        .data$taxonID,
                        .data$scientificName,
                        .data$individualID,
                        .data$simplePlantStatus,
                        .data$growthForm,
                        .data$totalSampledAreaTrees,
                        .data$totalSampledAreaOther) %>%
        dplyr::summarise(source = paste(unique(.data$source), collapse = ", "),
                         agb_kg = sum(.data$agb, na.rm = TRUE),
                         .groups = "drop")


      ##  Assign total sampled area for each individual based on growthForm
      #--> palm tree and large tree fern individuals sampled throughout plot like trees.
      vst_agb_final_other <- vst_agb_final_other %>%
        dplyr::mutate(sampledArea_m2 = dplyr::case_when(.data$growthForm %in% c("palm tree", "large tree fern") ~
                                                          .data$totalSampledAreaTrees,
                                                        TRUE ~ .data$totalSampledAreaOther))


      ##  Remove unneeded totalSampledArea columns
      vst_agb_final_other <- vst_agb_final_other %>%
        dplyr::select(-"totalSampledAreaTrees",
                      -"totalSampledAreaOther")

    } else {

      vst_agb_final_other <- data.frame()

    } # end nrow(vst_agb_other) conditional

  } #   end non-woody conditional



  ### Estimate woody biomass: Calculate biomass for individuals in vst_apparentindividual table ####

  ##  Read in the Chojnacky et al 2014 parameters for each of their 35 defined allometric groups
  parameters <- parameters %>%
    dplyr::select("allometry_ID",
                  "b0",
                  "b1",
                  "minDiameter",
                  "maxDiameter")


  ##  Load wood density, veg type, and other data needed to assign species to Chojnacky allometry groups
  taxon_fields <- taxon_fields
  taxon_fields_list <- unique(taxon_fields$taxonID)


  ##  Load USDA Plants characteristics to get PLANTS.Floristic.Area and Native.Status: Filtered to records that have PLANTS.Floristic.Area, Native.Status, or both
  plantIntTrop <- plantIntTrop

  #   Add tropical floristic area and/or introduced status to taxa derived from vst_mappingandtagging data
  plant_char <- merge(taxonID_df,
                      plantIntTrop,
                      by = "taxonID",
                      all.x = TRUE)


  ##  Programatically assign a Chojnacky allometry_ID based on genus, family, specific gravity, deciduous vs. evergreen, and/or woodland vs. forest habit
  Choj <- merge(taxon_fields,
                plant_char,
                by = "taxonID",
                all = TRUE)

  #   Retain only taxonIDs found in the vst_mappingandtagging data
  Choj <- Choj[Choj$taxonID %in% vst_taxonIDs, ]

  #   Stanardize 'nativeStatus' and 'tropical' LOV elements
  Choj$nativeStatus <- dplyr::if_else(Choj$nativeStatus == "int",
                                      "introduced",
                                      "native",
                                      "native")

  Choj$tropical <- dplyr::if_else(Choj$tropical == "trop",
                                  "tropical",
                                  "temperate",
                                  "temperate")


  ##  Assign Chojnacky allometric equation IDs
  Choj <- Choj %>%
    dplyr::mutate(allometry_ID = dplyr::case_when(

      .data$woodland_vs_forest == "forest" & .data$genus == "Abies" & .data$spg_gcm3 < 0.35 ~ "C1",
      .data$woodland_vs_forest == "forest" & .data$genus == "Abies" & .data$spg_gcm3 >= 0.35 ~ "C2",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" & .data$spg_gcm3 < 0.30 ~ "C3",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" &
        .data$spg_gcm3 >= 0.30 & .data$spg_gcm3 < 0.40 ~ "C4",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" & .data$spg_gcm3 >= 0.40 ~ "C5",
      .data$woodland_vs_forest == "forest" & .data$genus == "Larix" ~ "C6",
      .data$woodland_vs_forest == "forest" & .data$genus == "Picea" & .data$spg_gcm3 < 0.35 ~ "C7",
      .data$woodland_vs_forest == "forest" & .data$genus == "Picea" & .data$spg_gcm3 >= 0.35 ~ "C8",
      .data$woodland_vs_forest == "forest" & .data$genus == "Pinus" & .data$spg_gcm3 < 0.45 ~ "C9",
      .data$woodland_vs_forest == "forest" & .data$genus == "Pinus" & .data$spg_gcm3 >= 0.45 ~ "C10",
      .data$woodland_vs_forest == "forest" & .data$genus %in% c("Pseudotsuga", "Taxus") ~ "C11",
      .data$woodland_vs_forest == "forest" & .data$genus == "Tsuga" & .data$spg_gcm3 < 0.40 ~ "C12",
      .data$woodland_vs_forest == "forest" & .data$genus == "Tsuga" & .data$spg_gcm3 >= 0.40 ~ "C13",
      .data$woodland_vs_forest  %in% c("forest", "") & .data$family == "Aceraceae" & .data$spg_gcm3 < 0.50 ~ "H1",
      .data$woodland_vs_forest %in% c("forest", "") & .data$family == "Aceraceae" & .data$spg_gcm3 >= 0.50 ~ "H2",
      .data$family == "Betulaceae" & .data$spg_gcm3 < 0.40 ~ "H3",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.40 & .data$spg_gcm3 < 0.50 ~ "H4",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.50 & .data$spg_gcm3 < 0.60 ~ "H5",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.60 ~ "H6",
      .data$family %in% c("Cornaceae", "Ericaceae", "Lauraceae", "Platanaceae", "Rosaceae", "Ulmaceae") ~ "H7",
      .data$woodland_vs_forest == "forest" & .data$genus == "Carya" ~ "H8",
      .data$woodland_vs_forest == "forest" & .data$family %in% c("Fabaceae", "Juglandaceae") & .data$genus != "Carya" ~ "H9",
      .data$woodland_vs_forest == "forest" & .data$family == "Fagaceae" & .data$decid_vs_ever == "decid" ~ "H10",
      .data$woodland_vs_forest == "forest" & .data$family == "Fagaceae" & .data$decid_vs_ever == "ever" ~ "H11",
      .data$family == "Hamamelidaceae" ~ "H12",
      .data$family %in% c("Hippocastanaceae", "Tiliaceae") ~ "H13",
      .data$family == "Magnoliaceae" ~ "H14",
      .data$family == "Oleaceae" & .data$spg_gcm3 < 0.55 ~ "H15",
      .data$family == "Oleaceae" & .data$spg_gcm3 >= 0.55 ~ "H16",
      .data$family == "Salicaceae" & .data$spg_gcm3 < 0.35 ~ "H17",
      .data$family == "Salicaceae" & .data$spg_gcm3 >= 0.35 ~ "H18",
      .data$woodland_vs_forest == "woodland" & .data$family == "Cupressaceae" ~ "W1",
      .data$woodland_vs_forest == "woodland" & .data$family %in% c("Fabaceae", "Rosaceae") ~ "W2",
      .data$woodland_vs_forest == "woodland" & .data$family == "Fagaceae" ~ "W3",
      .data$woodland_vs_forest == "woodland" & .data$family == "Pinaceae" ~ "W4",
      #   Arbitrarily picked C9 (forest) over C10 (forest spg_gcm3>=0.45) or W4 (woodland)
      .data$taxonID == "PINACE" ~ "C9",
      #   Arbitrarily picked H9 (forest) over W2 (woodland)
      .data$taxonID == "FABACE" ~ "H9",
      TRUE ~ NA

    )) %>%
    dplyr::relocate("allometry_ID",
                    "family",
                    "genus",
                    .before = "taxonID") %>%

    #   Identify taxa not in Chojnacky
    dplyr::mutate(source = ifelse(!is.na(.data$allometry_ID),
                                  "Chojnacky_etal_2014",
                                  "missingAllometry")) %>%

    #   Reduce 'Choj' to desired columns
    dplyr::select("allometry_ID",
                  "family",
                  "genus",
                  "taxonID",
                  "spg_gcm3",
                  "scientificName",
                  "nativeStatus",
                  "tropical",
                  "source")


  ##  Merge 'Choj' to associate taxonIDs in data with allometric parameters
  Choj <- merge(parameters,
                Choj,
                by = "allometry_ID",
                all.y = TRUE)



  ### Prepare 'vst_agb' for biomass estimation by taxonID
  vst_agb <- merge(appInd,
                   Choj,
                   by = "taxonID",
                   all.x = TRUE)

  #   Manually assign 'tropical' and 'temperate' status for a subset of taxonIDs
  vst_agb <- vst_agb %>%
    dplyr::mutate(tropical = dplyr::case_when(.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("2PLANT", "2PLANT-H", "ANAL12", "BOURR", "BUMI6",
                                                                     "CONVOL", "CROSS", "FABACE", "JACQU", "JACQU2",
                                                                     "COPRO", "HYDRAN") ~ "tropical",
                                              TRUE ~ .data$tropical))

  vst_agb <- vst_agb %>%
    dplyr::mutate(tropical = dplyr::case_when(!.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("AMAR5", "CELTI", "DAWR2", "LIJA", "MEAZ", "OPUNT",
                                                                     "RHUS", "SAMBU", "SMSM", "SYMPL2", "VITIS")
                                              ~ "temperate",
                                              TRUE ~ .data$tropical))

  #   Assign specific gravity data type
  vst_agb$spg_gcm3 <- as.numeric(vst_agb$spg_gcm3)

  #   Correct negative ninetyCrownDiameter: Meaningless and generates NaN warnings with some allometric equations
  vst_agb$ninetyCrownDiameter <- dplyr::if_else(vst_agb$ninetyCrownDiameter < 0,
                                                NA,
                                                vst_agb$ninetyCrownDiameter)

  #   Assumption: For tropical species, if specific gravity is not known then assume it is 0.5 g/cm3 to permit usage of Chave et al 2014, following precedent of Asner et al 2011
  vst_agb$spg_gcm3 <- dplyr::if_else(is.na(vst_agb$spg_gcm3) & vst_agb$tropical == "tropical",
                                     0.5,
                                     vst_agb$spg_gcm3,
                                     vst_agb$spg_gcm3)

  #   Select columns to remove unneeded data
  vst_agb <- vst_agb %>%
    dplyr::select("plot_eventID",
                  "domainID",
                  "siteID",
                  "plotID",
                  "subplotID",
                  "taxonID",
                  "family",
                  "genus",
                  "scientificName",
                  "individualID",
                  "year",
                  "eventID",
                  "date",
                  "growthForm",
                  "nlcdClass",
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana",
                  "plantStatus",
                  "height",
                  "measurementHeight",
                  "stemDiameter",
                  "basalStemDiameter",
                  "basalStemDiameterMsrmntHeight",
                  "maxCrownDiameter",
                  "ninetyCrownDiameter",
                  "allometry_ID",
                  "b0",
                  "b1",
                  "minDiameter",
                  "maxDiameter",
                  "spg_gcm3",
                  "nativeStatus",
                  "tropical",
                  "source")



  ### Multi-bole trees: Assume that 'height' of individual that is measured for primary bole applies to secondary boles. Secondary 'mbt' boles at PUUM with no 'height' would otherwise have AGB = NA since Chave "E" parameter unavailable for PUUM and "E" is needed to estimate 'height' required by Chave allometry when 'height' is missing.
  #   Separate 'mbt' from other growth forms
  nonMbt <- vst_agb %>%
    dplyr::filter(.data$growthForm != "multi-bole tree" | is.na(.data$growthForm))

  mbt <- vst_agb %>%
    dplyr::filter(.data$growthForm == "multi-bole tree") %>%
    dplyr::mutate(tempIndivID = stringr::str_extract(string = .data$individualID,
                                                     pattern = "^NEON.PLA.D[0-9]{2}.[A-Z]{4}.[0-9]{5}"),
                  .before = "individualID")

  #   Assign height and crown dimensions from primary bole to secondary boles
  heightCrownMBT <- mbt %>%
    dplyr::group_by(.data$tempIndivID) %>%
    dplyr::summarise(height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA),
                     maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                               max(.data$maxCrownDiameter, na.rm = TRUE),
                                               NA),
                     ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                  max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                  NA))

  mbt <- dplyr::left_join(mbt %>%
                            dplyr::select(-"height",
                                          -"maxCrownDiameter",
                                          -"ninetyCrownDiameter"),
                          heightCrownMBT,
                          by = "tempIndivID") %>%
    dplyr::relocate("height",
                    .after = "plantStatus") %>%
    dplyr::relocate("maxCrownDiameter":"ninetyCrownDiameter",
                    .before = "allometry_ID") %>%
    dplyr::select(-"tempIndivID")

  vst_agb <- dplyr::bind_rows(nonMbt,
                              mbt)



  ### Shrubs: Combine emergent boles for use with Conti et al. 2019 allometries
  #   Separate shrubs from other growthForms to calculate aggregated basalStemDiameter inputs for Conti
  nonShrub <- vst_agb %>%
    dplyr::filter(!.data$growthForm %in% c("single shrub", "small shrub") | is.na(.data$growthForm))

  shrub <- vst_agb %>%
    dplyr::filter(.data$growthForm %in% c("single shrub", "small shrub"))

  #   For shrubs, height and crown dimensions are measured once per individualID; apply these measurements to all emergent boles so that separate live and dead biomass estimates can be generated. Crown dimensions are particularly important for some taxon-specific allometries (e.g., ARTR2).
  heightCrownShrub <- shrub %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA),
                     maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                               max(.data$maxCrownDiameter, na.rm = TRUE),
                                               NA),
                     ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                  max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                  NA))

  #   Group multiple stems belonging to same individualID x plantStatus combination, and calculate equivalent stemDiameter and basalStemDiameter.
  shrub <- shrub %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$subplotID,
                    .data$taxonID,
                    .data$family,
                    .data$genus,
                    .data$scientificName,
                    .data$individualID,
                    .data$year,
                    .data$eventID,
                    .data$date,
                    .data$growthForm,
                    .data$nlcdClass,
                    .data$totalSampledAreaTrees,
                    .data$totalSampledAreaShrubSapling,
                    .data$totalSampledAreaLiana,
                    .data$plantStatus,
                    .data$allometry_ID,
                    .data$b0,
                    .data$b1,
                    .data$minDiameter,
                    .data$maxDiameter,
                    .data$spg_gcm3,
                    .data$nativeStatus,
                    .data$tropical,
                    .data$source) %>%
    dplyr::summarise(stemDiameter = ifelse(!all(is.na(.data$stemDiameter)),
                                           round(sqrt(sum(.data$stemDiameter^2)),
                                                 digits = 1),
                                           NA),
                     basalStemDiameter = ifelse(!all(is.na(.data$basalStemDiameter)),
                                                round(sqrt(sum(.data$basalStemDiameter^2)),
                                                      digits = 1),
                                                NA),
                     measurementHeight = ifelse(!all(is.na(.data$measurementHeight)),
                                                round(mean(.data$measurementHeight, na.rm = TRUE),
                                                      digits = 0),
                                                NA),
                     basalStemDiameterMsrmntHeight = ifelse(!all(is.na(.data$basalStemDiameterMsrmntHeight)),
                                                            round(mean(.data$basalStemDiameterMsrmntHeight, na.rm = TRUE),
                                                                  digits = 0),
                                                            NA),
                     .groups = "drop")

  ##  Join with 'heightCrownShrub' to assign crown dimensions based on individualID
  shrub <- dplyr::left_join(shrub,
                            heightCrownShrub,
                            by = "individualID")

  rm(heightCrownShrub)


  ##  Bind 'nonShrub' and 'shrub' together into simplified dataframe
  vst_agb <- dplyr::bind_rows(nonShrub,
                              shrub)

  rm(nonShrub, shrub)



  ### Calculate AGB for each VST appInd record using Choj allometry_ID and Choj parameters

  # Assumption: Chojnacky et al 2014 allometric equations are the best first estimate of biomass
  vst_agb$agb <- round(exp(vst_agb$b0 + vst_agb$b1 * log(vst_agb$stemDiameter)),
                       digits = 2)

  #   Assign Chojnacky AGB estimates to specific column; needed to preserve Chojnacky estimates when alternate is used for tropical or introduced species.
  vst_agb$agb_Chojnacky  <- vst_agb$agb

  #   Assumption: When the necessary ancillary variables are available for tropical species, replace the Chojnacky et al 2014 biomass estimates with the Chave et al 2014 biomass estimates.
    # Update tropical species records based on Chave et al 2014 if wood specific gravity is available (or an approximation based on congeners).
    # Instructions on extracting environmental stress value E at http://chave.ups-tlse.fr/pantropical_allometry.html; Chave et al 2014 has pantropical allometric equations for tree biomass that require tree height. If tree height is not available, estimate it using their value E.
  #   Chave et al 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20:3177-3190
  # install.packages("raster"); install.packages("ncdf4"); library("raster"); library("ncdf4")
  # source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
  # coord <- data.frame(siteID = c("GUAN", "LAJA", "PUUM"), longitude = c(-66.8687, -67.07689, -155.31731), latitude = c(17.96955, 18.02126, 19.55309) );  rownames(coord) <- coord$siteID; coord$siteID <- NULL
  # Chave_et_al_2014_E <- retrieve_raster("E",coord,plot=TRUE,format="nc") returns an E of 0.5074847 for GUAN, 0.4440793 for LAJA, and NA for PUUM

  #   Assign Chave et al 2014 "E" values needed for site-specific height estimation when height is missing
  vst_agb$Chave_E <- ifelse(vst_agb$siteID == "GUAN",
                            0.5074847,
                            NA)

  vst_agb$Chave_E <- ifelse(vst_agb$siteID == "LAJA",
                            0.4440793,
                            vst_agb$Chave_E)


  ##  Estimate AGB for tropical species: Different equations with 'height' and without 'height'
  vst_agb <- vst_agb %>%
    dplyr::mutate(agb_trop = dplyr::case_when(!dplyr::if_any(c("height", "stemDiameter", "spg_gcm3"), is.na) &
                                                .data$tropical == "tropical" ~
                                                round(0.0673 * (vst_agb$spg_gcm3 * (vst_agb$stemDiameter^2) *
                                                                  vst_agb$height)^0.976,
                                                      digits = 2),

                                              # Estimate when 'height' missing
                                              is.na(.data$height) & !dplyr::if_any(c("stemDiameter", "spg_gcm3"), is.na) &
                                                .data$tropical == "tropical" ~
                                                round(exp(-1.803 - (0.976 * vst_agb$Chave_E) +
                                                            (0.976 * log(vst_agb$spg_gcm3)) +
                                                            (2.673 * log(vst_agb$stemDiameter)) -
                                                            (0.0299 * (log(vst_agb$stemDiameter))^2)),
                                                      digits = 2),

                                              TRUE ~ NA)) %>%

    #   Assign allometry source for tropical species
    dplyr::mutate(source = dplyr::case_when(is.na(.data$agb_trop) ~ .data$source,
                                            TRUE ~ "Chave_etal_2014")) %>%

    #   Update "agb" biomass column with Chave estimates for tropical species
    dplyr::mutate(agb = dplyr::case_when(is.na(.data$agb_trop) ~ .data$agb,
                                         TRUE ~ .data$agb_trop))



  ### Apply shrub-specific biomass equations from Conti et al. 2019 to shrub growth forms
  # Note: Conti et al. 2019 assume that multiple stems of same individual have been aggregated into a single equivalent basalStemDiameter for all stems; this calculation was performed above on the 'shrub' dataframe subset.

  #   Calculate mean crown diameter for shrubs using max/ninetyCrownDiameter inputs; confirmed with G. Conti that geometric mean might be more appropriate but arithmetic mean was used to construct allometries.
  vst_agb <- vst_agb %>%
    dplyr::mutate(meanCrownDiameter = dplyr::case_when(.data$growthForm %in% c("single shrub", "small shrub") &
                                                         !is.na(.data$maxCrownDiameter) &
                                                         !is.na(.data$ninetyCrownDiameter) ~
                                                         round(rowMeans(dplyr::across(c("maxCrownDiameter",
                                                                                        "ninetyCrownDiameter")),
                                                                        na.rm = TRUE),
                                                               digits = 1),
                                                       TRUE ~ NA),
                  .after = "ninetyCrownDiameter")


  #   Estimate shrub biomass: Case when basalStemDiameter is missing and mean crownDiameter and height are available (biomass estimate with most uncertainty)
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$meanCrownDiameter) & !is.na(vst_agb$height),
                              round(exp(-0.370 + 1.903 * log(vst_agb$meanCrownDiameter) +
                                          0.652 * log(vst_agb$height)) * 1.403,
                                    digits = 2),
                              NA)

  #   Estimate shrub biomass: Improved output when basalStemDiameter is available --> less uncertainty
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$basalStemDiameter),
                              round(exp(-2.869 + 2.584 * log(vst_agb$basalStemDiameter)),
                                    digits = 2),
                              vst_agb$agb_shrub)

  #   Estimate shrub biomass: Even better output when basalStemDiameter AND mean crownDiameter available (compared to basalStemDiameter alone)
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$meanCrownDiameter) & !is.na(vst_agb$basalStemDiameter),
                              round(exp(-2.057 + 1.741 * log(vst_agb$basalStemDiameter) + 0.945 *
                                          log(vst_agb$meanCrownDiameter)),
                                    digits = 2),
                              vst_agb$agb_shrub)

  #   Estimate shrub biomass: Best output when basalStemDiameter, mean crownDiameter, AND height are all available
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$meanCrownDiameter) & !is.na(vst_agb$basalStemDiameter) &
                                !is.na(vst_agb$height),
                              round(exp(-2.281 + 1.525 * log(vst_agb$basalStemDiameter) + 0.831 *
                                          log(vst_agb$meanCrownDiameter) + 0.523 * log(vst_agb$height)),
                                    digits = 2),
                              vst_agb$agb_shrub)

  #   Assign AGB allometry source for shrubs. Citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.
  vst_agb$source <- ifelse(!is.na(vst_agb$agb_shrub),
                           "Conti_etal_2019",
                           vst_agb$source)

  #   Update AGB column with shrub biomass from Conti
  vst_agb$agb <- dplyr::if_else(vst_agb$source == "Conti_etal_2019",
                                vst_agb$agb_shrub,
                                vst_agb$agb,
                                vst_agb$agb)



  ### Assumption: Where available, species-specific allometric equations are preferable to more generic ones; update AGB estimates for taxa for which species-specific allometric equations exist

  ##  Species: Metrosideros polymorpha (MEPO5) - first estimate AGB for all MEPO5 with a stemDiameter (Litton and Kauffman 2008), then in subsequent steps update the AGB estimate with Selmants et al 2014; approach retains Litton and Kauffman 2008 estimate for those MEPO5 with DBH >= 33 cm and that do not have 'height' recorded. All other individuals have an AGB estimate via Selmants et al 2014.

  #   Allometry for all MEPO5 individuals; citation: Litton and Kauffman 2008. Allometric Models for Predicting Aboveground Biomass in Two Widespread WoodyPlants in Hawaii. BIOTROPICA 40(3): 313-320.
  vst_agb$agb_MEPO5_Litton <- ifelse(vst_agb$taxonID == "MEPO5" & !is.na(vst_agb$stemDiameter),
                                     round(0.88 * (vst_agb$stemDiameter^1.86),
                                           digits = 2),
                                     NA)

  vst_agb$source <- ifelse(!is.na(vst_agb$agb_MEPO5_Litton),
                           "Litton_Kauffman_2008_MEPO5",
                           vst_agb$source)

  vst_agb$agb <- ifelse(vst_agb$source == "Litton_Kauffman_2008_MEPO5",
                        vst_agb$agb_MEPO5_Litton,
                        vst_agb$agb)

  #   Update MEPO5 AGB estimate for individuals with DBH <= 33 cm, or > 33 cm AND with 'height' data; citation: Selmants, PC, CM Litton, CP Giardina, and GP Asner. 2014. Global Change Biology 20:2927-2937.
  vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter <= 33,
                              round(0.2085 * (vst_agb$stemDiameter^2.318),
                                    digits = 2),
                              NA)

  vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter > 33 &
                                !is.na(vst_agb$height) & !is.na(vst_agb$spg_gcm3),
                              round(0.0776 * ((vst_agb$spg_gcm3 * (vst_agb$stemDiameter^2) * vst_agb$height)^0.94),
                                    digits = 2),
                              vst_agb$agb_MEPO5)

  #   Update AGB allometry for MEPO5 that have a new value in "agb_MEPO5" column
  vst_agb$source <- ifelse(!is.na(vst_agb$agb_MEPO5),
                           "Selmants_etal_2014_MEPO5",
                           vst_agb$source)

  #   Update "agb" column with Selmants et al 2014 estimates
  vst_agb$agb <- ifelse(vst_agb$source == "Selmants_etal_2014_MEPO5",
                        vst_agb$agb_MEPO5,
                        vst_agb$agb)


  ##  Species: Rhamnus davurica (RHDA); citation: Zhang et al 2012. Sexual dimorphism in reproductive and vegetative allometry for two dioecious Rhamnus plants in north-eastern China. Eur J Forest Res (2012) 131:1287-1296.
   # The taxonID RHDA is the most frequent introduced species in NEON VST dataset, and Zhang et al 2012 have a specific equation for RHDA. There is one equation for males and another for females; here, we take the average because NEON does not record sex of RHDA. Output is divided by 1000 to convert to "kg".
  vst_agb <- vst_agb %>%
    dplyr::mutate(agb_RHDA = dplyr::case_when(.data$taxonID == "RHDA" & !is.na(.data$stemDiameter) ~
                                                round(0.001 * ((exp(5.237 + 1.996 * log(.data$stemDiameter)) +
                                                                  exp(5.016 + 2.306 * log(.data$stemDiameter))) / 2),
                                                      digits = 2),
                                              TRUE ~ NA)) %>%

    #   Update AGB allometry citation for RHDA individuals
    dplyr::mutate(source = ifelse(!is.na(.data$agb_RHDA),
                                  "Zhang_etal_2012_RHDA",
                                  .data$source)) %>%

    #   Update "agb" column with Zhang et al 2012 estimates
    dplyr::mutate(agb = ifelse(.data$source == "Zhang_etal_2012_RHDA",
                               .data$agb_RHDA,
                               .data$agb))


  ##  Species: Cornus spp; citation: Lutz, J.A., K.A. Schwindt, T.J. Furniss, J.A. Freund, M.E Swanson, K.J. Hogan, G.E. Kenagy, and A.J. Larson. 2014. Community composition and allometry of Leucothoe davisiae, Cornus sericea, and Chrysolepis sempervirens. Canadian Journal of Forest Research 44:677-683. Output divided by 1000 to convert to "kg".

  #   Estimate AGB for individuals with a basalStemDiameter; most emergent shrub stems have basalStemDiameter but a small number of are occluded from measurement.
  vst_agb <- vst_agb %>%
    dplyr::mutate(agb_Cornus = dplyr::case_when(grepl("Cornus", .data$scientificName) & !is.na(.data$basalStemDiameter) &
                                                  .data$growthForm %in% c("single shrub", "small shrub") &
                                                  is.na(.data$stemDiameter) ~
                                                  round(exp(3.315 + 2.647 * log(.data$basalStemDiameter)) / 1000,
                                                        digits = 3),
                                                grepl("Cornus", .data$scientificName) & !is.na(.data$stemDiameter) &
                                                  .data$growthForm == "single shrub" ~
                                                  round(exp(5.089 + 1.883 * log(.data$stemDiameter)) / 1000,
                                                        digits = 3),
                                                TRUE ~ NA),

                  #   Update AGB allometry citation for Cornus individuals
                  source = dplyr::case_when(!is.na(.data$agb_Cornus) ~ "Lutz_etal_2014_Cornus",
                                                TRUE ~ .data$source),

                  #   Update "agb" column with Lutz et al 2014 estimates
                  agb = dplyr::case_when(!is.na(.data$agb_Cornus) ~ .data$agb_Cornus,
                                         TRUE ~ .data$agb))


  ##  Assumption: Allometric equations developed specifically for lianas are better than generic allometric equations used above for trees and shrubs. Citation: Schnitzer, SA, SJ DeWalt, and J Chave. 2006. Censusing and measuring lianas: A quantitative comparison of the common methods. Biotropica 38:581-591.
  #   Update AGB for lianas with equations from Schnitzer_et_al_2006 (Chojnacky is not intended for lianas, or for introduced or tropical species, and there are numerous introduced and tropical liana species, see below). Equation for tropical lianas is used for temperate liana species.
  vst_agb$agb_liana <- ifelse(vst_agb$growthForm == "liana" & !is.na(vst_agb$stemDiameter),
                              round(exp(-1.484 + 2.657 * log(vst_agb$stemDiameter)),
                                    digits = 3),
                              NA)

  #   Update AGB allometry citation for lianas
  vst_agb$source <- ifelse(!is.na(vst_agb$agb_liana),
                           "Schnitzer_etal_2006",
                           vst_agb$source)

  #   Update AGB column with Schnitzer et al 2006 estimates
  vst_agb$agb <- ifelse(!is.na(vst_agb$agb_liana),
                        vst_agb$agb_liana,
                        vst_agb$agb)


  ##  Retain only those records with unambiguous live or dead plantStatus values that contribute to standing AGB
  #   Identify missing and downed individuals and bind to missing/downed from vst_non-woody
  missingDownedDF <- dplyr::bind_rows(missingDownedDF,
                           vst_agb %>%
                             dplyr::filter(.data$plantStatus %in% missingDowned | is.na(.data$growthForm) |
                                             is.na(.data$agb)) %>%
                             dplyr::rename("agb_kg" = "agb") %>%
                             dplyr::select("plot_eventID",
                                           "domainID",
                                           "siteID",
                                           "year",
                                           "eventID",
                                           "date",
                                           "nlcdClass",
                                           "plotID",
                                           "subplotID",
                                           "taxonID",
                                           "scientificName",
                                           "individualID",
                                           "plantStatus",
                                           "growthForm",
                                           "measurementHeight",
                                           "stemDiameter",
                                           "basalStemDiameter",
                                           "height",
                                           "maxCrownDiameter",
                                           "ninetyCrownDiameter",
                                           "source",
                                           "agb_kg"))

  #   Add 'eventType', 'plotType', and 'dataCollected' from perPlot table
  missingDownedDF <- dplyr::left_join(missingDownedDF,
                                      perPlot %>%
                                        dplyr::select("plot_eventID",
                                                      "plotType",
                                                      "eventType",
                                                      "dataCollected"),
                                      by = "plot_eventID") %>%
    dplyr::relocate("plotType":"dataCollected",
                    .before = "plotID") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID,
                   .data$individualID)

  #   Retain live and dead individuals that contribute to standing AGB; create 'simplePlantStatus' == "live|dead"
  vst_agb <- vst_agb %>%
    dplyr::filter(.data$plantStatus %in% standingLiveDead,
                  !is.na(.data$agb)) %>%
    dplyr::mutate(simplePlantStatus = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                       TRUE ~ "dead"),
                  .after = "plantStatus")


   ##  Aggregate woody biomass data by "individualID' x 'simplePlantStatus' x 'year'; assumes that multiple instances of same individualID are true multiple boles and not accidental duplicates. Output is used for both annual biomass summaries and NPP calculations for specified consecutive years.
  vst_agb_kg <- vst_agb %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$date,
                    .data$nlcdClass,
                    .data$plotID,
                    .data$subplotID,
                    .data$taxonID,
                    .data$family,
                    .data$genus,
                    .data$scientificName,
                    .data$individualID,
                    .data$simplePlantStatus,
                    .data$growthForm,
                    .data$totalSampledAreaTrees,
                    .data$totalSampledAreaShrubSapling,
                    .data$totalSampledAreaLiana) %>%
    dplyr::summarise(stemDiameter = round(sqrt(sum(.data$stemDiameter^2)),
                                          digits = 1),
                     basalStemDiameter = round(sqrt(sum(.data$basalStemDiameter^2)),
                                               digits = 1),
                     height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA),
                     maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                               max(.data$maxCrownDiameter, na.rm = TRUE),
                                               NA),
                     ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                  max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                  NA),
                     source = paste(unique(.data$source), collapse = ", "),
                     agb_kg = sum(.data$agb, na.rm = TRUE),
                     .groups = "drop")


  ##  Assign total sampled area by growthForm into a single column
  vst_agb_kg <- vst_agb_kg %>%
    dplyr::mutate(sampledArea_m2 = dplyr::case_when(growthForm %in% c("single bole tree",
                                                                      "multi-bole tree") ~ .data$totalSampledAreaTrees,
                                                    growthForm %in% c("single shrub",
                                                                      "small shrub",
                                                                      "small tree",
                                                                      "sapling") ~ .data$totalSampledAreaShrubSapling,
                                                    growthForm == "liana" ~ .data$totalSampledAreaLiana,
                                                    TRUE ~ NA))


  ##  Remove unneeded totalSampledArea columns
  vst_agb_kg <- vst_agb_kg %>%
    dplyr::select(-"totalSampledAreaTrees",
                  -"totalSampledAreaShrubSapling",
                  -"totalSampledAreaLiana")


  ##  Combine AGB for vst_apparentindividual and vst_nonWoody
  if (nrow(vst_agb_final_other) > 0) {

    vst_agb_kg <- dplyr::bind_rows(vst_agb_kg,
                                   vst_agb_final_other)

  }


  ##  Join with perPlot to obtain 'plotType', 'eventType', and 'dataCollected' fields and arrange for output
  vst_agb_kg <- dplyr::left_join(vst_agb_kg,
                           perPlot %>%
                             dplyr::select("plot_eventID",
                                           "plotType",
                                           "eventType",
                                           "dataCollected"),
                           by = "plot_eventID") %>%
    dplyr::relocate("plotType":"dataCollected",
                    .before = "plotID") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID,
                   .data$individualID)



  ### Scaling: Determine biomass per unit area (Mg/ha) ####

  ##  Identify records with full sampling of each plot --> remove records that cannot be scaled to a per area basis, and create "Mg/ha" estimate for downstream plot- and site-level outputs; filtering only on is.na(sampledArea_m2) should be sufficient, and "dataCollected" filter is insurance.

  vst_agb_Mgha <- vst_agb_kg %>%

    #   Conditional growthFormSubset filtering: Include dataCollected == "partial" when only estimating "trees", as "partial" means trees were sampled throughout the plot but no smaller growth forms.
    dplyr::filter(dplyr::case_when(growthFormSubset == "tree" ~
                                     (.data$dataCollected != "dendrometerOnly" | is.na(.data$dataCollected)) &
                                     !is.na(.data$sampledArea_m2) & .data$sampledArea_m2 > 0,
                                   growthFormSubset == "all" ~ !.data$dataCollected %in% c("dendrometerOnly", "partial") &
                                     !is.na(.data$sampledArea_m2) & .data$sampledArea_m2 > 0)) %>%

    #   Calculate "Mg/ha" AGB for each individual with full plot sampling
    dplyr::mutate(agb_Mgha = round(.data$agb_kg * 0.001 * (10000 / .data$sampledArea_m2),
                                   digits = 4),
                  .after = "agb_kg")


  ##  Identify 'plot_eventIDs' with full sampling and no qualifying biomass
  #   Create list of 'plot_eventID' values from fully sampled plots that have downstream 'appInd' data
  plot_eventID_appInd <- unique(vst_agb_Mgha$plot_eventID)

  #   Create list of 'plot_eventID' values for fully sampled plots with NO downstream 'appInd' data
  plot_eventID_zeros <- setdiff(plot_eventID_full, plot_eventID_appInd)

  #   Create data frame of zeros via filtering 'perPlot' table
  if (length(plot_eventID_zeros) > 0) {

    vst_agb_zeros <- perPlot %>%
      dplyr::filter(.data$plot_eventID %in% plot_eventID_zeros) %>%
      dplyr::select(-"totalSampledAreaTrees",
                    -"totalSampledAreaShrubSapling",
                    -"totalSampledAreaLiana",
                    -"totalSampledAreaOther") %>%
      dplyr::mutate(live_Mgha = 0,
                    dead_Mgha = 0)

  } else {

    vst_agb_zeros <- data.frame()

  }



  ### Generate plot-level biomass summary ####

  #   Sum biomass per unit area for each 'plotID' x 'year' x 'simplePlantStatus' x 'nlcdClass': Aggregate across individualIDs
  vst_plot_summary <- vst_agb_Mgha %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$eventID,
                    .data$nlcdClass,
                    .data$plotType,
                    .data$eventType,
                    .data$dataCollected,
                    .data$plotID,
                    .data$simplePlantStatus) %>%
    dplyr::summarise(agb_Mgha = round(sum(.data$agb_Mgha, na.rm = TRUE),
                                      digits = 2),
                     .groups = "drop")

  #   Within a given year, transpose live and dead AGB into separate columns
  vst_plot_summary <- tidyr::pivot_wider(vst_plot_summary,
                                      id_cols = c("plot_eventID",
                                                  "domainID",
                                                  "siteID",
                                                  "year",
                                                  "eventID",
                                                  "nlcdClass",
                                                  "plotType",
                                                  "eventType",
                                                  "dataCollected",
                                                  "plotID"),
                                      names_from = "simplePlantStatus",
                                      names_glue = "{simplePlantStatus}_Mgha",
                                      values_from = "agb_Mgha")

  #   Add column "live_Mgha' if missing; may happen if all standing biomass in plot is dead
  if (!"live_Mgha" %in% names(vst_plot_summary)) {

    vst_plot_summary$live_Mgha <- NA

  }

  #   Add column 'dead_Mgha' if missing; may happen if all standing biomass plot is live
  if (!"dead_Mgha" %in% names(vst_plot_summary)) {

    vst_plot_summary$dead_Mgha <- NA

  }

  #   Assumption: Replace NAs created during transpose with zeroes; assume both live and dead were sampled in a plot
  vst_plot_summary$dead_Mgha[is.na(vst_plot_summary$dead_Mgha)] <- 0
  vst_plot_summary$live_Mgha[is.na(vst_plot_summary$live_Mgha)] <- 0

  #   Add rows for plots with zero biomass to plots with AGB, calculate total "agb_Mgha"
  vst_plot_summary <- dplyr::bind_rows(vst_plot_summary,
                                       vst_agb_zeros) %>%
    dplyr::relocate("live_Mgha",
                    .before = "dead_Mgha") %>%
    dplyr::mutate(agb_Mgha = rowSums(dplyr::across(c("live_Mgha", "dead_Mgha")), na.rm = TRUE),
                  .before = "live_Mgha") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotID)



  ### Generate site-level biomass summary ####

  ##  Create site-level summary table: mean, sd, n()
  if(nrow(vst_plot_summary) > 0) {

    vst_site_summary <- vst_plot_summary %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year) %>%
      dplyr::summarise(woodPlotNum = dplyr::n(),
                       woodPlotType = paste(unique(.data$plotType), collapse = ", "),
                       woodMassMean_Mgha = round(mean(.data$agb_Mgha, na.rm = TRUE),
                                                 digits = 1),
                       woodMassSD_Mgha = round(stats::sd(.data$agb_Mgha, na.rm = TRUE),
                                               digits = 1),
                       woodLiveMassMean_Mgha = round(mean(.data$live_Mgha, na.rm = TRUE),
                                                     digits = 1),
                       woodLiveMassSD_Mgha = round(stats::sd(.data$live_Mgha, na.rm = TRUE),
                                                   digits = 1),
                       woodDeadMassMean_Mgha = round(mean(.data$dead_Mgha, na.rm = TRUE),
                                                     digits = 1),
                       woodDeadMassSD_Mgha = round(stats::sd(.data$dead_Mgha, na.rm = TRUE),
                                                   digits = 1),
                       .groups = "drop") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year)

  } else {

    vst_site_summary <- "Could not create site-level output table: Insufficient plot-level data"

  } # end nrow() conditional



  ### Bundle and return output ####
  output.list <- list(vst_agb_kg = vst_agb_kg,
                      vst_missing = missingDownedDF,
                      vst_plot_Mgha = vst_plot_summary,
                      vst_site_Mgha = vst_site_summary)

  return(output.list)
}