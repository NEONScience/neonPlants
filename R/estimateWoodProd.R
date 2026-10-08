#' @title Estimate woody ANPP for a NEON site
#'
#' @author
#' Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#' Claire K Lunch \email{clunch@BattelleEcology.org} \cr
#'
#' @description Calculate above-ground net primary productivity of trees reported in the NEON "Vegetation structure" data product (DP1.10098.001). Data must be provided one site at a time, and input data must be NEON RELEASE-2027 or newer.. Results are summarized as mass per unit area per year at scales of the plotID and siteID.
#'
#' Data inputs are "Vegetation structure" data for a single site (DP1.10098.001) in list format, either provided via the neonUtilities::loadByProduct() function (preferred), as data tables downloaded from the NEON Data Portal, or as input tables with an equivalent structure.
#'
#' Data must be provided to the function one site at a time, and the 'vst_mappingandtagging' table should include all years of data from the beginning of collection to the last year being analyzed. Returning the full dataset in 'vst_mappingandtagging' is the default behavior of neonUtilities::loadByProduct().
#'
#' @details The input data are passed to the companion estimateWoodMass() function to estimate biomass for qualifying trees, and then aboveground net primary productivity is calculated for live trees at each timepoint. Input data are filtered by the 'plotSubset' argument if output for only certain types of plots or sampling intervals is desired. Productivity is summarized on an areal basis with units "Mg/ha/yr" at the hierarchical level of the plot and site.
#'
#' For trees, the individual-level approach to calculating productivity is used from Clark DA, S Brown, DW Kicklighter, JQ Chambers, JR Thomlinson, and J Ni. 2001. Measuring Net Primary Production in Forests: Concepts and Field Methods. Ecological Applications 11:356-370. With this approach, NEON data enable calculating woody productivity for individuals with a growthForm of "single bole tree" or "multi-bole tree".
#'
#' NEON has an extensive data QA/QC process, but users should be aware that productivity estimates are very sensitive to data entry errors and so the function output should be examined carefully.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) for a single site, downloaded using the neonUtilities::loadByProduct() function. Expected input table names are "vst_perplotperyear", "vst_mappingandtagging", and "vst_apparentindividual"; it is optional to include the "vst_non-woody" table in the list. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for the 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments. [list]
#'
#' @param inputIndividual The 'vst_apparentindividual' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputMapTag The 'vst_mappingandtagging' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputPerPlot The 'vst_perplotperyear' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputNonWoody The 'vst_non-woody' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param plotSubset Options are the default of "all" (all Tower and Distributed plots), "towerAll" (all plots in the Tower airshed but no Distributed plots), "towerAnnualSubset" (the subset of n=5 Tower plots that are sampled annually), and "distributed" (all Distributed plots, which are sampled at 5-yr intervals and are spatially representative of the NLCD classes at a site). [character]
#'
#' @param flagged Select how to handle individuals flagged for implausibly large stem diameter increments: "retain" (default) includes flagged individuals in calculations and outputs them to a "flagged" table for review, or "filter" removes all records with ≥3.5 cm absolute annual stem diameter increment (including recruits with inferred initial diameter ≥10 cm) and sends flagged individuals to the "flagged" table. [character]
#'
#' @param missing Select how to handle individuals missed during sampling and for which plantStatus and biomass cannot be inferred/estimated: "filter" (default) removes missed individuals before productivity calculations and collates them in a "missing" table. The "retain" option assumes missing individuals are dead, and these individuals may contribute to ANPP. [character]
#'
#' @return A list that includes productivity summary data frames. Output tables are:
#'   * vst_ANPP_indiv - Woody ANPP for each individual at each time step for which data exist ("Mg/ha/yr").
#'   * vst_ANPP_plot - Summarizes woody ANPP for each plot x year combination ("Mg/ha/yr").
#'   * vst_ANPP_site - Summarizes woody ANPP for each site x year combination ("Mg/ha/yr").
#'   * vst_ANPP_duplicates - Duplicated single- or multi-bole tree individualIDs within a given sampling event (i.e., "eventID"). The individualID should be unique for single- and multi-bole tree growthForms within an eventID; duplicates are removed before productivity calculations are carried out.
#'   * vst_ANPP_flagged - Individuals flagged for changes in stemDiameter > 3.5 cm/yr; includes records from all time points for flagged individuals. By default, the 'flagged' argument is "retain" and the records in this table are included in the productivity calculation.
#'   * vst_ANPP_missing - Individuals that were missed during a sampling event; table is populated only when the 'missing' argument is set to "filter" (default).
#'   * variables - Units and definitions of novel variables created by the function that are not already defined in the Vegetation Structure data product.
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Vegetation structure for a single site; note that a token is required and may be obtained after creating a NEON user account
#' vstDF <- neonUtilities::loadByProduct(
#' dpID = "DP1.10098.001",
#' site = "ABBY",
#' package = "basic",
#' check.size = FALSE,
#' token = "my_NEON_token"
#' )
#'
#' woodProdOutput <- neonPlants::estimateWoodProd(inputDataList = vstDF)
#'
#' }
#'
#' @export estimateWoodProd

estimateWoodProd <- function(inputDataList,
                             inputIndividual = NA,
                             inputMapTag = NA,
                             inputPerPlot = NA,
                             inputNonWoody = NA,
                             plotSubset = "all",
                             flagged = "retain",
                             missing = "filter") {



  ### SESSION: SET SESSION BEHAVIOR FOR 'DPLYR::SUMMARISE' ####
  sessionInform <- getOption("dplyr.summarise.inform", default = TRUE)
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = sessionInform), add = TRUE)



  ### INPUT VERIFICATION: CHECK THAT INPUT ARGUMENTS MEET ASSUMPTIONS ####

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
  
  #   Check for RELEASE-2027 or more recent
  if ("release" %in% names(perPlot)) {
    
    releaseValue <- unique(perPlot$release)
    
    if (length(releaseValue) > 1) {
      
      stop("Data from more than one NEON RELEASE detected: Function does not support using data from multiple RELEASES.")
      
    } else {
      
      releaseCheck <- dplyr::case_when(releaseValue == "LATEST" ~ TRUE,
                                       as.numeric(stringr::str_extract(releaseValue, "20[0-9]{2}$")) >= 2027 ~ TRUE,
                                       TRUE ~ FALSE)
      
      if (!isTRUE(releaseCheck)) {stop("Input data must be RELEASE-2027 or newer.")}
      
    }
    
  } else {
    warning("Cannot determine the NEON RELEASE for the input data: Outputs may contain known errors if data older than RELEASE-2027 are used.")
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



  ### Verify only one siteID is in input data
  if (length(unique(perPlot$siteID)) > 1) {

    excessSites <- paste(sort(unique(perPlot$siteID)), collapse = ", ")
    stop(glue::glue("Woody productivity may only be estimated for one siteID at a time. The input data set currently contains data for: {excessSites}"))

  }

  #   Message to user if PUUM data are supplied
  if ("PUUM" %in% perPlot$siteID) {
    message("ANPP estimates for the PUUM site do not currently include tree ferns")
  }



  ### Verify optional input arguments meet requirements

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The 'plotSubset' argument must be one of: 'all', 'towerAll', 'towerAnnualSubset', 'distributed'")
  }

  # Error if invalid 'flagged' option selected
  if (!flagged %in% c("filter", "retain")) {
    stop("The 'flagged' argument must be one of: 'filter', 'retain'")
  }

  # Error if invalid 'missing' option selected
  if (!missing %in% c("filter", "retain")) {
    stop("The 'missing' argument must be one of: 'filter', 'retain'")
  }
  
  
  
  ### Assign plotType needed in output based on 'plotSubset' argument
  plotType <- dplyr::case_when(plotSubset == "all" ~ "all",
                               plotSubset == "distributed" ~ "distributed",
                               plotSubset %in% c("towerAll", "towerAnnualSubset") ~ "tower")
  
  
  
  
  
  ### PREPARE PERPLOT INPUT DATA ####
  
  ##  Extract year from eventID, create 'plotID x eventID' identifier
  perPlot <- perPlot %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$eventID, "20[0-9]{2}$")),
                  .before = "eventID") %>%
    dplyr::mutate(plot_eventID = paste(.data$plotID, .data$eventID, sep = "_"),
                  .before = "plotID")
  
  
  ##  Remove duplicates: Sort by date before removing duplicates so that if duplicates are from different dates the record from latest date will be retained. Sorting by date and then using fromLast = TRUE retains the most recent version of duplicates.
  perPlot <- perPlot[order(perPlot$date), ]
  perPlot <- perPlot[!duplicated(perPlot$plot_eventID, fromLast = TRUE), ]
  
  
  ##  Join with plot priority data; the 'specificModuleSamplingPriority' field is used to optionally filter only to plots with priority 1-5 when user-supplied 'plotSubset' == "towerAnnualSubset"
  data("priority_plots", envir = environment())
  
  priority_plots <- priority_plots %>%
    dplyr::select("plotID",
                  "specificModuleSamplingPriority")
  
  perPlot <- dplyr::left_join(perPlot,
                              priority_plots,
                              by = "plotID")
  
  
  ##  Filter plots to user-supplied 'plotSubset'
  #   Conditionally retain only Tower annual subset
  perPlot <- perPlot %>%
    dplyr::filter(as.logical(dplyr::case_when(
      plotSubset == "towerAnnualSubset" & .data$specificModuleSamplingPriority <= 5 ~ TRUE,
      plotSubset != "towerAnnualSubset" &
        (is.na(.data$specificModuleSamplingPriority) | .data$specificModuleSamplingPriority <= 50) ~ TRUE,
      TRUE ~ FALSE
    )))
  
  #   Conditionally retain all Tower plots
  if (plotSubset == "towerAll") {
    perPlot <- perPlot[which(perPlot$plotType == "tower"),]
  }
  
  #   Conditionally retain Distributed plots
  if (plotSubset == "distributed") {
    perPlot <- perPlot[which(perPlot$plotType == "distributed"),]
  }
  
  
  ##  Identify plot_eventIDs with full plot sampling 
  
  
  #--> account for samplingImpractical and dataCollected variations; then remove plot-events from perPlot that are NOT full plot sampling
  
  



  ### PREPARE APPARENT INDIVIDUAL INPUT DATA ####
  
  ###-----------> Filter out records not associated with full plot sampling for trees
  
  
  ### Filter to 'tree' growthForms
  appInd <- appInd %>%
    dplyr::filter(.data$growthForm %in% c("single bole tree", "multi-bole tree"))
  
  

  ### Remove duplicates: Dupes cause problems when calculating 'estimatedMass' in calculateTransitions() function
  #   Identify individualID x eventID combos for "tree" growthForms that are duplicated (more prevalent in older data)
  treeDupes <- appInd %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-")) %>%
    dplyr::filter(duplicated(.data$indivEventID))

  #   Extract all duplicated individualID x eventID records; 'treeDupes' only contains one of each pair
  treeDupeDF <- appInd %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-")) %>%
    dplyr::filter(.data$indivEventID %in% treeDupes$indivEventID) %>%
    dplyr::arrange(.data$plotID,
                   .data$eventID,
                   .data$individualID)

  #   Remove all duplicate records from 'appInd' table
  appInd <- appInd %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-")) %>%
    dplyr::filter(!.data$indivEventID %in% treeDupes$indivEventID) %>%
    dplyr::select(-"indivEventID")

  rm(treeDupes)
  
  
  
  ### Create 'liveDeadStatus' field to parse standing biomass unambiguously
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
  appInd <- appInd %>%
    dplyr::mutate(liveDeadStatus = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                    .data$plantStatus %in% tail(standingLiveDead, 2) ~ "dead",
                                                    .data$plantStatus %in% head(lostDowned, 2) ~ "dead",
                                                    .data$plantStatus %in% tail(lostDowned, 5) ~ "lost",
                                                    TRUE ~ NA_character_),
                  .after = "plantStatus")
  
  
  
  #--> Need to assign liveDeadStatus, identify transitions, interpolate missing stemDiameters, and flag implausible stemDiameter increments *before* biomass is calculated...
  #--> Problem: Calculating transitions at this point means a number of variables created need to be passed through estimateWoodMass which is difficult due to calls to group_by() --> use only allometric mass function instead?

  
  
  
  ### ESTIMATE BIOMASS OF TREES ####

  ### Generate wood mass estimates
  
  #--> Replace code below with output from estimateAllometricWoodMass function; need to verify what 'agb_kg' output looks like for lost/downed individuals.
  
  # woodMassOutput <- neonPlants::estimateWoodMass(
  #   inputIndividual = appInd,
  #   inputMapTag = map,
  #   inputPerPlot = perPlot,
  #   plotSubset = plotSubset,
  #   growthFormSubset = "tree"
  # )
  # 
  # 
  # ##  Extract required estimateWoodMass output tables
  # agb <- woodMassOutput$vst_AGB_indiv
  # lostDowned <- woodMassOutput$vst_lost_downed
  # 
  # 
  # 
  # ### Prepare outputs from estimateWoodMass
  # 
  # ##  Create unified 'agb' data frame that includes lost/downed individuals (no growthform, or plantStatus 'downed', 'lost', or 'no longer qualifies')
  # agb <- dplyr::bind_rows(agb,
  #                         lostDowned %>%
  #                           dplyr::select(-"tempStemID", -("measurementHeight":"dataQF"))
  #                         ) %>%
  #   dplyr::select(-"sampledArea_m2")



  ### FIND MORTALITY AND RECRUITMENT EVENTS #### -------------------------------> move up before biomass estimation

  if (nrow(agb) > 0) {

    transitions <- calculateTransitions(biomassTable = agb,
                                        plotYearTable = perPlot)

  } else {

    message("No tree biomass found.")
    return(invisible())

  }



  ### CALCULATE BIOMASS CHANGES FROM INCREMENT, RECRUITMENT, AND MORTALITY ####

  increment <- estimateIncrement(biomassTable = transitions,
                                 missing = missing,
                                 flagged = flagged)

  agbIncrDF <- increment$agbIncrDF
  missingDF <- increment$missingDF
  flaggedDF <- increment$flaggedDF



  ### PLOT SCALE: DETERMINE PLOT-LEVEL PRODUCTION (Mg/ha/y) ####

  ##  Calculate increment per unit area using 'totalSampledAreaTrees'
  agbIncrDF <- agbIncrDF %>%
    dplyr::mutate(
      agbIncr_Mghayr = dplyr::case_when(

        !is.na(.data$agbIncr_kgyr) & !is.na(.data$totalSampledAreaTrees) ~ (.data$agbIncr_kgyr / .data$totalSampledAreaTrees) * 10,

        TRUE ~ NA_real_
      ),
      .before = "agbIncr_kgyr"
    )


  ##  Sum increment for each plotID x eventID combination and convert to Mg/ha/y
  plotDF <- agbIncrDF %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$eventID,
                    .data$year,
                    .data$plotID,
                    .data$plotType,
                    # .data$nlcdClass, #--> causes problems since NA in some older data
                    .data$eventType,
                    .data$dataCollected) %>%

    dplyr::summarise(

      #   Deal with NAs present in nlcdClass
      nlcdClass = dplyr::case_when(
        all(is.na(.data$nlcdClass)) ~ NA,
        TRUE ~ paste(unique(na.omit(.data$nlcdClass)), collapse = ", ")
        ),

      #   Sum biomass increment at plotID x eventID level --------------------------> inappropriately returns 0 for first year
      woodProd_Mghayr = dplyr::case_when(
        all(is.na(.data$agbIncr_Mghayr)) ~ 0,
        TRUE ~ round(sum(.data$agbIncr_Mghayr, na.rm = TRUE), digits = 2)
      ),

      #   Determine count of individualIDs contributing to plot-level increment sum
      treeCount = dplyr::case_when(
        all(is.na(.data$agbIncr_Mghayr)) ~ 0,
        TRUE ~ sum(!is.na(.data$agbIncr_Mghayr))
      ),

      .groups = "drop"
    ) %>%
    dplyr::relocate("nlcdClass",
                    .after = "plotType") %>%

    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$plotID,
                   .data$year)



  ### SITE SCALE: DETERMINE SITE-LEVEL PRODUCTION AND UNCERTAINTY (Mg/ha/y) ####
  siteDF <- plotDF %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$eventID,
                    .data$year) %>%
    dplyr::summarise(

      #   Deal with NAs present in nlcdClass
      nlcdClass = dplyr::case_when(
        all(is.na(.data$nlcdClass)) ~ NA,
        TRUE ~ paste(unique(na.omit(.data$nlcdClass)), collapse = ", ")
      ),

      #   Report comma-separated plotType(s)
      plotType = paste(sort(unique(.data$plotType)), collapse = ", "),

      #   Determine plot count
      plotCount = dplyr::n(),

      #   Calculate mean biomass increment at siteID x eventID level
      woodProdSite_Mghayr = round(mean(.data$woodProd_Mghayr, na.rm = TRUE), digits = 2),

      #   Determine biomass increment Standard Deviation
      woodProdSD_Mghayr = round(stats::sd(.data$woodProd_Mghayr, na.rm = TRUE), digits = 2),

      .groups = "drop"
    ) %>%

    dplyr::rename("woodProd_Mghayr" = "woodProdSite_Mghayr")



  ### VARIABLES: PROCESS FUNCTION-SPECIFIC VARIABLES FOR OUTPUT ################
  data("variables", envir = environment())

  variables <- variables %>%
    dplyr::filter(.data$functionName == "estimateWoodProd")



  ### OUTPUT ###################################################################

  output <- list(
    vst_ANPP_indiv = agbIncrDF,
    vst_ANPP_plot = plotDF,
    vst_ANPP_site = siteDF,
    vst_ANPP_duplicates = treeDupeDF,
    vst_ANPP_flagged = flaggedDF,
    vst_ANPP_missing = missingDF,
    variables = variables
  )

  return(output)

}
