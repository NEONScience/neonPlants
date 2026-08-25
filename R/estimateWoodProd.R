#' @title Estimate woody ANPP for a NEON site
#'
#' @author
#' Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#' Claire K Lunch \email{clunch@BattelleEcology.org} \cr
#'
#' @description Calculate above-ground net primary productivity of trees reported in the NEON "Vegetation structure" data product (DP1.10098.001). Data must be provided one site at a time. Results are summarized as mass per unit area per year at scales of the plotID and siteID.
#'
#' Data inputs are "Vegetation structure" data for a single site (DP1.10098.001) in list format, either provided via the neonUtilities::loadByProduct() function (preferred), as data tables downloaded from the NEON Data Portal, or as input tables with an equivalent structure and representing the same site x month combinations.
#'
#' Data must be provided to the function one site at a time, and the 'vst_mappingandtagging' table should include all years of data from the year 2013 to the last year being analyzed.
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
#'   * duplicates -
#'   * flagged - Individuals flagged for changes in stemDiameter > 3.5 cm/yr; includes records from all time points for flagged individuals. By default, the 'flagged' argument is "retain" and the records in this table are included in the productivity calculation.
#'   * missing - Individuals that were missed during a sampling event; table is populated only when the 'missing' argument is set to "filter" (default).
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Vegetation structure for a single site
#' VstDat <- neonUtilities::loadByProduct(
#' dpID = "DP1.10098.001",
#' site = "ABBY",
#' package = "basic",
#' check.size = FALSE,
#' token = "my_NEON_token"
#' )
#'
#' woodProdOutput <- neonPlants::estimateWoodProd(inputDataList = VstDat)
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



  ### Verify only one siteID is in input data
  if (length(unique(perPlot$siteID)) > 1) {

    excessSites <- paste(sort(unique(perPlot$siteID)), collapse = ", ")
    stop(glue::glue("Woody productivity may only be estimated for one siteID at a time. The input data set currently contains data for: {excessSites}"))

  }



  ### Verify optional input arguments meet requirements

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'towerAll', 'towerAnnualSubset', and 'distributed'.")
  }

  # Error if invalid 'missing' option selected
  if (!missing %in% c("filter", "retain")) {
    stop("The only valid 'missing' options are 'filter', 'retain'.")
  }

  # Error if invalid 'flagged' option selected
  if (!flagged %in% c("filter", "retain")) {
    stop("The only valid 'flagged' options are 'filter', 'retain'.")
  }



  ### ESTIMATE BIOMASS OF TREES ####

  ### Remove duplicates: Dupes cause problems when calculating 'estimatedMass' in calculateTransitions() function
  #   Identify individualID x eventID combos for "tree" growthForms that are duplicated (more prevalent in older data)
  treeDupes <- appInd %>%
    dplyr::filter(.data$growthForm %in% c("single bole tree", "multi-bole tree")) %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-")) %>%
    dplyr::filter(duplicated(indivEventID))

  #   Extract all duplicated individualID x eventID records; 'treeDupes' only contains one of each pair
  treeDupeDF <- appInd %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-")) %>%
    dplyr::filter(.data$indivEventID %in% treeDupes$indivEventID)

  #   Remove all duplicate records from 'appInd' table
  appInd <- appInd %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-")) %>%
    dplyr::filter(!.data$indivEventID %in% treeDupes$indivEventID) %>%
    dplyr::select(-"indivEventID")

  rm(treeDupes)


  ### Generate wood mass estimates
  woodMassOutput <- neonPlants::estimateWoodMass(
    inputIndividual = appInd,
    inputMapTag = map,
    inputPerPlot = perPlot,
    plotSubset = plotSubset,
    growthFormSubset = "tree"
  )


  ##  Extract required estimateWoodMass output tables
  agb <- woodMassOutput$vst_agb_kg
  lostDowned <- woodMassOutput$vst_lost_downed



  ### Prepare outputs from estimateWoodMass

  ##  Update 'liveDeadStatus' for "downedDead" individuals to "dead"; "standing dead" versus "downed dead" distinction not relevant for NPP
  lostDowned <- lostDowned %>%
    dplyr::mutate(liveDeadStatus = dplyr::replace_when(.data$liveDeadStatus,
                                                       .data$liveDeadStatus == "downedDead" ~ "dead"))


  ##  Create unified 'agb' data frame that includes lost/downed individuals (no growthform, or plantStatus 'downed', 'lost', or 'no longer qualifies')
  agb <- dplyr::bind_rows(agb,
                          lostDowned %>%
                            dplyr::select(-"tempStemID", -("measurementHeight":"dataQF"))
                          ) %>%
    dplyr::select(-"sampledArea_m2")



  ### FIND MORTALITY AND RECRUITMENT EVENTS ####

  if (nrow(agb) > 0) {

    transitions <- neonPlants:::calculateTransitions(biomassTable = agb,
                                                     plotYearTable = perPlot)

  } else {

    message("No tree biomass found.")
    return(invisible())

  }



  ### CALCULATE BIOMASS CHANGES FROM INCREMENT, RECRUITMENT, AND MORTALITY ####

  increment <- neonPlants:::estimateIncrement(biomassTable = transitions,
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

      #   Sum biomass increment at plotID x eventID level
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



  ### OUTPUT ###################################################################

  output <- list(
    vst_ANPP_indiv = agbIncrDF,
    vst_ANPP_plot = plotDF,
    vst_ANPP_site = siteDF,
    duplicates = treeDupeDF,
    flagged = flaggedDF,
    missing = missingDF
  )

  return(output)

}
