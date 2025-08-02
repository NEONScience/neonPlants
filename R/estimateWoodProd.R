##############################################################################################
#' @title Estimate ANPP (Aboveground Net Primary Productivity) contributed by woody vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Calculate annual productivity of woody vegetation.
#'
#' Data inputs are "Vegetation structure" data (DP1.10098.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' Data should be from just one site, and exactly two temporal eventIDs.
#'
#' @details The input data is passed to the companion estimateWoodMass() function to create biomass summaries, and then aboveground productivity is calculated for woody vegetation.
#'
#' The stand-level approach to calculating productivity (approach 2) is used from Clark DA, S Brown, DW Kicklighter, JQ Chambers, JR Thomlinson, and J Ni. 2001. Measuring Net Primary Production in Forests: Concepts and Field Methods. Ecological Applications 11:356-370.
#'
#' Woody productivity is only calculated for trees with growthForm of "single bole tree" or "multi-bole tree".
#'
#' NEON has an extensive data QA/QC process, but users should be aware that these productivity estimates are very sensitive to any residual errors and so the data should be examined carefully
#'
#' @details Input data can be filtered by 'plotSubset' if output for only certain types of plots or sampling intervals is desired. Input data are combined with allometric equation parameters and taxon specific characteristics, and biomass is estimated for each individual using allometric equations. Generalized allometric equations are applied first and are replaced by taxon-specific equations if available. Only the set of growth forms selected via the growthForm parameter are included in outputs. The non-woody "cactus" and "ferns" growthForms are not currently included. Biomass is summarized on an areal basis at the hierarchical level of the plot and site.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments. [list]
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
#' @param siteID Specify the four-letter siteID for a single NEON site (only one siteID is allowed). [character]
#'
#' @param plotSubset The available options for this function are the default of "towerAnnualSubset" (only the subset of tower plots that are sampled annually) or "towerAll" (all plots in the tower airshed but no distributed plots). [character]
#'#'
#' @param mortalityMissing Select how missing individuals are handled. Default is "filterMissing" that removes individualIDs with plantStatus NA at time 2, as well as filtering to remove from the vst_agb_kg table that is summarized to produce plot-level increment, with the individuals with plantStatus NA at time sent to "filtered" table. Alternate option is "retainMissing", which retains individuals with plantStatus NA at time 2 and assumes that they are dead and contributing to "mortality" table and increment. [character]
#'
#' @param stemIncrementFlagged Select how individuals implausibly large stem diameter increments from time 1 to time 2 are handled. Default is "filterFlagged" that removes individuals that are NA at time 1 but >= 3 cm stemDiameter increment (e.g., 9.9 + 3 = 12.9 for samplingInterval of 1 yr) at time 2, or that have an absolute stem diameter increment >= 3 from one live bout to a second live bout, with individuals sent to "filtered" table. Alternate option is "retainFlagged", which retains individuals and includes them in recruitment and vst_agb_kg tables regardless of stem diameter increment. [character]
#'
#' @return A list that includes productivity summary data frames. Output tables include:
#'   * vst_ANPP_plot - Summarizes woody ANPP for each plot x year combination ("Mg/ha/yr").
#'   * vst_ANPP_site - Summarizes woody ANPP for each site x year combination ("Mg/ha/yr").
#'
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Vegetation structure
#' VstDat <- neonUtilities::loadByProduct(
#' dpID="DP1.10098.001",
#' package = "basic",
#' check.size = FALSE
#' )
#'
#' estimateWoodProdOutputs <- estimateWoodProd(inputDataList = VstDat, siteID = "LENO")
#'
#'
#' }
#'
#' @export estimateWoodProd

estimateWoodProd = function(inputDataList,
                            inputIndividual = NA,
                            inputMapTag = NA,
                            inputNonWoody = NA,
                            inputPerPlot = NA,
                            siteID,
                            plotSubset = "towerAnnualSubset",
                            mortalityMissing = "filterMissing",
                            stemIncrementFlagged = "filterFlagged") {

  options(dplyr.summarise.inform = FALSE)

  ### Check that input arguments meet assumptions ####

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("towerAll", "towerAnnualSubset")) {
    stop("The only valid plotSubset options are 'towerAll', 'towerAnnualSubset'.")
  }

  plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50) # convert to numeric (50 is highest plotPriority)
  plotType <- "tower"

  # Error if invalid mortalityMissing option selected
  if (!mortalityMissing %in% c("filterMissing", "retainMissing")) {
    stop("The only valid mortalityMissing options are 'filterMissing', 'retainMissing'.")
  }

    # Error if invalid stemIncrementFlagged option selected
  if (!stemIncrementFlagged %in% c("filterFlagged", "retainFlagged")) {
    stop("The only valid stemIncrementFlagged options are 'filterFlagged', 'retainFlagged'.")
  }

  estimateWoodMassOutputs <- estimateWoodMass(
    inputDataList = inputDataList,
    plotSubset = plotSubset,
    growthFormSubset = "tree"
   )

#  vst_plot_Mgha <- estimateWoodMassOutputs$vst_plot_w_0s
  vst_plot_Mgha <- estimateWoodMassOutputs$vst_plot_Mgha
  vst_agb_kg <- estimateWoodMassOutputs$vst_agb_kg
  vst_missing <- estimateWoodMassOutputs$vst_missing
  
    liveList <- c("Live",
                "Live, insect damaged",
                "Live, disease damaged",
                "Live, physically damaged",
                "Live, other damage",
                "Live, broken bole",
                "No longer qualifies")

    vst_missing <- vst_missing %>%
      dplyr::mutate(simplePlantStatus = dplyr::case_when(.data$plantStatus %in% liveList ~ "live",
                                                         TRUE ~ "dead"))
    
  vst_agb_kg <- dplyr::bind_rows(vst_agb_kg, vst_missing)

  # used later when calling estimateWoodMass for recruitment
  map_input <- vst_agb_kg
  map_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here


  # filter by eventType based on plotSubset argument
  if(plotSubset == "towerAll") {
    message(glue::glue("Since plotSubset 'towerAll' was selected, input data has been filtered to just those plots in the tower airshed."))
      vst_plot_Mgha <- vst_plot_Mgha %>%
        dplyr::filter(.data$plotType == "tower")

      vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_Mgha$plot_eventID)
    }

  if(plotSubset == "towerAnnualSubset") {
    message(glue::glue("Since plotSubset 'towerAnnualSubset' was selected, input data has been filtered to just those sampling bouts when all tower plots were sampled."))
      vst_plot_Mgha <- vst_plot_Mgha %>%
        dplyr::filter(grepl("owerSubset", .data$eventType))

      vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_Mgha$plot_eventID)
    }

    ### Error if not a single site of data after filtering to the siteID in the siteID argument

    vst_plot_Mgha <- vst_plot_Mgha[vst_plot_Mgha$siteID == siteID, ]


    vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_Mgha$plot_eventID)

    sites_in_input <- unique(vst_plot_Mgha$siteID)

    if (length(sites_in_input) >1) {
    stop(glue::glue("Only one siteID is allowed in filtered dataset. Current filtered dataset has data from: {unique(vst_plot_Mgha$siteID)}"))
    }

    if(length(sites_in_input) == 0) {
    stop(glue::glue("Filtered dataset has no siteID. Select a different siteID argument and/or a different inputDataList"))
    }


    ### Error if not 2 years of data
  years_in_input <- unique(sort(vst_plot_Mgha$year))
  year1 <- min(as.numeric(years_in_input))
  year2 <- max(as.numeric(years_in_input))

    if (length(years_in_input) < 2) {
    stop(glue::glue("Two years of data are needed to calculate woody productivity. Current filtered dataset only has woody biomass data from: {unique(vst_plot_Mgha$year)}"))
    }

    if (length(years_in_input) > 2) {
     stop(glue::glue("This function expects there to be data from only two eventID years after filtering based on plotSubset. The current filtered dataset has woody biomass data from: {paste(unique(vst_plot_Mgha$eventID), collapse = ', ')}"))
    }

  samplingInterval <- abs(diff(years_in_input))

  # filter to plots with exactly 2 years in vst_plot_Mgha and provide warning if there was < 2 years or > 2 years
  vst_plot_Mgha <- vst_plot_Mgha %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(yr_count = dplyr::n_distinct(.data$year) )

  lt_2_yr <-  vst_plot_Mgha %>% dplyr::filter(.data$yr_count < 2) %>% dplyr::pull("plotID") %>% unique() %>% as.character()
  gt_2_yr <-  vst_plot_Mgha %>% dplyr::filter(.data$yr_count > 2) %>% dplyr::pull("plotID") %>% unique() %>% as.character()
  desired_2_yr <-  vst_plot_Mgha %>% dplyr::filter(.data$yr_count == 2)  %>% dplyr::pull("plotID") %>% unique() %>% as.character()

  vst_plot_Mgha <- vst_plot_Mgha %>%
    dplyr::filter(.data$yr_count == 2)

    if (length(lt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from less than two years and have been removed from vst_plot_Mgha: {paste(unique(lt_2_yr), collapse = ', ')}"))
    }

    if (length(gt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from more than two years and have been removed from vst_plot_Mgha: {paste(unique(gt_2_yr), collapse = ', ')}"))
    }

  # filter to plots with exactly 2 years in vst_agb_kg and provide warning if there was < 2 years or > 2 years
  agb_only_1_yr <-  vst_agb_kg %>% dplyr::filter(!.data$plotID %in% desired_2_yr) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(yr_count = dplyr::n_distinct(.data$year) ) %>%
    dplyr::filter(.data$yr_count < 2) %>%
    dplyr::pull("plotID") %>% unique() %>% as.character()
  agb_gt_2_yr <-  vst_agb_kg %>% dplyr::filter(!.data$plotID %in% desired_2_yr) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(yr_count = dplyr::n_distinct(.data$year) ) %>%
    dplyr::filter(.data$yr_count > 2) %>%
    dplyr::pull("plotID") %>% unique() %>% as.character()

  vst_agb_kg <- vst_agb_kg %>% dplyr::filter(.data$plotID %in% desired_2_yr)


    if (length(agb_only_1_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from less than two years and have been removed from vst_agb_kg: {paste(unique(agb_only_1_yr), collapse = ', ')}"))
    }

    if (length(agb_gt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from more than two years and have been removed from vst_agb_kg: {paste(unique(agb_gt_2_yr), collapse = ', ')}"))
    }


  ### Identify plotIDs and associated plotType in the dataset
  plotType_df <- unique(vst_plot_Mgha %>% dplyr::select("plotID", "plotType"))

  ##  Identify all plot by eventID combos in the filtered vst_plot_w_os dataframe
  plot_eventID_list <- unique(vst_plot_Mgha$plot_eventID)


  ### CALCULATE MORTALITY

  # Create placeholder 'plot_mortality' dataframe: gets overwritten later if there is any mortality
  plot_mortality <- data.frame(siteID = character(),
                                 plotID = character(),
                                 taxonID = character(),
                                 year = character(),
                                 mortality_Mghayr = numeric(),
                                 mortality_Mghayr = numeric())

  if (nrow(vst_agb_kg) > 0) {

    #   convert kg to Mg/ha
    vst_agb_kg$agb_Mgha <- round(vst_agb_kg$agb_kg * 0.001 * (10000/vst_agb_kg$sampledArea_m2),
                                 digits = 4)


    ##  Categorize individualIDs based on their changes (or not) in simplePlantStatus
    input_to_transitions <- vst_agb_kg %>%
      dplyr::select("plot_eventID",
                    "domainID",
                    "siteID",
                    "plotID",
                    "sampledArea_m2",
                    "individualID",
                    "taxonID",
                    "simplePlantStatus",
                    "year")

    #   Retain records unique with respect to individualID, taxonID, year, simplePlantStatus; don't need to worry about multi-bole smaller individuals because 'estimateWoodMass()' function combines mass for boles in output.
    input_to_transitions <- input_to_transitions %>%
      dplyr::distinct(.data$individualID,
                      .data$taxonID,
                      .data$year,
                      .data$simplePlantStatus,
                      .keep_all = TRUE)

    input_to_transitions <- input_to_transitions[order(input_to_transitions$year),]


    area_lookup <- input_to_transitions %>% dplyr::select("plotID","year","sampledArea_m2") %>% dplyr::filter(!is.na(.data$sampledArea_m2)) %>% unique()
    input_to_transitions <- input_to_transitions %>% dplyr::select(-"sampledArea_m2")
    input_to_transitions <- merge(input_to_transitions, area_lookup, by = c("plotID","year"), all.x=T) # add sampledArea_m2 to records where it is missing

    transitions <- tidyr::pivot_wider(input_to_transitions,
                                      id_cols = c("domainID", "siteID", "plotID", "individualID", "taxonID", "sampledArea_m2"),
                                      names_from = "year",
                                      names_prefix = "status_",
                                      values_from = "simplePlantStatus",
                                      values_fn = list
                                      )

    # identify expected status_YYYY names
    status_min_year <- paste0("status_", year1)
    status_max_year <- paste0("status_", year2)

    # Add fields if they don't exist
    if (!(status_min_year %in% names(transitions))) {
      transitions[[status_min_year]] <- NA
    }
    if (!(status_max_year %in% names(transitions))) {
      transitions[[status_max_year]] <- NA
    }


    transitions <- as.data.frame(lapply(transitions, as.character)) # if there are >1 status values per group then the status column is a list; this converts to character
    transitions <- as.data.frame(lapply(transitions, function(x) { gsub('NULL', NA, x, fixed=TRUE)  })) # convert character NULL values to NA

    transitions <- transitions %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::contains("status", ignore.case = TRUE),
          .fns = ~ {
            ifelse(is.na(.x), NA, ifelse(grepl("live", .x), "live", "dead")) # if at least one stem is live, classify as live
          }
        )
      )


    #   Identify cases where individual was previously "live" and is currently "dead"
    for (i in 2:length(years_in_input)) {

      column_name_prev <- paste0("status_", years_in_input[i-1])
      column_name <- paste0("status_", years_in_input[i])
      transitionType_column_name <- paste0("transitionType_", years_in_input[i])

      transitions <- transitions %>%
        dplyr::mutate(!!transitionType_column_name := dplyr::case_when(
        ((!!sym(column_name)) == 'dead' | is.na(!!sym(column_name)) ) & !!sym(column_name_prev) == 'live' ~ 'mortality',
        (!!sym(column_name)) == 'live' & is.na(!!sym(column_name_prev) ) ~ 'recruitment',
        ))
     transitions$t2Missing <- ifelse(is.na(transitions[[column_name]]) & !is.na(transitions[[transitionType_column_name]]), "missing", NA)
     mortality <- transitions %>% dplyr::filter(transitions[[transitionType_column_name]] == "mortality")
    }


    #   Associate biomass data in 'vst_agb_kg' with mortality transition data
    mortality <- merge(vst_agb_kg,
                       mortality %>% dplyr::select(-"sampledArea_m2", -"domainID"),
                       by = c("plotID", "siteID", "individualID", "taxonID"),
                       all.y = TRUE)

    ### if specified in mortalityMissin arg, ID individual(s) with status missing in time 2, filter them from mortality df, and filter same individualID(s) from vst_agb_kg
    if(mortalityMissing == "filterMissing"){
    missing <- mortality %>% dplyr::filter(.data$t2Missing == "missing") %>% dplyr::select(-"t2Missing") %>%
      dplyr::mutate(samplingInterval = samplingInterval, diameterIncrement = NA, diameterIncrementFlag = NA)
    missingIDlist <- unique(missing$individualID)
    mortality <- mortality %>% dplyr::filter(is.na(.data$t2Missing))
    missing_totalCount <- missing %>%
      dplyr::group_by(.data$plotID, .data$year) %>%
      dplyr::summarise(filteredCount = dplyr::n())
    vst_agb_kg <- vst_agb_kg %>% dplyr::filter(!.data$individualID %in% missingIDlist) # important: removes missing individualIDs from increment calculations
    }

    if(nrow(mortality) > 0) {
    mortality$agb_Mgha <- ifelse(is.na(mortality$agb_Mgha), 0, mortality$agb_Mgha) # placeholders for year 2 (only need mass from year 1)
    mortality$mortality_Mgha <- NA

    #   If transitionType for a given year is "mortality" then assign a mortality value based on the biomass at the PREVIOUS year
    for (i in 2:length(years_in_input)) {

      year_previous <- years_in_input[i-1]
      column_name <- paste0("transitionType_", years_in_input[i])

      mortality <- mortality %>%
        dplyr::mutate(mortality_Mgha = dplyr::case_when(
          (!!sym(column_name)) == 'mortality' & year == year_previous ~ .data$agb_Mgha,
          TRUE ~ .data$mortality_Mgha
        ))

    }
     mortality$year1 <- as.numeric(mortality$year)
     mortality$year2 <- as.numeric(mortality$year + samplingInterval)

    mortality$year <- as.numeric(mortality$year + samplingInterval) # for plot_mortality assign the live mass from year 1 as mortality mass in year 2

    plot_mortality <- mortality %>%
      dplyr::group_by(.data$siteID,
                      .data$plotID,
                      .data$year) %>%
      dplyr::summarise(mortality_Mgha = sum(.data$mortality_Mgha, na.rm = TRUE),
                       mortalityCount = dplyr::n())
   plot_mortality$mortality_Mghayr <- round(plot_mortality$mortality_Mgha /samplingInterval, digits = 3)

    mortality <- mortality %>% dplyr::filter(!is.na(.data$mortality_Mgha) )
  plot_mortality <- plot_mortality %>% dplyr::filter(.data$year == year2)

  # after creating plot-level summaries, format individual-level mortality table
     mortality$year <- NULL
     mortality <- mortality %>%
       dplyr::filter(!is.na(.data$mortality_Mgha)) %>%
       dplyr::rename("eventID1" = "eventID", "biomassWhenLive_kg" = "agb_kg") %>%
       dplyr::mutate(eventID2 = paste0("vst_",.data$siteID,"_",year2)) %>%
       dplyr::select(-"plot_eventID", -"agb_Mgha", -"simplePlantStatus") %>%
       dplyr::relocate("plotID", .after = "siteID") %>%
       dplyr::relocate("eventID1", .after = "plotID") %>%
       dplyr::relocate("eventID2", .after = "eventID1") %>%
       dplyr::relocate("year1", .after = "eventID2") %>%
       dplyr::relocate("year2", .after = "year1")   }

 }


  ####  CALCULATE RECRUITMENT

  # identify transitions that represent recruitment

  transition_simple <- transitions %>% dplyr::select(-"domainID",-"siteID", -"plotID", -"taxonID", -"sampledArea_m2", -"t2Missing")
  recruitment <- transition_simple  %>% dplyr::left_join(vst_agb_kg, by = c("individualID"))
  recruitment_input <- transitions  %>%  dplyr::select("plotID", "individualID", "sampledArea_m2", dplyr::starts_with("transitionType_")) %>%
                        tidyr::pivot_longer(cols = !c("plotID", "individualID", "sampledArea_m2"), names_to = "year", names_prefix = "transitionType_", values_to = "transition_type")
  recruitment_input <-  recruitment_input %>% dplyr::filter(.data$transition_type == "recruitment") %>% dplyr::select("individualID", "year") %>%
      dplyr::mutate(year = as.numeric(.data$year))
  recruitment_ind <- unique(recruitment_input$individualID)

  recruitment <- recruitment %>% dplyr::filter(.data$individualID %in% recruitment_ind) %>%
                       dplyr::mutate(samplingInterval = samplingInterval,
                                     diameterIncrement = (.data$stemDiameter - 9.9)/samplingInterval,
                                     diameterIncrementFlag = ifelse(.data$diameterIncrement >= 3, "flagged", NA))

  recruitmentPlots <- recruitment %>% dplyr::select("domainID","individualID","plotID","sampledArea_m2") %>% unique()
  recruitment_input <- recruitment_input %>% dplyr::left_join(recruitmentPlots, by = c("individualID"))

  if(stemIncrementFlagged == "filterFlagged"){
  recruitmentFlags <- recruitment %>% dplyr::filter(.data$diameterIncrementFlag == "flagged")
    recruitmentFlagsIDlist <- unique(recruitmentFlags$individualID)
  recruitment <- recruitment %>% dplyr::filter(.data$diameterIncrementFlag != "flagged" | is.na(.data$diameterIncrementFlag))
  recruitment_input <- recruitment_input %>% dplyr::filter(!.data$individualID %in% recruitmentFlagsIDlist )
  recruitmentFlags_totalCount <- recruitmentFlags %>%
      dplyr::group_by(.data$plotID, .data$year) %>%
      dplyr::summarise(filteredCount = dplyr::n())
  vst_agb_kg <- vst_agb_kg %>% dplyr::filter(!.data$individualID %in% recruitmentFlagsIDlist ) # important: removes individualIDs with implausible diameter increment from increment calculations
 }

  if(nrow(recruitment_input) >0 ) {
  # produce dataframe with structure required to be passed successfully as vst_apparentindividual to estimateWoodMass function
  recruitment_input$stemDiameter <- 10
  recruitment_input$basalStemDiameter <- recruitment_input$height <- recruitment_input$measurementHeight <- recruitment_input$basalStemDiameterMsrmntHeight <-
           recruitment_input$maxCrownDiameter <- recruitment_input$ninetyCrownDiameter <- NA
  recruitment_input$plantStatus <- "Live" # we are only looking at individuals that were live in most recent year so this is appropriate
  recruitment_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here
  recruitment_input$eventID <- paste0("vst_", substr(recruitment_input$individualID, 14, 17), "_", recruitment_input$year) # recreate eventID
  recruitment_input$siteID <- substr(recruitment_input$individualID, 14, 17)
  recruitment_input$year <- NULL
  # if recruitment were to be extended to other growthForms the following line would NOT be appropriate
  recruitment_input$growthForm <- "multi-bole tree" # required in order to call estimateWoodMass, which doesn't distinguish between single and multi bole trees
  recruitment_input$uid <- recruitment_input$namedLocation <- recruitment_input$dendrometerInstallationDate <- recruitment_input$initialGapMeasurementDate <- recruitment_input$initialBandStemDiameter <- recruitment_input$initialDendrometerGap <- 
    recruitment_input$dendrometerHeight <- recruitment_input$dendrometerGap <- recruitment_input$dendrometerCondition <- recruitment_input$bandStemDiameter <- recruitment_input$publicationDate <- recruitment_input$measuredBy <- 
    recruitment_input$recordedBy <- recruitment_input$dataEntryRecordID <- recruitment_input$release <- recruitment_input$dataQF <- recruitment_input$subplotID <- NA # placeholders, estimateWoodMass function removes them
  
  # produce dataframe with structure required to be passed successfully as vst_perplotperyear to estimateWoodMass function
  perplot_input <- vst_plot_Mgha
  perplot_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here
  perplot_input$samplingImpractical <- "OK"
  perplot_input$year <- NULL
  # if recruitment were to be extended to other growthForms the following two lines would NOT be appropriate
  perplot_input$totalSampledAreaShrubSapling <- perplot_input$totalSampledAreaLiana <- perplot_input$totalSampledAreaFerns <- perplot_input$totalSampledAreaOther <- NA
  perplot_input$totalSampledAreaTrees <- NA #perplot_input$sampledArea_m2 # we already know sampledArea_m2, but this is workaround to allow estimateWoodMass to recalculate it
  
  # bind required dataframes together for input to estimateWoodMass function
  recruitment_list <- list(vst_apparentindividual = recruitment_input,
                      vst_mappingandtagging = map_input,
                      vst_perplotperyear = perplot_input) # , 'vst_non-woody' = vst_non_woody )

  # call estimateWoodMass function to estimate species-specific mass of recruiting individual within minimum diameter of 10 cm
  recruitment_output <- estimateWoodMass(inputDataList = recruitment_list,
                            plotSubset = plotSubset,
                            growthFormSubset = "tree")

  # add taxonID
  taxon_per_ID <- recruitment_output$vst_agb_kg %>% dplyr::select("individualID", "taxonID")
  recruitment_input_w_taxonID <- merge(recruitment_input, taxon_per_ID, by = "individualID")
  recruitment_input_w_taxonID$year <- substr(recruitment_input_w_taxonID$eventID,10,13) # add year back 

  # summarize number of stems per taxonID for each plot and year
  recruitment_count <-  recruitment_input_w_taxonID %>%
      dplyr::group_by(.data$plotID,
                      .data$year,
                      .data$sampledArea_m2,
                      .data$taxonID) %>%
      dplyr::summarise(recruitment_count = dplyr::n(), .groups = "drop")

  # simplify table with biomass of 10 cm diameter individual for each taxonID
  taxon_biomass <- recruitment_output$vst_agb_kg %>%
      dplyr::select("taxonID", "agb_kg") %>%
      dplyr::distinct(.data$taxonID, .keep_all = TRUE)

  # link biomass to each taxonID
  recruitmentMass <- merge(recruitment_count, taxon_biomass, by = "taxonID")

  # multiply number of recruitment stems by taxon-specific biomass and then convert mass from kg to Mg/ha
  recruitmentMass$sampledArea_m2 <- as.numeric(recruitmentMass$sampledArea_m2)
  recruitmentMass$recruitment_Mgha <-  recruitmentMass$recruitment_count * recruitmentMass$agb_kg *  0.001 * (10000/recruitmentMass$sampledArea_m2)
  recruitmentMass$sampledArea_m2 <- recruitmentMass$recruitment_count <- recruitmentMass$agb_kg <- NULL

  # multiply number of recruitment stems by taxon-specific biomass and then convert mass from kg to Mg/ha
  plot_recruitment <- recruitmentMass %>%
    dplyr::group_by(.data$plotID, .data$year) %>%
    dplyr::summarize(recruitment_Mgha = sum(.data$recruitment_Mgha, na.rm = T)) %>%
    dplyr::ungroup()
  plot_recruitment$recruitment_Mghayr <- round(plot_recruitment$recruitment_Mgha /samplingInterval, digits = 3)

   recruitment_input$year <- substr(recruitment_input$eventID,10,13) # add year back 
    recruitment_totalCount <-  recruitment_input %>%
      dplyr::group_by(.data$plotID,
                      .data$year) %>%
      dplyr::summarise(recruitmentCount = dplyr::n(), .groups = "drop")
  plot_recruitment <- merge(plot_recruitment, recruitment_totalCount, by = c("plotID", "year"), all.x=TRUE)

    } else {
  plot_recruitment <- vst_plot_Mgha %>% dplyr::select("plotID", "year")
  plot_recruitment$recruitment_Mgha <- plot_recruitment$recruitment_Mghayr <- 0
  plot_recruitment$recruitmentCount <- 0
    }



  ### CALCULATE BIOMASS INCREMENT (Clark et al. 2001 approach 2 - stand level productivity calculation) ####

  if(stemIncrementFlagged == "filterFlagged"){
  diameter_inc <- vst_agb_kg %>% dplyr::filter(.data$simplePlantStatus == "live") %>%
      dplyr::select("plot_eventID",
                    "domainID",
                    "siteID",
                    "plotID",
                    "individualID",
                    "stemDiameter",
                    "year")

    #   Retain records unique with respect to individualID, taxonID, year, simplePlantStatus; don't need to worry about multi-bole smaller individuals because 'estimateWoodMass()' function combines mass for boles in output.
    diameter_inc <- diameter_inc %>%
      dplyr::distinct(.data$individualID,
                      .data$year,
                      .keep_all = TRUE)

    diameter_inc <- diameter_inc[order(diameter_inc$year),]
    diameter_inc$yearRel <- as.numeric(factor(diameter_inc$year)) # convert true year to relative year (1 or 2)

    diameter_wide <- tidyr::pivot_wider(diameter_inc,
                                      id_cols = c("domainID", "siteID", "plotID", "individualID"),
                                      names_from = "yearRel",
                                      names_prefix = "stemDiameter_",
                                      values_from = "stemDiameter")

    # Add fields if they don't exist
    if (!("stemDiameter_1" %in% names(diameter_wide))) {
      diameter_wide$stemDiameter_1 <- NA
    }
    if (!("stemDiameter_2" %in% names(diameter_wide))) {
      diameter_wide$stemDiameter_2 <- NA
    }

    diameter_wide$samplingInterval <- samplingInterval
    diameter_wide$diameterIncrement <- abs(as.numeric(diameter_wide$stemDiameter_2) - as.numeric(diameter_wide$stemDiameter_1))/samplingInterval
    diameter_wide$diameterIncrementFlag <- ifelse(diameter_wide$diameterIncrement > 3, "flagged", NA)

    incrementFlags <- diameter_wide %>% dplyr::filter(.data$diameterIncrementFlag == "flagged")

    if(nrow(incrementFlags) >0){
    incrementFlagsSimple <- incrementFlags %>% dplyr::select("individualID", "samplingInterval", "diameterIncrement", "diameterIncrementFlag")

    incrementFlaglist <- unique(incrementFlags$individualID)

    stemIncrementFlags <- vst_agb_kg %>% dplyr::filter(.data$individualID %in% incrementFlaglist )
    stemIncrementFlags <- merge(stemIncrementFlags, transition_simple, by = "individualID", all.x =TRUE)
    stemIncrementFlags <- merge(stemIncrementFlags, incrementFlagsSimple, by = "individualID", all.x =TRUE)
     vst_agb_kg <- vst_agb_kg %>% dplyr::filter(!.data$individualID %in% incrementFlaglist ) # important: removes missing individualIDs from increment calculations
    }
  }

  ############ Scale biomass per area and convert to Mg / ha ######################
  #   Remove records that cannot be scaled to a per area basis
  vst_agb_Mgha <- vst_agb_kg %>%
    dplyr::filter(!is.na(.data$sampledArea_m2) & .data$sampledArea_m2 > 0 & !is.na(.data$agb_kg))

  #   Create "Mg/ha" biomass estimate for each record; used in downstream plot- and site-level biomass estimation
  vst_agb_Mgha$agb_Mgha <- round(vst_agb_Mgha$agb_kg * 0.001 * (10000/vst_agb_Mgha$sampledArea_m2),
                                  digits = 4)


  ##  Create list of plot x eventIDs from vst_apparentindividaul data; later diff against what is reported in perplot data
  agb_ind_eventID_list <- unique(vst_agb_Mgha$plot_eventID)


  ##  Identify plot by eventID combos that don't have biomass values
  vst_agb_zeros <- base::setdiff(plot_eventID_list, agb_ind_eventID_list)

  vst_agb_zeros <- as.data.frame(vst_agb_zeros)

  vst_agb_zeros <- dplyr::rename(vst_agb_zeros,
                                 plot_eventID = vst_agb_zeros)

  vst_agb_zeros$plotID <- substr(vst_agb_zeros$plot_eventID, 1, 8)

  vst_agb_zeros$siteID <- substr(vst_agb_zeros$plot_eventID, 1, 4)

  vst_agb_zeros$year <- as.numeric(substr(vst_agb_zeros$plot_eventID, 19, 22))

  vst_agb_zeros$eventID <- substr(vst_agb_zeros$plot_eventID, 10, 22)

  vst_agb_zeros <- merge(vst_agb_zeros,
                         plotType_df,
                         by = "plotID",
                         all.x = TRUE)



  ### Generate plot-level biomass summary ####

  #   Sum biomass per unit area for each plot x year x simplePlantStatus x nlcdClass x taxonID combo (aggegate across individualIDs)
  vst_plot_summary <- vst_agb_Mgha %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$siteID,
                    .data$eventID,
                    .data$plotID,
                    .data$sampledArea_m2,
                    .data$eventType,
                    .data$plotType,
                    .data$nlcdClass,
                    .data$taxonID,
                    .data$growthForm,
                    .data$simplePlantStatus,
                    .data$year) %>%
    dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE),
                     .groups = "drop")

  #   Within a given year, transpose live and dead AGB into separate columns
  vst_plot_wide <- tidyr::pivot_wider(vst_plot_summary,
                                      id_cols = c("siteID", "plot_eventID", "eventID", "plotID",  "sampledArea_m2", "eventType",
                                                  "plotType", "nlcdClass", "taxonID", "growthForm", "year"),
                                      names_from = "simplePlantStatus",
                                      names_glue = "{simplePlantStatus}_Mgha",
                                      values_from = "agb_Mgha")
  if (!"dead_Mgha" %in% names(vst_plot_wide)) {
    vst_plot_wide$dead_Mgha <- NA
  }


  #   Assumption: Replace NAs created during transpose with zeroes; assume both live and dead were sampled in a plot

  vst_plot_wide$dead_Mgha[is.na(vst_plot_wide$dead_Mgha)] <- 0
  vst_plot_wide$live_Mgha[is.na(vst_plot_wide$live_Mgha)] <- 0


  #   Assign zero AGB values to plots with zero biomass
  vst_agb_zeros_plot <- vst_agb_zeros

  if (nrow(vst_agb_zeros_plot) > 0) {
    perplot_meta_for_missing <- unique(vst_plot_Mgha %>% dplyr::select("plotID", "eventID", "eventType", "nlcdClass"))
    vst_agb_zeros_plot <- merge(vst_agb_zeros_plot, perplot_meta_for_missing, by = c("plotID", "eventID"), all.x = T)
    vst_agb_zeros_plot$taxonID <-  vst_agb_zeros_plot$growthForm <- vst_agb_zeros_plot$sampledArea_m2 <- NA # placeholders to allow rbind without errors
    vst_agb_zeros_plot$dead_Mgha <- vst_agb_zeros_plot$live_Mgha <- 0

  #   Add rows for plots with zero biomass to plots with AGB
  vst_plot_Mgha <- rbind(vst_plot_wide,
                         vst_agb_zeros_plot)
  } else {
     vst_plot_Mgha <- vst_plot_wide
  }

  priority_plots <- priority_plots # force load from data table
  priority_plots_add <- priority_plots %>%
  dplyr::select("plotID",
                  "specificModuleSamplingPriority")

  #   Add 'specificModuleSamplingPriority' column to output
  vst_plot_Mgha <- merge(vst_plot_Mgha,
                         priority_plots_add,
                         by = c("plotID"),
                         all.x = TRUE)

  #   Retain AGB estimates for records with values for both live and dead biomass
  vst_plot_Mgha <- vst_plot_Mgha %>%
    dplyr::filter(!is.na(.data$live_Mgha) & !is.na(.data$dead_Mgha))


  #   Some taxonIDs are represented in multiple growthForms (e.g., sapling and single bole tree): This sums the growthForms
  vst_agb_Live <- vst_plot_Mgha %>%
    dplyr::group_by(.data$siteID,
                    .data$plotID,
                    .data$plotType,
                    .data$specificModuleSamplingPriority,
                    .data$eventID,
                    .data$year,
                    .data$plot_eventID
#                    , .data$nlcdClass
#                    , .data$taxonID
                    ) %>%
    dplyr::summarise(Mgha_live = sum(.data$live_Mgha, na.rm = TRUE),
#                     dead_Mgha = sum(.data$dead_Mgha, na.rm = TRUE),
                     .groups = "drop")

  vst_agb_Live <- vst_agb_Live[order(vst_agb_Live$year),]


  #   Convert 'vst_agb_Live' from long to wide format (all years in same row)
  vst_increment <- tidyr::pivot_wider(vst_agb_Live,
                                      id_cols = c("siteID", "plotID", "plotType"), # , "nlcdClass" , "taxonID"),
                                      names_from = "year",
                                      names_prefix = "Mgha_",
                                      values_from = "Mgha_live")

  #   Calculate plot-level increment
  for (i in 2:length(years_in_input)) {
    column_name_prev <- paste0("Mgha_", years_in_input[i - 1])
    column_name <- paste0("Mgha_", years_in_input[i])
    increment_column_name <- paste0("Mgha_increment_", years_in_input[i])
    vst_increment <- vst_increment %>%
      dplyr::mutate(!!increment_column_name := (!!sym(column_name)) - !!sym(column_name_prev))

  } # end 'for' loop


  vst_increment_long <- vst_increment %>%
    #   Remove individual 'year' columns
    dplyr::select(-dplyr::contains("Mgha_2"))  %>%

    #   Return to long format to obtain 'cols' below and increment by taxonID by year in each row
    tidyr::pivot_longer(cols = !c("plotID", "siteID","plotType"), #  "taxonID", "nlcdClass"
                        names_to = "year",
                        names_prefix = "Mgha_increment_",
                        values_to = "increment_Mgha")
 vst_increment_long$increment_Mghayr <- round(vst_increment_long$increment_Mgha /samplingInterval, digits = 3)

 vst_ANPP_plot <- vst_increment_long


  ### combine missing, recruitmentFlags, and stemIncrementFlags tables if they exist

  filtered_df_names <- c("missing", "recruitmentFlags", "stemIncrementFlags")

  # remove df if it exists and has 0 rows
  for (df_name in filtered_df_names) {
    if (exists(df_name) && is.data.frame(get(df_name)) && nrow(get(df_name)) == 0) {
      rm(list = df_name)
    }
  }

  # Filter to only those dataframe(s) that exist and are data frames
  existing_dfs <- lapply(filtered_df_names, function(name) {
    if (exists(name) && is.data.frame(get(name))) get(name) else NULL
  })

  # Remove NULLs
  existing_dfs <- Filter(Negate(is.null), existing_dfs)


  # Remove NULLs
  existing_dfs <- Filter(Negate(is.null), existing_dfs)

  # Bind rows if any data frames exist
  if (length(existing_dfs) > 0) {
    filtered <- dplyr::bind_rows(existing_dfs)
    filtered_totalCount <- filtered %>%
      dplyr::group_by(.data$plotID, .data$year) %>%
      dplyr::summarise(filteredCount = dplyr::n())
  } else {
  filtered <- data.frame()
  }

  ##############################################################################################
  #   Gather mortality, recruitment, and increment data into same dataframe
  if(nrow(plot_mortality) > 0){
    vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  plot_mortality,
                                  by = c("siteID", "plotID", "year"),
                                  all.x = TRUE)
  }
   if (!"mortalityCount" %in% names(vst_ANPP_plot)) {
  vst_ANPP_plot$mortalityCount <- 0
  vst_ANPP_plot$mortality_Mghayr <- 0
  } else {
  vst_ANPP_plot$mortalityCount <- ifelse(is.na(vst_ANPP_plot$mortalityCount), 0, vst_ANPP_plot$mortalityCount)
  vst_ANPP_plot$mortality_Mghayr <- ifelse(is.na(vst_ANPP_plot$mortality_Mghayr), 0, vst_ANPP_plot$mortality_Mghayr)
  }

  if(nrow(plot_recruitment) > 0){
  #   Add recruitment data to same dataframe
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  plot_recruitment,
                                  by = c("plotID", "year"),
                                  all.x = TRUE)
  }
   if (!"recruitmentCount" %in% names(vst_ANPP_plot)) {
  vst_ANPP_plot$recruitmentCount <- 0
  vst_ANPP_plot$recruitment_Mghayr <- 0
  } else {
  vst_ANPP_plot$recruitmentCount <- ifelse(is.na(vst_ANPP_plot$recruitmentCount), 0, vst_ANPP_plot$recruitmentCount)
  vst_ANPP_plot$recruitment_Mghayr <- ifelse(is.na(vst_ANPP_plot$recruitment_Mghayr), 0, vst_ANPP_plot$recruitment_Mghayr)
  }

  # Add filtered individual count, if applicable
  if(nrow(filtered) > 0){
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  filtered_totalCount,
                                  by = c("plotID", "year"),
                                  all.x = TRUE)
  }
  if (!"filteredCount" %in% names(vst_ANPP_plot)) {
  vst_ANPP_plot$filteredCount <- 0
  } else {
  vst_ANPP_plot$filteredCount <- ifelse(is.na(vst_ANPP_plot$filteredCount), 0, vst_ANPP_plot$filteredCount)
  }
  
  vst_ANPP_plot$samplingInterval <- samplingInterval
  vst_ANPP_plot <- vst_ANPP_plot %>%
       dplyr::relocate("samplingInterval", .after = "plotType") %>%
       dplyr::relocate("filteredCount", .after = "samplingInterval") %>%
       dplyr::relocate("mortalityCount", .after = "filteredCount") %>%
       dplyr::relocate("recruitmentCount", .after = "mortalityCount")


  #   Set "year" data type
  vst_ANPP_plot$year <- as.numeric(vst_ANPP_plot$year)

  #   Remove records with increment == NA; very important because NAs for both increment and mortality lead to false zeroes during subsequent group_by() steps.
  vst_ANPP_plot <- vst_ANPP_plot %>%
    dplyr::filter(!is.na(.data$increment_Mghayr))





  #   Sum increment plus mortality and subtract recruitment to get woody ANPP by plotID x year
  vst_ANPP_plot$woodANPP_Mghayr <- round(vst_ANPP_plot$increment_Mghayr + vst_ANPP_plot$mortality_Mghayr - vst_ANPP_plot$recruitment_Mghayr,
                                             digits = 3)

  # remove fields not adjusted forsamplingInterval
  vst_ANPP_plot$increment_Mgha <- vst_ANPP_plot$mortality_Mgha <- vst_ANPP_plot$recruitment_Mgha <- NULL


  priority_plots_add <- unique(priority_plots_add)

  #   Add plot prioritization number to per plot ANPP estimate dataframe
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                           priority_plots_add,
                           by = c("plotID"),
                           all.x = TRUE)

  #   Filter plot output with user-provided plotSubset argument
  if(plotType == "tower") {

    vst_ANPP_plot <- vst_ANPP_plot %>%
      dplyr::filter(.data$plotType == "tower")

  }

  vst_ANPP_plot <- vst_ANPP_plot %>%
    dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)

  #   Add metadata to contextualize ANPP estimates
  vst_NPP_plot_yearFirst <- vst_ANPP_plot %>%
    dplyr::group_by(.data$siteID,
                    .data$plotID,
                    .data$plotType) %>%
    dplyr::summarise(wood_N = dplyr::n(),
                     woodANPP_Mghayr_sd = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE),
                                                digits = 2),
                     woodANPP_Mghayr_se = round((.data$woodANPP_Mghayr_sd / sqrt(.data$wood_N)),
                                                digits = 2),
                     woodANPP_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),
                                             digits = 4),
                     .groups = "drop") %>%
    dplyr::mutate(wood_count_type = "years")

  #   Estimate ANPP at site level using plot data
  vst_ANPP_site <- vst_ANPP_plot %>%
    dplyr::group_by(.data$siteID, .data$year, .data$samplingInterval) %>%
    dplyr::summarise(woodPlotNum = dplyr::n(),
                     woodANPPSD_Mghayr = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE),
                                               digits = 2),
                     woodANPPMean_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),
                                                 digits = 4),
                     .groups = "drop")

    message(glue::glue("Returning productivity summary data frames for: {paste(siteID, collapse = ', ')}"))


    output.list <- list(
      vst_ANPP_plot = vst_ANPP_plot,
      vst_ANPP_site = vst_ANPP_site,
      filtered = filtered,
      mortality = mortality,
      recruitment = recruitment
    )


  return(output.list)

}



=======
##############################################################################################
#' @title Estimate ANPP (Aboveground Net Primary Productivity) contributed by woody vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Calculate annual productivity of woody vegetation.
#'
#' Data inputs are "Vegetation structure" data (DP1.10098.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' Data should be from just one site, and exactly two temporal eventIDs.
#'
#' @details The input data is passed to the companion estimateWoodMass() function to create biomass summaries, and then aboveground productivity is calculated for woody vegetation.
#'
#' The stand-level approach to calculating productivity (approach 2) is used from Clark DA, S Brown, DW Kicklighter, JQ Chambers, JR Thomlinson, and J Ni. 2001. Measuring Net Primary Production in Forests: Concepts and Field Methods. Ecological Applications 11:356-370.
#'
#' Woody productivity is only calculated for trees with growthForm of "single bole tree" or "multi-bole tree".
#'
#' NEON has an extensive data QA/QC process, but users should be aware that these productivity estimates are very sensitive to any residual errors and so the data should be examined carefully
#'
#' @details Input data can be filtered by 'plotSubset' if output for only certain types of plots or sampling intervals is desired. Input data are combined with allometric equation parameters and taxon specific characteristics, and biomass is estimated for each individual using allometric equations. Generalized allometric equations are applied first and are replaced by taxon-specific equations if available. Only the set of growth forms selected via the growthForm parameter are included in outputs. The non-woody "cactus" and "ferns" growthForms are not currently included. Biomass is summarized on an areal basis at the hierarchical level of the plot and site.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments. [list]
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
#' @param siteID Specify the four-letter siteID for a single NEON site (only one siteID is allowed). [character]
#'
#' @param plotSubset The available options for this function are the default of "towerAnnualSubset" (only the subset of tower plots that are sampled annually) or "towerAll" (all plots in the tower airshed but no distributed plots). [character]
#'#'
#' @param mortalityMissing Select how missing individuals are handled. Default is "filterMissing" that removes individualIDs with plantStatus NA at time 2, as well as filtering to remove from the vst_agb_kg table that is summarized to produce plot-level increment, with the individuals with plantStatus NA at time sent to "filtered" table. Alternate option is "retainMissing", which retains individuals with plantStatus NA at time 2 and assumes that they are dead and contributing to "mortality" table and increment. [character]
#'
#' @param stemIncrementFlagged Select how individuals implausibly large stem diameter increments from time 1 to time 2 are handled. Default is "filterFlagged" that removes individuals that are NA at time 1 but >= 3 cm stemDiameter increment (e.g., 9.9 + 3 = 12.9 for samplingInterval of 1 yr) at time 2, or that have an absolute stem diameter increment >= 3 from one live bout to a second live bout, with individuals sent to "filtered" table. Alternate option is "retainFlagged", which retains individuals and includes them in recruitment and vst_agb_kg tables regardless of stem diameter increment. [character]
#'
#' @return A list that includes productivity summary data frames. Output tables include:
#'   * vst_ANPP_plot - Summarizes woody ANPP for each plot x year combination ("Mg/ha/yr").
#'   * vst_ANPP_site - Summarizes woody ANPP for each site x year combination ("Mg/ha/yr").
#'
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Vegetation structure
#' VstDat <- neonUtilities::loadByProduct(
#' dpID="DP1.10098.001",
#' package = "basic",
#' check.size = FALSE
#' )
#'
#' estimateWoodProdOutputs <- estimateWoodProd(inputDataList = VstDat, siteID = "LENO")
#'
#'
#' }
#'
#' @export estimateWoodProd

estimateWoodProd = function(inputDataList,
                            inputIndividual = NA,
                            inputMapTag = NA,
                            inputNonWoody = NA,
                            inputPerPlot = NA,
                            siteID,
                            plotSubset = "towerAnnualSubset",
                            mortalityMissing = "filterMissing",
                            stemIncrementFlagged = "filterFlagged") {

  options(dplyr.summarise.inform = FALSE)

  ### Check that input arguments meet assumptions ####

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("towerAll", "towerAnnualSubset")) {
    stop("The only valid plotSubset options are 'towerAll', 'towerAnnualSubset'.")
  }

  plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50) # convert to numeric (50 is highest plotPriority)
  plotType <- "tower"

  # Error if invalid mortalityMissing option selected
  if (!mortalityMissing %in% c("filterMissing", "retainMissing")) {
    stop("The only valid mortalityMissing options are 'filterMissing', 'retainMissing'.")
  }

    # Error if invalid stemIncrementFlagged option selected
  if (!stemIncrementFlagged %in% c("filterFlagged", "retainFlagged")) {
    stop("The only valid stemIncrementFlagged options are 'filterFlagged', 'retainFlagged'.")
  }

  estimateWoodMassOutputs <- estimateWoodMass(
    inputDataList = inputDataList,
    plotSubset = plotSubset,
    growthFormSubset = "tree"
   )

#  vst_plot_Mgha <- estimateWoodMassOutputs$vst_plot_w_0s
  vst_plot_Mgha <- estimateWoodMassOutputs$vst_plot_Mgha
  vst_agb_kg <- estimateWoodMassOutputs$vst_agb_kg
  vst_missing <- estimateWoodMassOutputs$vst_missing
  
    liveList <- c("Live",
                "Live, insect damaged",
                "Live, disease damaged",
                "Live, physically damaged",
                "Live, other damage",
                "Live, broken bole",
                "No longer qualifies")

    vst_missing <- vst_missing %>%
      dplyr::mutate(simplePlantStatus = dplyr::case_when(.data$plantStatus %in% liveList ~ "live",
                                                         TRUE ~ "dead"))
    
  vst_agb_kg <- dplyr::bind_rows(vst_agb_kg, vst_missing)

  # used later when calling estimateWoodMass for recruitment
  map_input <- vst_agb_kg
  map_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here


  # filter by eventType based on plotSubset argument
  if(plotSubset == "towerAll") {
    message(glue::glue("Since plotSubset 'towerAll' was selected, input data has been filtered to just those plots in the tower airshed."))
      vst_plot_Mgha <- vst_plot_Mgha %>%
        dplyr::filter(.data$plotType == "tower")

      vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_Mgha$plot_eventID)
    }

  if(plotSubset == "towerAnnualSubset") {
    message(glue::glue("Since plotSubset 'towerAnnualSubset' was selected, input data has been filtered to just those sampling bouts when all tower plots were sampled."))
      vst_plot_Mgha <- vst_plot_Mgha %>%
        dplyr::filter(grepl("owerSubset", .data$eventType))

      vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_Mgha$plot_eventID)
    }

    ### Error if not a single site of data after filtering to the siteID in the siteID argument

    vst_plot_Mgha <- vst_plot_Mgha[vst_plot_Mgha$siteID == siteID, ]


    vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_Mgha$plot_eventID)

    sites_in_input <- unique(vst_plot_Mgha$siteID)

    if (length(sites_in_input) >1) {
    stop(glue::glue("Only one siteID is allowed in filtered dataset. Current filtered dataset has data from: {unique(vst_plot_Mgha$siteID)}"))
    }

    if(length(sites_in_input) == 0) {
    stop(glue::glue("Filtered dataset has no siteID. Select a different siteID argument and/or a different inputDataList"))
    }


    ### Error if not 2 years of data
  years_in_input <- unique(sort(vst_plot_Mgha$year))
  year1 <- min(as.numeric(years_in_input))
  year2 <- max(as.numeric(years_in_input))

    if (length(years_in_input) < 2) {
    stop(glue::glue("Two years of data are needed to calculate woody productivity. Current filtered dataset only has woody biomass data from: {unique(vst_plot_Mgha$year)}"))
    }

    if (length(years_in_input) > 2) {
     stop(glue::glue("This function expects there to be data from only two eventID years after filtering based on plotSubset. The current filtered dataset has woody biomass data from: {paste(unique(vst_plot_Mgha$eventID), collapse = ', ')}"))
    }

  samplingInterval <- abs(diff(years_in_input))

  # filter to plots with exactly 2 years in vst_plot_Mgha and provide warning if there was < 2 years or > 2 years
  vst_plot_Mgha <- vst_plot_Mgha %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(yr_count = dplyr::n_distinct(.data$year) )

  lt_2_yr <-  vst_plot_Mgha %>% dplyr::filter(.data$yr_count < 2) %>% dplyr::pull("plotID") %>% unique() %>% as.character()
  gt_2_yr <-  vst_plot_Mgha %>% dplyr::filter(.data$yr_count > 2) %>% dplyr::pull("plotID") %>% unique() %>% as.character()
  desired_2_yr <-  vst_plot_Mgha %>% dplyr::filter(.data$yr_count == 2)  %>% dplyr::pull("plotID") %>% unique() %>% as.character()

  vst_plot_Mgha <- vst_plot_Mgha %>%
    dplyr::filter(.data$yr_count == 2)

    if (length(lt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from less than two years and have been removed from vst_plot_Mgha: {paste(unique(lt_2_yr), collapse = ', ')}"))
    }

    if (length(gt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from more than two years and have been removed from vst_plot_Mgha: {paste(unique(gt_2_yr), collapse = ', ')}"))
    }

  # filter to plots with exactly 2 years in vst_agb_kg and provide warning if there was < 2 years or > 2 years
  agb_only_1_yr <-  vst_agb_kg %>% dplyr::filter(!.data$plotID %in% desired_2_yr) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(yr_count = dplyr::n_distinct(.data$year) ) %>%
    dplyr::filter(.data$yr_count < 2) %>%
    dplyr::pull("plotID") %>% unique() %>% as.character()
  agb_gt_2_yr <-  vst_agb_kg %>% dplyr::filter(!.data$plotID %in% desired_2_yr) %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(yr_count = dplyr::n_distinct(.data$year) ) %>%
    dplyr::filter(.data$yr_count > 2) %>%
    dplyr::pull("plotID") %>% unique() %>% as.character()

  vst_agb_kg <- vst_agb_kg %>% dplyr::filter(.data$plotID %in% desired_2_yr)


    if (length(agb_only_1_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from less than two years and have been removed from vst_agb_kg: {paste(unique(agb_only_1_yr), collapse = ', ')}"))
    }

    if (length(agb_gt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from more than two years and have been removed from vst_agb_kg: {paste(unique(agb_gt_2_yr), collapse = ', ')}"))
    }


  ### Identify plotIDs and associated plotType in the dataset
  plotType_df <- unique(vst_plot_Mgha %>% dplyr::select("plotID", "plotType"))

  ##  Identify all plot by eventID combos in the filtered vst_plot_w_os dataframe
  plot_eventID_list <- unique(vst_plot_Mgha$plot_eventID)


  ### CALCULATE MORTALITY

  # Create placeholder 'plot_mortality' dataframe: gets overwritten later if there is any mortality
  plot_mortality <- data.frame(siteID = character(),
                                 plotID = character(),
                                 taxonID = character(),
                                 year = character(),
                                 mortality_Mghayr = numeric(),
                                 mortality_Mghayr = numeric())

  if (nrow(vst_agb_kg) > 0) {

    #   convert kg to Mg/ha
    vst_agb_kg$agb_Mgha <- round(vst_agb_kg$agb_kg * 0.001 * (10000/vst_agb_kg$sampledArea_m2),
                                 digits = 4)


    ##  Categorize individualIDs based on their changes (or not) in simplePlantStatus
    input_to_transitions <- vst_agb_kg %>%
      dplyr::select("plot_eventID",
                    "domainID",
                    "siteID",
                    "plotID",
                    "sampledArea_m2",
                    "individualID",
                    "taxonID",
                    "simplePlantStatus",
                    "year")

    #   Retain records unique with respect to individualID, taxonID, year, simplePlantStatus; don't need to worry about multi-bole smaller individuals because 'estimateWoodMass()' function combines mass for boles in output.
    input_to_transitions <- input_to_transitions %>%
      dplyr::distinct(.data$individualID,
                      .data$taxonID,
                      .data$year,
                      .data$simplePlantStatus,
                      .keep_all = TRUE)

    input_to_transitions <- input_to_transitions[order(input_to_transitions$year),]


    area_lookup <- input_to_transitions %>% dplyr::select("plotID","year","sampledArea_m2") %>% dplyr::filter(!is.na(.data$sampledArea_m2)) %>% unique()
    input_to_transitions <- input_to_transitions %>% dplyr::select(-"sampledArea_m2")
    input_to_transitions <- merge(input_to_transitions, area_lookup, by = c("plotID","year"), all.x=T) # add sampledArea_m2 to records where it is missing

    transitions <- tidyr::pivot_wider(input_to_transitions,
                                      id_cols = c("domainID", "siteID", "plotID", "individualID", "taxonID", "sampledArea_m2"),
                                      names_from = "year",
                                      names_prefix = "status_",
                                      values_from = "simplePlantStatus",
                                      values_fn = list
                                      )

    # identify expected status_YYYY names
    status_min_year <- paste0("status_", year1)
    status_max_year <- paste0("status_", year2)

    # Add fields if they don't exist
    if (!(status_min_year %in% names(transitions))) {
      transitions[[status_min_year]] <- NA
    }
    if (!(status_max_year %in% names(transitions))) {
      transitions[[status_max_year]] <- NA
    }


    transitions <- as.data.frame(lapply(transitions, as.character)) # if there are >1 status values per group then the status column is a list; this converts to character
    transitions <- as.data.frame(lapply(transitions, function(x) { gsub('NULL', NA, x, fixed=TRUE)  })) # convert character NULL values to NA

    transitions <- transitions %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::contains("status", ignore.case = TRUE),
          .fns = ~ {
            ifelse(is.na(.x), NA, ifelse(grepl("live", .x), "live", "dead")) # if at least one stem is live, classify as live
          }
        )
      )


    #   Identify cases where individual was previously "live" and is currently "dead"
    for (i in 2:length(years_in_input)) {

      column_name_prev <- paste0("status_", years_in_input[i-1])
      column_name <- paste0("status_", years_in_input[i])
      transitionType_column_name <- paste0("transitionType_", years_in_input[i])

      transitions <- transitions %>%
        dplyr::mutate(!!transitionType_column_name := dplyr::case_when(
        ((!!sym(column_name)) == 'dead' | is.na(!!sym(column_name)) ) & !!sym(column_name_prev) == 'live' ~ 'mortality',
        (!!sym(column_name)) == 'live' & is.na(!!sym(column_name_prev) ) ~ 'recruitment',
        ))
     transitions$t2Missing <- ifelse(is.na(transitions[[column_name]]) & !is.na(transitions[[transitionType_column_name]]), "missing", NA)
     mortality <- transitions %>% dplyr::filter(transitions[[transitionType_column_name]] == "mortality")
    }


    #   Associate biomass data in 'vst_agb_kg' with mortality transition data
    mortality <- merge(vst_agb_kg,
                       mortality %>% dplyr::select(-"sampledArea_m2", -"domainID"),
                       by = c("plotID", "siteID", "individualID", "taxonID"),
                       all.y = TRUE)

    ### if specified in mortalityMissin arg, ID individual(s) with status missing in time 2, filter them from mortality df, and filter same individualID(s) from vst_agb_kg
    if(mortalityMissing == "filterMissing"){
    missing <- mortality %>% dplyr::filter(.data$t2Missing == "missing") %>% dplyr::select(-"t2Missing") %>%
      dplyr::mutate(samplingInterval = samplingInterval, diameterIncrement = NA, diameterIncrementFlag = NA)
    missingIDlist <- unique(missing$individualID)
    mortality <- mortality %>% dplyr::filter(is.na(.data$t2Missing))
    missing_totalCount <- missing %>%
      dplyr::group_by(.data$plotID, .data$year) %>%
      dplyr::summarise(filteredCount = dplyr::n())
    vst_agb_kg <- vst_agb_kg %>% dplyr::filter(!.data$individualID %in% missingIDlist) # important: removes missing individualIDs from increment calculations
    }

    if(nrow(mortality) > 0) {
    mortality$agb_Mgha <- ifelse(is.na(mortality$agb_Mgha), 0, mortality$agb_Mgha) # placeholders for year 2 (only need mass from year 1)
    mortality$mortality_Mgha <- NA

    #   If transitionType for a given year is "mortality" then assign a mortality value based on the biomass at the PREVIOUS year
    for (i in 2:length(years_in_input)) {

      year_previous <- years_in_input[i-1]
      column_name <- paste0("transitionType_", years_in_input[i])

      mortality <- mortality %>%
        dplyr::mutate(mortality_Mgha = dplyr::case_when(
          (!!sym(column_name)) == 'mortality' & year == year_previous ~ .data$agb_Mgha,
          TRUE ~ .data$mortality_Mgha
        ))

    }
     mortality$year1 <- as.numeric(mortality$year)
     mortality$year2 <- as.numeric(mortality$year + samplingInterval)

    mortality$year <- as.numeric(mortality$year + samplingInterval) # for plot_mortality assign the live mass from year 1 as mortality mass in year 2

    plot_mortality <- mortality %>%
      dplyr::group_by(.data$siteID,
                      .data$plotID,
                      .data$year) %>%
      dplyr::summarise(mortality_Mgha = sum(.data$mortality_Mgha, na.rm = TRUE),
                       mortalityCount = dplyr::n())
   plot_mortality$mortality_Mghayr <- round(plot_mortality$mortality_Mgha /samplingInterval, digits = 3)

    mortality <- mortality %>% dplyr::filter(!is.na(.data$mortality_Mgha) )
  plot_mortality <- plot_mortality %>% dplyr::filter(.data$year == year2)

  # after creating plot-level summaries, format individual-level mortality table
     mortality$year <- NULL
     mortality <- mortality %>%
       dplyr::filter(!is.na(.data$mortality_Mgha)) %>%
       dplyr::rename("eventID1" = "eventID", "biomassWhenLive_kg" = "agb_kg") %>%
       dplyr::mutate(eventID2 = paste0("vst_",.data$siteID,"_",year2)) %>%
       dplyr::select(-"plot_eventID", -"agb_Mgha", -"simplePlantStatus") %>%
       dplyr::relocate("plotID", .after = "siteID") %>%
       dplyr::relocate("eventID1", .after = "plotID") %>%
       dplyr::relocate("eventID2", .after = "eventID1") %>%
       dplyr::relocate("year1", .after = "eventID2") %>%
       dplyr::relocate("year2", .after = "year1")   }

 }


  ####  CALCULATE RECRUITMENT

  # identify transitions that represent recruitment

  transition_simple <- transitions %>% dplyr::select(-"domainID",-"siteID", -"plotID", -"taxonID", -"sampledArea_m2", -"t2Missing")
  recruitment <- transition_simple  %>% dplyr::left_join(vst_agb_kg, by = c("individualID"))
  recruitment_input <- transitions  %>%  dplyr::select("plotID", "individualID", "sampledArea_m2", dplyr::starts_with("transitionType_")) %>%
                        tidyr::pivot_longer(cols = !c("plotID", "individualID", "sampledArea_m2"), names_to = "year", names_prefix = "transitionType_", values_to = "transition_type")
  recruitment_input <-  recruitment_input %>% dplyr::filter(.data$transition_type == "recruitment") %>% dplyr::select("individualID", "year") %>%
      dplyr::mutate(year = as.numeric(.data$year))
  recruitment_ind <- unique(recruitment_input$individualID)

  recruitment <- recruitment %>% dplyr::filter(.data$individualID %in% recruitment_ind) %>%
                       dplyr::mutate(samplingInterval = samplingInterval,
                                     diameterIncrement = (.data$stemDiameter - 9.9)/samplingInterval,
                                     diameterIncrementFlag = ifelse(.data$diameterIncrement >= 3, "flagged", NA))

  recruitmentPlots <- recruitment %>% dplyr::select("domainID","individualID","plotID","sampledArea_m2") %>% unique()
  recruitment_input <- recruitment_input %>% dplyr::left_join(recruitmentPlots, by = c("individualID"))

  if(stemIncrementFlagged == "filterFlagged"){
  recruitmentFlags <- recruitment %>% dplyr::filter(.data$diameterIncrementFlag == "flagged")
    recruitmentFlagsIDlist <- unique(recruitmentFlags$individualID)
  recruitment <- recruitment %>% dplyr::filter(.data$diameterIncrementFlag != "flagged" | is.na(.data$diameterIncrementFlag))
  recruitment_input <- recruitment_input %>% dplyr::filter(!.data$individualID %in% recruitmentFlagsIDlist )
  recruitmentFlags_totalCount <- recruitmentFlags %>%
      dplyr::group_by(.data$plotID, .data$year) %>%
      dplyr::summarise(filteredCount = dplyr::n())
  vst_agb_kg <- vst_agb_kg %>% dplyr::filter(!.data$individualID %in% recruitmentFlagsIDlist ) # important: removes individualIDs with implausible diameter increment from increment calculations
 }

  if(nrow(recruitment_input) >0 ) {
  # produce dataframe with structure required to be passed successfully as vst_apparentindividual to estimateWoodMass function
  recruitment_input$stemDiameter <- 10
  recruitment_input$basalStemDiameter <- recruitment_input$height <- recruitment_input$measurementHeight <- recruitment_input$basalStemDiameterMsrmntHeight <-
           recruitment_input$maxCrownDiameter <- recruitment_input$ninetyCrownDiameter <- NA
  recruitment_input$plantStatus <- "Live" # we are only looking at individuals that were live in most recent year so this is appropriate
  recruitment_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here
  recruitment_input$eventID <- paste0("vst_", substr(recruitment_input$individualID, 14, 17), "_", recruitment_input$year) # recreate eventID
  recruitment_input$siteID <- substr(recruitment_input$individualID, 14, 17)
  recruitment_input$year <- NULL
  # if recruitment were to be extended to other growthForms the following line would NOT be appropriate
  recruitment_input$growthForm <- "multi-bole tree" # required in order to call estimateWoodMass, which doesn't distinguish between single and multi bole trees
  recruitment_input$uid <- recruitment_input$namedLocation <- recruitment_input$dendrometerInstallationDate <- recruitment_input$initialGapMeasurementDate <- recruitment_input$initialBandStemDiameter <- recruitment_input$initialDendrometerGap <- 
    recruitment_input$dendrometerHeight <- recruitment_input$dendrometerGap <- recruitment_input$dendrometerCondition <- recruitment_input$bandStemDiameter <- recruitment_input$publicationDate <- recruitment_input$measuredBy <- 
    recruitment_input$recordedBy <- recruitment_input$dataEntryRecordID <- recruitment_input$release <- recruitment_input$dataQF <- recruitment_input$subplotID <- NA # placeholders, estimateWoodMass function removes them
  
  # produce dataframe with structure required to be passed successfully as vst_perplotperyear to estimateWoodMass function
  perplot_input <- vst_plot_Mgha
  perplot_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here
  perplot_input$samplingImpractical <- "OK"
  perplot_input$year <- NULL
  # if recruitment were to be extended to other growthForms the following two lines would NOT be appropriate
  perplot_input$totalSampledAreaShrubSapling <- perplot_input$totalSampledAreaLiana <- perplot_input$totalSampledAreaFerns <- perplot_input$totalSampledAreaOther <- NA
  perplot_input$totalSampledAreaTrees <- NA #perplot_input$sampledArea_m2 # we already know sampledArea_m2, but this is workaround to allow estimateWoodMass to recalculate it
  
  # bind required dataframes together for input to estimateWoodMass function
  recruitment_list <- list(vst_apparentindividual = recruitment_input,
                      vst_mappingandtagging = map_input,
                      vst_perplotperyear = perplot_input) # , 'vst_non-woody' = vst_non_woody )

  # call estimateWoodMass function to estimate species-specific mass of recruiting individual within minimum diameter of 10 cm
  recruitment_output <- estimateWoodMass(inputDataList = recruitment_list,
                            plotSubset = plotSubset,
                            growthFormSubset = "tree")

  # add taxonID
  taxon_per_ID <- recruitment_output$vst_agb_kg %>% dplyr::select("individualID", "taxonID")
  recruitment_input_w_taxonID <- merge(recruitment_input, taxon_per_ID, by = "individualID")
  recruitment_input_w_taxonID$year <- substr(recruitment_input_w_taxonID$eventID,10,13) # add year back 

  # summarize number of stems per taxonID for each plot and year
  recruitment_count <-  recruitment_input_w_taxonID %>%
      dplyr::group_by(.data$plotID,
                      .data$year,
                      .data$sampledArea_m2,
                      .data$taxonID) %>%
      dplyr::summarise(recruitment_count = dplyr::n(), .groups = "drop")

  # simplify table with biomass of 10 cm diameter individual for each taxonID
  taxon_biomass <- recruitment_output$vst_agb_kg %>%
      dplyr::select("taxonID", "agb_kg") %>%
      dplyr::distinct(.data$taxonID, .keep_all = TRUE)

  # link biomass to each taxonID
  recruitmentMass <- merge(recruitment_count, taxon_biomass, by = "taxonID")

  # multiply number of recruitment stems by taxon-specific biomass and then convert mass from kg to Mg/ha
  recruitmentMass$sampledArea_m2 <- as.numeric(recruitmentMass$sampledArea_m2)
  recruitmentMass$recruitment_Mgha <-  recruitmentMass$recruitment_count * recruitmentMass$agb_kg *  0.001 * (10000/recruitmentMass$sampledArea_m2)
  recruitmentMass$sampledArea_m2 <- recruitmentMass$recruitment_count <- recruitmentMass$agb_kg <- NULL

  # multiply number of recruitment stems by taxon-specific biomass and then convert mass from kg to Mg/ha
  plot_recruitment <- recruitmentMass %>%
    dplyr::group_by(.data$plotID, .data$year) %>%
    dplyr::summarize(recruitment_Mgha = sum(.data$recruitment_Mgha, na.rm = T)) %>%
    dplyr::ungroup()
  plot_recruitment$recruitment_Mghayr <- round(plot_recruitment$recruitment_Mgha /samplingInterval, digits = 3)

   recruitment_input$year <- substr(recruitment_input$eventID,10,13) # add year back 
    recruitment_totalCount <-  recruitment_input %>%
      dplyr::group_by(.data$plotID,
                      .data$year) %>%
      dplyr::summarise(recruitmentCount = dplyr::n(), .groups = "drop")
  plot_recruitment <- merge(plot_recruitment, recruitment_totalCount, by = c("plotID", "year"), all.x=TRUE)

    } else {
  plot_recruitment <- vst_plot_Mgha %>% dplyr::select("plotID", "year")
  plot_recruitment$recruitment_Mgha <- plot_recruitment$recruitment_Mghayr <- 0
  plot_recruitment$recruitmentCount <- 0
    }



  ### CALCULATE BIOMASS INCREMENT (Clark et al. 2001 approach 2 - stand level productivity calculation) ####

  if(stemIncrementFlagged == "filterFlagged"){
  diameter_inc <- vst_agb_kg %>% dplyr::filter(.data$simplePlantStatus == "live") %>%
      dplyr::select("plot_eventID",
                    "domainID",
                    "siteID",
                    "plotID",
                    "individualID",
                    "stemDiameter",
                    "year")

    #   Retain records unique with respect to individualID, taxonID, year, simplePlantStatus; don't need to worry about multi-bole smaller individuals because 'estimateWoodMass()' function combines mass for boles in output.
    diameter_inc <- diameter_inc %>%
      dplyr::distinct(.data$individualID,
                      .data$year,
                      .keep_all = TRUE)

    diameter_inc <- diameter_inc[order(diameter_inc$year),]
    diameter_inc$yearRel <- as.numeric(factor(diameter_inc$year)) # convert true year to relative year (1 or 2)

    diameter_wide <- tidyr::pivot_wider(diameter_inc,
                                      id_cols = c("domainID", "siteID", "plotID", "individualID"),
                                      names_from = "yearRel",
                                      names_prefix = "stemDiameter_",
                                      values_from = "stemDiameter")

    # Add fields if they don't exist
    if (!("stemDiameter_1" %in% names(diameter_wide))) {
      diameter_wide$stemDiameter_1 <- NA
    }
    if (!("stemDiameter_2" %in% names(diameter_wide))) {
      diameter_wide$stemDiameter_2 <- NA
    }

    diameter_wide$samplingInterval <- samplingInterval
    diameter_wide$diameterIncrement <- abs(as.numeric(diameter_wide$stemDiameter_2) - as.numeric(diameter_wide$stemDiameter_1))/samplingInterval
    diameter_wide$diameterIncrementFlag <- ifelse(diameter_wide$diameterIncrement > 3, "flagged", NA)

    incrementFlags <- diameter_wide %>% dplyr::filter(.data$diameterIncrementFlag == "flagged")

    if(nrow(incrementFlags) >0){
    incrementFlagsSimple <- incrementFlags %>% dplyr::select("individualID", "samplingInterval", "diameterIncrement", "diameterIncrementFlag")

    incrementFlaglist <- unique(incrementFlags$individualID)

    stemIncrementFlags <- vst_agb_kg %>% dplyr::filter(.data$individualID %in% incrementFlaglist )
    stemIncrementFlags <- merge(stemIncrementFlags, transition_simple, by = "individualID", all.x =TRUE)
    stemIncrementFlags <- merge(stemIncrementFlags, incrementFlagsSimple, by = "individualID", all.x =TRUE)
     vst_agb_kg <- vst_agb_kg %>% dplyr::filter(!.data$individualID %in% incrementFlaglist ) # important: removes missing individualIDs from increment calculations
    }
  }

  ############ Scale biomass per area and convert to Mg / ha ######################
  #   Remove records that cannot be scaled to a per area basis
  vst_agb_Mgha <- vst_agb_kg %>%
    dplyr::filter(!is.na(.data$sampledArea_m2) & .data$sampledArea_m2 > 0 & !is.na(.data$agb_kg))

  #   Create "Mg/ha" biomass estimate for each record; used in downstream plot- and site-level biomass estimation
  vst_agb_Mgha$agb_Mgha <- round(vst_agb_Mgha$agb_kg * 0.001 * (10000/vst_agb_Mgha$sampledArea_m2),
                                  digits = 4)


  ##  Create list of plot x eventIDs from vst_apparentindividaul data; later diff against what is reported in perplot data
  agb_ind_eventID_list <- unique(vst_agb_Mgha$plot_eventID)


  ##  Identify plot by eventID combos that don't have biomass values
  vst_agb_zeros <- base::setdiff(plot_eventID_list, agb_ind_eventID_list)

  vst_agb_zeros <- as.data.frame(vst_agb_zeros)

  vst_agb_zeros <- dplyr::rename(vst_agb_zeros,
                                 plot_eventID = vst_agb_zeros)

  vst_agb_zeros$plotID <- substr(vst_agb_zeros$plot_eventID, 1, 8)

  vst_agb_zeros$siteID <- substr(vst_agb_zeros$plot_eventID, 1, 4)

  vst_agb_zeros$year <- as.numeric(substr(vst_agb_zeros$plot_eventID, 19, 22))

  vst_agb_zeros$eventID <- substr(vst_agb_zeros$plot_eventID, 10, 22)

  vst_agb_zeros <- merge(vst_agb_zeros,
                         plotType_df,
                         by = "plotID",
                         all.x = TRUE)



  ### Generate plot-level biomass summary ####

  #   Sum biomass per unit area for each plot x year x simplePlantStatus x nlcdClass x taxonID combo (aggegate across individualIDs)
  vst_plot_summary <- vst_agb_Mgha %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$siteID,
                    .data$eventID,
                    .data$plotID,
                    .data$sampledArea_m2,
                    .data$eventType,
                    .data$plotType,
                    .data$nlcdClass,
                    .data$taxonID,
                    .data$growthForm,
                    .data$simplePlantStatus,
                    .data$year) %>%
    dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE),
                     .groups = "drop")

  #   Within a given year, transpose live and dead AGB into separate columns
  vst_plot_wide <- tidyr::pivot_wider(vst_plot_summary,
                                      id_cols = c("siteID", "plot_eventID", "eventID", "plotID",  "sampledArea_m2", "eventType",
                                                  "plotType", "nlcdClass", "taxonID", "growthForm", "year"),
                                      names_from = "simplePlantStatus",
                                      names_glue = "{simplePlantStatus}_Mgha",
                                      values_from = "agb_Mgha")
  if (!"dead_Mgha" %in% names(vst_plot_wide)) {
    vst_plot_wide$dead_Mgha <- NA
  }


  #   Assumption: Replace NAs created during transpose with zeroes; assume both live and dead were sampled in a plot

  vst_plot_wide$dead_Mgha[is.na(vst_plot_wide$dead_Mgha)] <- 0
  vst_plot_wide$live_Mgha[is.na(vst_plot_wide$live_Mgha)] <- 0


  #   Assign zero AGB values to plots with zero biomass
  vst_agb_zeros_plot <- vst_agb_zeros

  if (nrow(vst_agb_zeros_plot) > 0) {
    perplot_meta_for_missing <- unique(vst_plot_Mgha %>% dplyr::select("plotID", "eventID", "eventType", "nlcdClass"))
    vst_agb_zeros_plot <- merge(vst_agb_zeros_plot, perplot_meta_for_missing, by = c("plotID", "eventID"), all.x = T)
    vst_agb_zeros_plot$taxonID <-  vst_agb_zeros_plot$growthForm <- vst_agb_zeros_plot$sampledArea_m2 <- NA # placeholders to allow rbind without errors
    vst_agb_zeros_plot$dead_Mgha <- vst_agb_zeros_plot$live_Mgha <- 0

  #   Add rows for plots with zero biomass to plots with AGB
  vst_plot_Mgha <- rbind(vst_plot_wide,
                         vst_agb_zeros_plot)
  } else {
     vst_plot_Mgha <- vst_plot_wide
  }

  priority_plots <- priority_plots # force load from data table
  priority_plots_add <- priority_plots %>%
  dplyr::select("plotID",
                  "specificModuleSamplingPriority")

  #   Add 'specificModuleSamplingPriority' column to output
  vst_plot_Mgha <- merge(vst_plot_Mgha,
                         priority_plots_add,
                         by = c("plotID"),
                         all.x = TRUE)

  #   Retain AGB estimates for records with values for both live and dead biomass
  vst_plot_Mgha <- vst_plot_Mgha %>%
    dplyr::filter(!is.na(.data$live_Mgha) & !is.na(.data$dead_Mgha))


  #   Some taxonIDs are represented in multiple growthForms (e.g., sapling and single bole tree): This sums the growthForms
  vst_agb_Live <- vst_plot_Mgha %>%
    dplyr::group_by(.data$siteID,
                    .data$plotID,
                    .data$plotType,
                    .data$specificModuleSamplingPriority,
                    .data$eventID,
                    .data$year,
                    .data$plot_eventID
#                    , .data$nlcdClass
#                    , .data$taxonID
                    ) %>%
    dplyr::summarise(Mgha_live = sum(.data$live_Mgha, na.rm = TRUE),
#                     dead_Mgha = sum(.data$dead_Mgha, na.rm = TRUE),
                     .groups = "drop")

  vst_agb_Live <- vst_agb_Live[order(vst_agb_Live$year),]


  #   Convert 'vst_agb_Live' from long to wide format (all years in same row)
  vst_increment <- tidyr::pivot_wider(vst_agb_Live,
                                      id_cols = c("siteID", "plotID", "plotType"), # , "nlcdClass" , "taxonID"),
                                      names_from = "year",
                                      names_prefix = "Mgha_",
                                      values_from = "Mgha_live")

  #   Calculate plot-level increment
  for (i in 2:length(years_in_input)) {
    column_name_prev <- paste0("Mgha_", years_in_input[i - 1])
    column_name <- paste0("Mgha_", years_in_input[i])
    increment_column_name <- paste0("Mgha_increment_", years_in_input[i])
    vst_increment <- vst_increment %>%
      dplyr::mutate(!!increment_column_name := (!!sym(column_name)) - !!sym(column_name_prev))

  } # end 'for' loop


  vst_increment_long <- vst_increment %>%
    #   Remove individual 'year' columns
    dplyr::select(-dplyr::contains("Mgha_2"))  %>%

    #   Return to long format to obtain 'cols' below and increment by taxonID by year in each row
    tidyr::pivot_longer(cols = !c("plotID", "siteID","plotType"), #  "taxonID", "nlcdClass"
                        names_to = "year",
                        names_prefix = "Mgha_increment_",
                        values_to = "increment_Mgha")
 vst_increment_long$increment_Mghayr <- round(vst_increment_long$increment_Mgha /samplingInterval, digits = 3)

 vst_ANPP_plot <- vst_increment_long


  ### combine missing, recruitmentFlags, and stemIncrementFlags tables if they exist

  filtered_df_names <- c("missing", "recruitmentFlags", "stemIncrementFlags")

  # remove df if it exists and has 0 rows
  for (df_name in filtered_df_names) {
    if (exists(df_name) && is.data.frame(get(df_name)) && nrow(get(df_name)) == 0) {
      rm(list = df_name)
    }
  }

  # Filter to only those dataframe(s) that exist and are data frames
  existing_dfs <- lapply(filtered_df_names, function(name) {
    if (exists(name) && is.data.frame(get(name))) get(name) else NULL
  })

  # Remove NULLs
  existing_dfs <- Filter(Negate(is.null), existing_dfs)


  # Remove NULLs
  existing_dfs <- Filter(Negate(is.null), existing_dfs)

  # Bind rows if any data frames exist
  if (length(existing_dfs) > 0) {
    filtered <- dplyr::bind_rows(existing_dfs)
    filtered_totalCount <- filtered %>%
      dplyr::group_by(.data$plotID, .data$year) %>%
      dplyr::summarise(filteredCount = dplyr::n())
  } else {
  filtered <- data.frame()
  }

  ##############################################################################################
  #   Gather mortality, recruitment, and increment data into same dataframe
  if(nrow(plot_mortality) > 0){
    vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  plot_mortality,
                                  by = c("siteID", "plotID", "year"),
                                  all.x = TRUE)
  }
   if (!"mortalityCount" %in% names(vst_ANPP_plot)) {
  vst_ANPP_plot$mortalityCount <- 0
  vst_ANPP_plot$mortality_Mghayr <- 0
  } else {
  vst_ANPP_plot$mortalityCount <- ifelse(is.na(vst_ANPP_plot$mortalityCount), 0, vst_ANPP_plot$mortalityCount)
  vst_ANPP_plot$mortality_Mghayr <- ifelse(is.na(vst_ANPP_plot$mortality_Mghayr), 0, vst_ANPP_plot$mortality_Mghayr)
  }

  if(nrow(plot_recruitment) > 0){
  #   Add recruitment data to same dataframe
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  plot_recruitment,
                                  by = c("plotID", "year"),
                                  all.x = TRUE)
  }
   if (!"recruitmentCount" %in% names(vst_ANPP_plot)) {
  vst_ANPP_plot$recruitmentCount <- 0
  vst_ANPP_plot$recruitment_Mghayr <- 0
  } else {
  vst_ANPP_plot$recruitmentCount <- ifelse(is.na(vst_ANPP_plot$recruitmentCount), 0, vst_ANPP_plot$recruitmentCount)
  vst_ANPP_plot$recruitment_Mghayr <- ifelse(is.na(vst_ANPP_plot$recruitment_Mghayr), 0, vst_ANPP_plot$recruitment_Mghayr)
  }

  # Add filtered individual count, if applicable
  if(nrow(filtered) > 0){
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  filtered_totalCount,
                                  by = c("plotID", "year"),
                                  all.x = TRUE)
  }
  if (!"filteredCount" %in% names(vst_ANPP_plot)) {
  vst_ANPP_plot$filteredCount <- 0
  } else {
  vst_ANPP_plot$filteredCount <- ifelse(is.na(vst_ANPP_plot$filteredCount), 0, vst_ANPP_plot$filteredCount)
  }
  
  vst_ANPP_plot$samplingInterval <- samplingInterval
  vst_ANPP_plot <- vst_ANPP_plot %>%
       dplyr::relocate("samplingInterval", .after = "plotType") %>%
       dplyr::relocate("filteredCount", .after = "samplingInterval") %>%
       dplyr::relocate("mortalityCount", .after = "filteredCount") %>%
       dplyr::relocate("recruitmentCount", .after = "mortalityCount")


  #   Set "year" data type
  vst_ANPP_plot$year <- as.numeric(vst_ANPP_plot$year)

  #   Remove records with increment == NA; very important because NAs for both increment and mortality lead to false zeroes during subsequent group_by() steps.
  vst_ANPP_plot <- vst_ANPP_plot %>%
    dplyr::filter(!is.na(.data$increment_Mghayr))





  #   Sum increment plus mortality and subtract recruitment to get woody ANPP by plotID x year
  vst_ANPP_plot$woodANPP_Mghayr <- round(vst_ANPP_plot$increment_Mghayr + vst_ANPP_plot$mortality_Mghayr - vst_ANPP_plot$recruitment_Mghayr,
                                             digits = 3)

  # remove fields not adjusted forsamplingInterval
  vst_ANPP_plot$increment_Mgha <- vst_ANPP_plot$mortality_Mgha <- vst_ANPP_plot$recruitment_Mgha <- NULL


  priority_plots_add <- unique(priority_plots_add)

  #   Add plot prioritization number to per plot ANPP estimate dataframe
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                           priority_plots_add,
                           by = c("plotID"),
                           all.x = TRUE)

  #   Filter plot output with user-provided plotSubset argument
  if(plotType == "tower") {

    vst_ANPP_plot <- vst_ANPP_plot %>%
      dplyr::filter(.data$plotType == "tower")

  }

  vst_ANPP_plot <- vst_ANPP_plot %>%
    dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)

  #   Add metadata to contextualize ANPP estimates
  vst_NPP_plot_yearFirst <- vst_ANPP_plot %>%
    dplyr::group_by(.data$siteID,
                    .data$plotID,
                    .data$plotType) %>%
    dplyr::summarise(wood_N = dplyr::n(),
                     woodANPP_Mghayr_sd = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE),
                                                digits = 2),
                     woodANPP_Mghayr_se = round((.data$woodANPP_Mghayr_sd / sqrt(.data$wood_N)),
                                                digits = 2),
                     woodANPP_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),
                                             digits = 4),
                     .groups = "drop") %>%
    dplyr::mutate(wood_count_type = "years")

  #   Estimate ANPP at site level using plot data
  vst_ANPP_site <- vst_ANPP_plot %>%
    dplyr::group_by(.data$siteID, .data$year, .data$samplingInterval) %>%
    dplyr::summarise(woodPlotNum = dplyr::n(),
                     woodANPPSD_Mghayr = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE),
                                               digits = 2),
                     woodANPPMean_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),
                                                 digits = 4),
                     .groups = "drop")

    message(glue::glue("Returning productivity summary data frames for: {paste(siteID, collapse = ', ')}"))


    output.list <- list(
      vst_ANPP_plot = vst_ANPP_plot,
      vst_ANPP_site = vst_ANPP_site,
      filtered = filtered,
      mortality = mortality,
      recruitment = recruitment
    )


  return(output.list)

}