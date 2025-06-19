##############################################################################################
#' @title Estimate ANPP (Aboveground Net Primary Productivity) contributed by woody vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Calculate annual productivity of woody vegetation (and "non-woody perennial" vegetation, if included in inputDataList) using inputs from the companion estimateWoodMass() function.
#'
#' @details An input data list of woody biomass dataframes needs to be created by the companion estimateWoodMass() function and filtered to two eventIDs if needed. This input list object is read in and aboveground net primary productivity (ANPP) is calculated for woody vegetation from NEON "Vegetation structure" (DP1.10098.001) data.
#' The stand-level approach (approach 2) is used from Clark DA, S Brown, DW Kicklighter, JQ Chambers, JR Thomlinson, and J Ni. 2001. Measuring Net Primary Production in Forests: Concepts and Field Methods. Ecological Applications 11:356-370.
#' Woody productivity is only calculated for trees with growthForm of "single bole tree" or "multi-bole tree". Therefore, in estimateWoodMass function, specify growthForm "tree".
#' NEON has an extensive data QA/QC process, but users should be aware that these productivity estimates are very sensitive to any residual errors and so the data should be examined carefully
#'
#' @param siteID Specify the four-letter siteID for a single NEON site (only one siteID is allowed). [character]
#'
#' @param inputDataList Specify a loaded R list object (e.g. estimateWoodMassOutputs) that was produced by the companion estimateWoodMass function (and subsequently filtered to two eventIDs if needed). [character]
#'
#' @param plotSubset The available options for this function are the default of "towerAnnualSubset" (only the subset of tower plots that are sampled annually) or "towerAll" (all plots in the tower airshed but no distributed plots). [character]
#'#'
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
#' # Use estimateWoodMass to generate output list that is to be used as input to estimateWoodProd
#' estimateWoodMassOutputs <- estimateWoodMass(inputDataList = VstDat)
#'
#'
#' # example with arguments at default values
#' estimateWoodProdOutputs <- estimateWoodProd(inputDataList = estimateWoodMassOutputs)
#'
#'
#' }
#'
#' @export estimateWoodProd

estimateWoodProd = function(inputDataList,
                            siteID,
                            plotSubset = "towerAnnualSubset") {

  options(dplyr.summarise.inform = FALSE)

  ### Check that input arguments meet assumptions ####
  if (!methods::is(inputDataList, class = "list" )) {
    stop("The 'inputDataList' argument is expected to be a list generated via the 'estimateWoodMass()' function. A character, data.frame, or NA argument is not allowed.")
  }

  vst_agb_kg <- inputDataList$vst_agb_kg
  vst_plot_w_0s <- inputDataList$vst_plot_w_0s
  vst_deadNoMass <- inputDataList$vst_deadNoMass


  ### Check that input data only includes growthForms "single bole tree" and/or "multi-bole tree"
  if (any(!(vst_agb_kg$growthForm %in% c("single bole tree", "multi-bole tree")))) {
    stop(glue::glue("Input table 'vst_agb_kg' includes growthForms other than the currently supported 'single bole tree' and 'multi-bole tree'.
      Re-run estimateWoodMass() function with argument growthForm = 'tree' and use that output as the inputDataList for estimateWoodProd() function."))
  }

  #   Check that required tables within list match expected names
  listExpNames <- c("vst_agb_kg", "vst_plot_w_0s")

  if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from inputDataList list:",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
  }

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("towerAll", "towerAnnualSubset")) {
    stop("The only valid plotSubset options are 'towerAll', 'towerAnnualSubset'.")
  }

  plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50) # convert to numeric (50 is highest plotPriority)
  plotType <- ifelse(plotSubset == "towerAnnualSubset" | plotSubset == "towerAll", "tower", "distributed")
  plotType <- ifelse(plotSubset == "all", "all", plotType)

  ### Verify inputDataList tables contain required columns and data ####

  ### Verify user-supplied vst_agb_kg table contains required data
  #   Check for required columns
  vst_agb_kg_ExpCols <- c("siteID", "plotID", "eventID", "year",
                          "plot_eventID", "nlcdClass", "taxonID",
                          "individualID", "plantStatus2", "agb_kg")

  if (length(setdiff(vst_agb_kg_ExpCols, colnames(vst_agb_kg))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_agb_kg':",
                    '{paste(setdiff(vst_agb_kg_ExpCols, colnames(vst_agb_kg)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(vst_agb_kg) == 0) {
    stop(glue::glue("Table 'vst_agb_kg' has no data."))
  }




  ### Verify user-supplied vst_plot_w_0s table contains required data
  #   Check for required columns
  vst_plot_w_0s_ExpCols <- c("domainID", "siteID", "plotID", "eventID",
                             "year", "plot_eventID", "nlcdClass", "taxonID",
                             "Live_Mgha", "Dead_or_Lost_Mgha",
                             "specificModuleSamplingPriority", "plotType")

  if (length(setdiff(vst_plot_w_0s_ExpCols, colnames(vst_plot_w_0s))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_plot_w_0s':",
                    '{paste(setdiff(vst_plot_w_0s_ExpCols, colnames(vst_plot_w_0s)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(vst_plot_w_0s) == 0) {
    stop(glue::glue("Table 'vst_plot_w_0s' has no data."))
  }

  # used later when calling estimateWoodMass for recruitment
  map_input <- vst_agb_kg
  map_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here


  domainID_df <- unique(vst_plot_w_0s %>% dplyr::select("siteID", "domainID"))

  vst_agb_kg <- rbind(vst_agb_kg, vst_deadNoMass)

  # filter by eventType based on plotSubset argument
  if(plotSubset == "towerAll") {
    message(glue::glue("Since plotSubset 'towerAll' was selected, input data has been filtered to just those sampling bouts when all tower plots were sampled."))
      vst_plot_w_0s <- vst_plot_w_0s %>%
        dplyr::filter(.data$eventType == "allTowerPlots")

      vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_w_0s$plot_eventID)
    }

    ### Error if not a single site of data after filtering

    vst_plot_w_0s <- vst_plot_w_0s[vst_plot_w_0s$siteID == siteID, ]


    vst_agb_kg <- vst_agb_kg %>%
        dplyr::filter(.data$plot_eventID %in% vst_plot_w_0s$plot_eventID)

    sites_in_input <- unique(vst_plot_w_0s$siteID)

    if (length(sites_in_input) >1) {
    stop(glue::glue("Only one siteID is allowed in filtered dataset. Current filtered dataset only has woody biomass data from: {unique(vst_plot_w_0s$siteID)}"))
    }

    if(length(sites_in_input) == 0) {
    stop(glue::glue("Filtered dataset has no siteID. Select a different siteID argument and/or a different inputDataList"))
    }


    ### Error if not 2 years of data

  years_in_input <- unique(vst_plot_w_0s$year)

    if (length(years_in_input) < 2) {
    stop(glue::glue("Two years of data are needed to calculate woody productivity. Current filtered dataset only has woody biomass data from: {unique(vst_plot_w_0s$year)}"))
    }

    if (length(years_in_input) > 2) {
     stop(glue::glue("This function expects there to be data from only two eventID years after filtering based on plotSubset. The current filtered dataset has woody biomass data from: {paste(unique(vst_plot_w_0s$eventID), collapse = ', ')}"))
    }

 prod_interval <- abs(diff(years_in_input))

  # filter to plots with exactly 2 years in vst_plot_w_0s and provide warning if there was < 2 years or > 2 years
  vst_plot_w_0s <- vst_plot_w_0s %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::mutate(yr_count = dplyr::n_distinct(.data$year) )

  lt_2_yr <-  vst_plot_w_0s %>% dplyr::filter(.data$yr_count < 2) %>% dplyr::pull("plotID") %>% unique() %>% as.character()
  gt_2_yr <-  vst_plot_w_0s %>% dplyr::filter(.data$yr_count > 2) %>% dplyr::pull("plotID") %>% unique() %>% as.character()
  desired_2_yr <-  vst_plot_w_0s %>% dplyr::filter(.data$yr_count == 2)  %>% dplyr::pull("plotID") %>% unique() %>% as.character()

  vst_plot_w_0s <- vst_plot_w_0s %>%
    dplyr::filter(.data$yr_count == 2)

    if (length(lt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from less than two years and have been removed from vst_plot_w_0s: {paste(unique(lt_2_yr), collapse = ', ')}"))
    }

    if (length(gt_2_yr) > 0) {
    message(glue::glue("Exactly 2 years of data are needed to calculate woody productivity using this function. The following plots have data from more than two years and have been removed from vst_plot_w_0s: {paste(unique(gt_2_yr), collapse = ', ')}"))
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

  vst_agb_kg_incl_deadNoMass <- vst_agb_kg # for mortality estimates
  vst_agb_kg<- vst_agb_kg %>% dplyr::filter(!is.na(.data$agb_kg)) # after plot-level filtering above, remove records with no biomass as inputs from recruitment estimates

  ### Identify plotIDs and associated plotType in the dataset
  plotType_df <- unique(vst_plot_w_0s %>% dplyr::select("plotID", "plotType"))


  ### STAND-LEVEL BIOMASS INCREMENT (Clark et al. 2001 approach 2 - stand level productivity calculation) ####
  message("Calculating woody increment component of productivity at the stand-level ..... ")

  #   Some taxonIDs are represented in multiple growthForms (e.g., sapling and single bole tree): This sums the growthForms
  vst_agb_Live <- vst_plot_w_0s %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$plotType,
                    .data$specificModuleSamplingPriority,
                    .data$eventID,
                    .data$year,
                    .data$plot_eventID
#                    , .data$nlcdClass
#                    , .data$taxonID
                    ) %>%
    dplyr::summarise(Mgha_live = sum(.data$Live_Mgha, na.rm = TRUE),
#                     Dead_or_Lost_Mgha = sum(.data$Dead_or_Lost_Mgha, na.rm = TRUE),
                     .groups = "drop")

  vst_agb_Live <- vst_agb_Live[order(vst_agb_Live$year),]

  #   Sort list of years
  yearList <- unique(sort(vst_agb_Live$year))

  #   Convert 'vst_agb_Live' from long to wide format (all years in same row)
  vst_increment <- tidyr::pivot_wider(vst_agb_Live,
                                      id_cols = c("siteID", "plotID", "plotType"), # , "nlcdClass" , "taxonID"),
                                      names_from = "year",
                                      names_prefix = "Mgha_",
                                      values_from = "Mgha_live")

  #   Calculate plot-level increment (before incorporating mortality)
  for (i in 2:length(yearList)) {
    column_name_prev <- paste0("Mgha_", yearList[i - 1])
    column_name <- paste0("Mgha_", yearList[i])
    increment_column_name <- paste0("Mgha_increment_", yearList[i])
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
 vst_increment_long$increment_Mghayr <- round(vst_increment_long$increment_Mgha / prod_interval, digits = 3)

  ### CALCULATE STAND-LEVEL MORTALITY
  message("Calculating woody mortality component of productivity at the plot-level (approach 2) ..... ")

  if (nrow(vst_agb_kg_incl_deadNoMass) > 0) {

    #   convert kg to Mg/ha
    vst_agb_kg_incl_deadNoMass$agb_Mgha <- round(vst_agb_kg_incl_deadNoMass$agb_kg * 0.001 * (10000/vst_agb_kg_incl_deadNoMass$sampledAreaM2),
                                 digits = 4)


    ##  Categorize individualIDs based on their changes (or not) in plantStatus2
    input_to_transitions <- vst_agb_kg_incl_deadNoMass %>%
      dplyr::select("plot_eventID",
                    "siteID",
                    "plotID",
                    "sampledAreaM2",
                    "individualID",
                    "taxonID",
                    "plantStatus2",
                    "year")

    #   Retain records unique with respect to individualID, taxonID, year, plantStatus2; don't need to worry about multi-bole smaller individuals because 'estimateWoodMass()' function combines mass for boles in output.
    input_to_transitions <- input_to_transitions %>%
      dplyr::distinct(.data$individualID,
                      .data$taxonID,
                      .data$year,
                      .data$plantStatus2,
                      .keep_all = TRUE)

    input_to_transitions <- input_to_transitions[order(input_to_transitions$year),]

    #   Create sorted list of years
    yearList <- unique(sort(vst_agb_kg$year))

    area_lookup <- input_to_transitions %>% dplyr::select("plotID","year","sampledAreaM2") %>% dplyr::filter(!is.na(.data$sampledAreaM2)) %>% unique()
    input_to_transitions <- input_to_transitions %>% dplyr::select(-"sampledAreaM2")
    input_to_transitions <- merge(input_to_transitions, area_lookup, by = c("plotID","year"), all.x=T) # add sampledAreaM2 to records where it is missing

    transitions <- tidyr::pivot_wider(input_to_transitions,
                                      id_cols = c("siteID", "plotID", "individualID", "taxonID", "sampledAreaM2"),
                                      names_from = "year",
                                      names_prefix = "status_",
                                      values_from = "plantStatus2",
                                      values_fn = list
                                      )


    transitions <- as.data.frame(lapply(transitions, as.character)) # if there are >1 status values per group then the status column is a list; this converts to character
    transitions <- as.data.frame(lapply(transitions, function(x) { gsub('NULL', NA, x, fixed=TRUE)  })) # convert character NULL values to NA

    transitions <- transitions %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::contains("status", ignore.case = TRUE),
          .fns = ~ {
            ifelse(is.na(.x), NA, ifelse(grepl("Live", .x), "Live", "Dead_or_Lost")) # if at least one stem is Live, classify as Live
          }
        )
      )


    #   Identify cases where individual was previously "live" and is currently "dead_or_lost"
    for (i in 2:length(yearList)) {

      column_name_prev <- paste0("status_", yearList[i-1])
      column_name <- paste0("status_", yearList[i])
      transitionType_column_name <- paste0("transitionType_", yearList[i])

      transitions <- transitions %>%
        dplyr::mutate(!!transitionType_column_name := dplyr::case_when(
        (!!sym(column_name)) == 'Dead_or_Lost' & !!sym(column_name_prev) == 'Live' ~ 'mortality',
        (!!sym(column_name)) == 'Live' & is.na(!!sym(column_name_prev) ) ~ 'recruitment',
      ))
    }

    #   Associate biomass data in 'vst_agb_kg' with mortality transition data
    mortality <- merge(vst_agb_kg_incl_deadNoMass,
                       transitions %>% dplyr::select(-"sampledAreaM2"),
                       by = c("plotID", "siteID", "individualID", "taxonID"),
                       all.x = TRUE)

    mortality$agb_Mgha <- ifelse(is.na(mortality$agb_Mgha), 0, mortality$agb_Mgha) # placeholders for year 2 (only need mass from year 1)
    mortality$mortality_Mgha <- NA

    #   If transitionType for a given year is "mortality" then assign a mortality value based on the biomass at the PREVIOUS year
    for (i in 2:length(yearList)) {

      year_previous <- yearList[i-1]
      column_name <- paste0("transitionType_", yearList[i])

      mortality <- mortality %>%
        dplyr::mutate(mortality_Mgha = dplyr::case_when(
          (!!sym(column_name)) == 'mortality' & year == year_previous ~ .data$agb_Mgha,
          TRUE ~ .data$mortality_Mgha
        ))

    }

    mortality$year <- as.numeric(mortality$year + prod_interval) # we are assigning the live mass from year 1 as mortality mass in year 2

    plot_mortality <- mortality %>%
      dplyr::group_by(.data$siteID,
                      .data$plotID,
                      .data$year) %>%
      dplyr::summarise(mortality_Mgha = sum(.data$mortality_Mgha, na.rm = TRUE))
   plot_mortality$mortality_Mghayr <- round(plot_mortality$mortality_Mgha / prod_interval, digits = 3)


    ##  Conditional: Create placeholder 'plot_mortality' dataframe when rows in 'vst_agb_kg' == 0
    } else {

    plot_mortality <- data.frame(siteID = character(),
                                 plotID = character(),
                                 taxonID = character(),
                                 year = character(),
                                 mortality_Mghayr = numeric(),
                                 mortality_Mghayr = numeric())
    }


  ####  Calculate recruitment

  # identify transitions that represent recruitment
  recruitment_input <- transitions  %>%  dplyr::select("plotID", "individualID", "sampledAreaM2", dplyr::starts_with("transitionType_")) %>%
                        tidyr::pivot_longer(cols = !c("plotID", "individualID", "sampledAreaM2"), names_to = "year", names_prefix = "transitionType_", values_to = "transition_type")
  recruitment_input <-  recruitment_input %>% dplyr::filter(.data$transition_type == "recruitment")

  if(nrow(recruitment_input) >0 ) {
  # produce dataframe with structure required to be passed successfully as vst_apparentindividual to estimateWoodMass function
  recruitment_input$stemDiameter <- 10
  recruitment_input$basalStemDiameter <- recruitment_input$height <- recruitment_input$measurementHeight <- recruitment_input$basalStemDiameterMsrmntHeight <-
           recruitment_input$maxCrownDiameter <- recruitment_input$ninetyCrownDiameter <- NA
  recruitment_input$plantStatus <- "Live" # we are only looking at individuals that were Live in most recent year so this is appropriate
  recruitment_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here
  recruitment_input$eventID <- paste0("vst_", substr(recruitment_input$plotID, 1, 4), "_", recruitment_input$year) # recreate eventID
  recruitment_input$siteID <- substr(recruitment_input$plotID, 1, 4)
  recruitment_input <- merge(recruitment_input, domainID_df, by = "siteID", all.x = T)
  # if recruitment were to be extended to other growthForms the following line would NOT be appropriate
  recruitment_input$growthForm <- "multi-bole tree" # required in order to call estimateWoodMass, which doesn't distinguish between single and multi bole trees

  # produce dataframe with structure required to be passed successfully as vst_perplotperyear to estimateWoodMass function
  perplot_input <- vst_plot_w_0s
  perplot_input$date <- "2000-01-01" # placeholder, not needed since don't have duplicates to sort by date here
  perplot_input$samplingImpractical <- "OK"
  perplot_input$year <- NULL
  # if recruitment were to be extended to other growthForms the following two lines would NOT be appropriate
  perplot_input$totalSampledAreaShrubSapling <- perplot_input$totalSampledAreaLiana <- perplot_input$totalSampledAreaFerns <- perplot_input$totalSampledAreaOther <- NA
  perplot_input$totalSampledAreaTrees <- perplot_input$sampledAreaM2 # we already know sampledAreaM2, but this is workaround to allow estimateWoodMass to recalculate it

  # bind required dataframes together for input to estimateWoodMass function
  recruitment_list <- list(vst_apparentindividual = recruitment_input,
                      vst_mappingandtagging = map_input,
                      vst_perplotperyear = perplot_input) # , 'vst_non-woody' = vst_non_woody )

  # call estimateWoodMass function to estimate species-specific mass of recruiting individual within minimum diameter of 10 cm
  recruitment_output <- estimateWoodMass(inputDataList = recruitment_list,
                            plotSubset = plotSubset,
                            growthForm = "tree")

  # add taxonID
  taxon_per_ID <- recruitment_output$vst_agb_kg %>% dplyr::select("individualID", "taxonID")
  recruitment_input_w_taxonID <- merge(recruitment_input, taxon_per_ID, by = "individualID")

  # summarize number of stems per taxonID for each plot and year
  recruitment_count <-  recruitment_input_w_taxonID %>%
      dplyr::group_by(.data$plotID,
                      .data$year,
                      .data$sampledAreaM2,
                      .data$taxonID) %>%
      dplyr::summarise(recruitment_count = dplyr::n(), .groups = "drop")

  # simplify table with biomass of 10 cm diameter individual for each taxonID
  taxon_biomass <- recruitment_output$vst_agb_kg %>%
      dplyr::select("taxonID", "agb_kg") %>%
      dplyr::distinct(.data$taxonID, .keep_all = TRUE)

  # link biomass to each taxonID
  recruitment <- merge(recruitment_count, taxon_biomass, by = "taxonID")

  # multiply number of recruitment stems by taxon-specific biomass and then convert mass from kg to Mg/ha
  recruitment$sampledAreaM2 <- as.numeric(recruitment$sampledAreaM2)
  recruitment$recruitment_Mgha <-  recruitment$recruitment_count * recruitment$agb_kg *  0.001 * (10000/recruitment$sampledAreaM2)
  recruitment$sampledAreaM2 <- recruitment$recruitment_count <- recruitment$agb_kg <- NULL

  # multiply number of recruitment stems by taxon-specific biomass and then convert mass from kg to Mg/ha
  plot_recruitment <- recruitment %>%
    dplyr::group_by(.data$plotID, .data$year) %>%
    dplyr::summarize(recruitment_Mgha = sum(.data$recruitment_Mgha, na.rm = T)) %>%
    dplyr::ungroup()
  plot_recruitment$recruitment_Mghayr <- round(plot_recruitment$recruitment_Mgha / prod_interval, digits = 3)

    } else {
  plot_recruitment <- vst_increment_long %>% dplyr::select("plotID", "year")
  plot_recruitment$recruitment_Mgha <- plot_recruitment$recruitment_Mghayr <- 0
  }

  #   Gather mortality and increment data into same dataframe
  vst_ANPP_plot <- merge(vst_increment_long,
                                  plot_mortality,
                                  by = c("siteID", "plotID", "year"),
                                  all.x = TRUE)


  #   Add recruitment data to same dataframe
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  plot_recruitment,
                                  by = c("plotID", "year"),
                                  all.x = TRUE)


  #   Set "year" data type
  vst_ANPP_plot$year <- as.numeric(vst_ANPP_plot$year)

  #   Remove records with increment == NA; very important because NAs for both increment and mortality lead to false zeroes during subsequent group_by() steps.
  vst_ANPP_plot <- vst_ANPP_plot %>%
    dplyr::filter(!is.na(.data$increment_Mghayr))


  #   Assign mortality of zero if mortality is missing but increment is present
  vst_ANPP_plot$mortality_Mghayr <- dplyr::if_else(is.na(vst_ANPP_plot$mortality_Mghayr) &
                                                            !is.na(vst_ANPP_plot$increment_Mghayr),
                                                          0,
                                                          vst_ANPP_plot$mortality_Mghayr,
                                                          vst_ANPP_plot$mortality_Mghayr)

  #   Assign recruitment of zero if recruitment is missing but increment is present
  vst_ANPP_plot$recruitment_Mghayr <- dplyr::if_else(is.na(vst_ANPP_plot$recruitment_Mghayr) &
                                                            !is.na(vst_ANPP_plot$increment_Mghayr),
                                                          0,
                                                          vst_ANPP_plot$recruitment_Mghayr,
                                                          vst_ANPP_plot$recruitment_Mghayr)

  #   Sum increment plus mortality to get woody ANPP by plotID x year
  vst_ANPP_plot$woodANPP_Mghayr <- round(vst_ANPP_plot$increment_Mghayr + vst_ANPP_plot$mortality_Mghayr + vst_ANPP_plot$recruitment_Mghayr,
                                             digits = 3)

  # remove fields not adjusted for prod_interval
  vst_ANPP_plot$increment_Mgha <- vst_ANPP_plot$mortality_Mgha <- vst_ANPP_plot$recruitment_Mgha <- NULL


  priority_plots_add <- vst_plot_w_0s %>%
    dplyr::select("plotID",
                  "specificModuleSamplingPriority")

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
    dplyr::group_by(.data$siteID, .data$year) %>%
    dplyr::summarise(woodPlotNum = dplyr::n(),
                     woodANPPSD_Mghayr = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE),
                                               digits = 2),
                     woodANPPMean_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),
                                                 digits = 4),
                     .groups = "drop")



    message("Returning productivity summary data frames as a list object  ..... ")

    output.list <- list(
      vst_ANPP_plot = vst_ANPP_plot,
      vst_ANPP_site = vst_ANPP_site
    )


  return(output.list)

}



