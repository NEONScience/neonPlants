##############################################################################################
#' @title Estimate ANPP (Aboveground Net Primary Productivity) contributed by woody vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Calculate annual productivity of woody vegetation (and "non-woody perennial" vegetation, if included in inputDataList) using inputs from the companion estimateWoodMass() function.
#'
#' @details An input data list of woody biomass dataframes created by the companion estimateWoodMass() function is read in and aboveground net primary productivity (ANPP) is calculated for woody vegetation from NEON "Vegetation structure" (DP1.10098.001) data. The stand-level approach (approach 2) is used from Clark DA, S Brown, DW Kicklighter, JQ Chambers, JR Thomlinson, and J Ni. 2001. Measuring Net Primary Production in Forests: Concepts and Field Methods. Ecological Applications 11:356-370.
#' At present woody productivity is only calculated for trees with growthForm of "single bole tree" or "multi-bole tree". Therefore, in estimateWoodMass function, specify growthForm "tree".
#' NEON has an extensive data QA/QC process, but users should be aware that these productivity estimates are very sensitive to any residual errors and so the data should be examined carefully
#'
#' @param inputDataList Specify a loaded R list object (e.g. estimateWoodMassOutputs) that was produced by the companion estimateWoodMass function. [character]
#'
#' @param plotSubset The only current option is "towerAnnualSubset" (only the subset of tower plots that are sampled annually). Other options that may be added in the future are "all" (all tower and distributed plots), "towerAll" (all plots in the tower airshed but no distributed plots), and "distributed" (all distributed plots, which are sampled in 5-yr bouts and are spatially representative of the NLCD classes at at site). [character]
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
                            plotSubset = "towerAnnualSubset") {

  options(dplyr.summarise.inform = FALSE)

  ### Check that input arguments meet assumptions ####
  if (!methods::is(inputDataList, class = "list" )) {
    stop("The 'inputDataList' argument is expected to be a list generated via the 'estimateWoodMass()' function. A character, data.frame, or NA argument is not allowed.")
  }

  vst_agb_kg <- inputDataList$vst_agb_kg
  vst_agb_zeros <- inputDataList$vst_agb_zeros
  vst_plot_w_0s <- inputDataList$vst_plot_w_0s
  vst_site <- inputDataList$vst_site

  ### Check that input data only includes growthForms "single bole tree" and/or "multi-bole tree"
  if (any(!(vst_agb_kg$growthForm %in% c("single bole tree", "multi-bole tree")))) {
    stop(glue::glue("Input table 'vst_agb_kg' includes growthForms other than the currently supported 'single bole tree' and 'multi-bole tree'.
      Re-run estimateWoodMass() function with argument growthForm = 'tree' and use that output as the inputDataList for estimateWoodProd() function."))
  }

  #   Check that required tables within list match expected names
  listExpNames <- c("vst_agb_kg", "vst_plot_w_0s", "vst_agb_zeros", "vst_site")

  if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from inputDataList list:",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
  }

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'towerAll', 'towerAnnualSubset', 'distributed'.")
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



  ### Verify user-supplied vst_site contains required data
  #   Check for required columns
  vst_site_ExpCols <- c("siteID", "year", "woodPlotNum",
                        "woodLiveMassMean_Mgha", "woodLiveMassSD_Mgha")

  if (length(setdiff(vst_site_ExpCols, colnames(vst_site))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_site':",
                    '{paste(setdiff(vst_site_ExpCols, colnames(vst_site)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(vst_site) == 0) {
    stop(glue::glue("Table 'vst_site' has no data."))
  }


  ### Error if not at least 2 years of data
  start <- min(vst_plot_w_0s$year)
  end  <- max(vst_plot_w_0s$year)

    if (as.numeric(end) - as.numeric(start) < 1) {

    stop(glue::glue("At least 2 years of data are needed to calculate woody productivity. Current dataset only has woody biomass data from: {unique(vst_agb_kg$year)}"))

    }


  input_plots <- unique(vst_plot_w_0s$plotID)

  vst_plot_w_0s <- vst_plot_w_0s %>%
    dplyr::group_by(.data$plotID) %>%
    dplyr::filter(dplyr::n_distinct(.data$year) > 1)

  plots_w_gt_1_yr <- unique(vst_plot_w_0s$plotID)

  vst_agb_kg <- vst_agb_kg %>% dplyr::filter(.data$plotID %in% plots_w_gt_1_yr)


  # warn that plots with only one year have been removed from vst_agb_kg
    vst_agb_kg_w_only_1_yr <- setdiff(inputDataList$vst_agb_kg$plotID, plots_w_gt_1_yr)

    if (length(vst_agb_kg_w_only_1_yr) > 0) {

    message(glue::glue("At least 2 years of data are needed to calculate woody productivity. The following plots have data from only one year and have been removed from vst_agb_kg: {paste(unique(vst_agb_kg_w_only_1_yr), collapse = ', ')}"))

    }


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
  # vst_plot_w_0s_sum_taxa <- vst_plot_w_0s
  #
  # if(nrow(vst_plot_w_0s_sum_taxa) > 0) {
  #
  #   vst_plot_w_0s_sum_taxa <- vst_plot_w_0s %>%
  #     dplyr::group_by(.data$plot_eventID,
  #                     .data$eventID,
  #                     .data$siteID,
  #                     .data$plotID,
  #                     .data$plotType,
  #                     .data$nlcdClass,
  #                     .data$year) %>%
  #     dplyr::summarise(Live_Mgha = sum(.data$Live_Mgha, na.rm = TRUE),
  #                      Dead_or_Lost_Mgha = sum(.data$Dead_or_Lost_Mgha, na.rm = TRUE),
  #                      .groups = "drop")
  #
  # } else {
  #
  #   vst_plot_w_0s_sum_taxa$Live_Mgha = 0
  #   vst_plot_w_0s_sum_taxa$Dead_or_Lost_Mgha = 0
  #
  # }
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



  ### CALCULATE STAND-LEVEL MORTALITY
  message("Calculating woody mortality component of productivity at the plot-level (approach 2) ..... ")

  if (nrow(vst_agb_kg) > 0) {

    #   Remove records that cannot be scaled to a per area basis
    vst_agb_kg <- vst_agb_kg %>%
      dplyr::filter(!is.na(.data$sampledAreaM2) & .data$sampledAreaM2 > 0)

    #   convert kg to Mg/ha
    vst_agb_kg$agb_Mgha <- round(vst_agb_kg$agb_kg * 0.001 * (10000/vst_agb_kg$sampledAreaM2),
                                 digits = 4)


    ##  Categorize individualIDs based on their changes (or not) in plantStatus2
    input_to_transitions <- vst_agb_kg %>%
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
    mortality <- merge(vst_agb_kg,
                       transitions,
                       by = c("plotID", "siteID", "individualID", "taxonID"),
                       all.x = TRUE)

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

    mortality$year <- as.numeric(mortality$year + 1)

    plot_mortality <- mortality %>%
      dplyr::group_by(.data$siteID,
                      .data$plotID,
                      .data$year) %>%
      dplyr::summarise(mortality_Mgha = sum(.data$mortality_Mgha, na.rm = TRUE))


  ##  Conditional: Create placeholder 'plot_mortality' dataframe when rows in 'vst_agb_kg' == 0
  } else {

    plot_mortality <- data.frame(siteID = character(),
                                 plotID = character(),
                                 taxonID = character(),
                                 year = character(),
                                 Mgha_mortality = numeric())
  }


    #  Calculate recruitment


    recruitment_input <- transitions  %>%  dplyr::select("plotID", "individualID", "sampledAreaM2", dplyr::starts_with("transitionType_")) %>%
                        tidyr::pivot_longer(cols = !c("plotID", "individualID", "sampledAreaM2"), names_to = "year", names_prefix = "transitionType_", values_to = "transition_type")

     recruitment_input <-  recruitment_input %>% dplyr::filter(.data$transition_type == "recruitment")

     recruitment <-  recruitment_input %>%
      dplyr::group_by(.data$plotID,
                      .data$sampledAreaM2,
                      .data$year) %>%
      dplyr::summarise(recruitment_count = dplyr::n(), .groups = "drop")

    # ave_biomass<- exp(b0 + b1 * log(min_stemDiameter)) # generalized allometric equation
    ave_biomass_kg<- round(exp(-2.2118 + 2.4133 * log(10)), digits = 3) # b0 and b1 from Chojnacky et al "other group" (7th hardwood group)
    recruitment$sampledAreaM2 <- as.numeric(recruitment$sampledAreaM2)
    recruitment$recruitment_Mgha <-  recruitment$recruitment_count * ave_biomass_kg *  0.001 * (10000/recruitment$sampledAreaM2) # multiply number of recruitment stems by ave mass (kg) and then convert to Mg/ha
    recruitment$sampledAreaM2 <- NULL


  #   Gather mortality and increment data into same dataframe
  vst_ANPP_plot <- merge(vst_increment_long,
                                  plot_mortality,
                                  by = c("siteID", "plotID", "year"),
                                  all.x = TRUE)


  #   Add recruitment data to same dataframe
  vst_ANPP_plot <- merge(vst_ANPP_plot,
                                  recruitment,
                                  by = c("plotID", "year"),
                                  all.x = TRUE) %>% dplyr::select(-"recruitment_count")



  #   Set "year" data type
  vst_ANPP_plot$year <- as.numeric(vst_ANPP_plot$year)

  #   Remove records with increment == NA; very important because NAs for both increment and mortality lead to false zeroes during subsequent group_by() steps.
  vst_ANPP_plot <- vst_ANPP_plot %>%
    dplyr::filter(!is.na(.data$increment_Mgha))


  #   Assign mortality of zero if mortality is missing but increment is present
  vst_ANPP_plot$mortality_Mgha <- dplyr::if_else(is.na(vst_ANPP_plot$mortality_Mgha) &
                                                            !is.na(vst_ANPP_plot$increment_Mgha),
                                                          0,
                                                          vst_ANPP_plot$mortality_Mgha,
                                                          vst_ANPP_plot$mortality_Mgha)

  #   Assign recruitment of zero if recruitment is missing but increment is present
  vst_ANPP_plot$recruitment_Mgha <- dplyr::if_else(is.na(vst_ANPP_plot$recruitment_Mgha) &
                                                            !is.na(vst_ANPP_plot$increment_Mgha),
                                                          0,
                                                          vst_ANPP_plot$recruitment_Mg,
                                                          vst_ANPP_plot$recruitment_Mg)



  #   Sum increment plus mortality to get woody ANPP by plotID x year
  vst_ANPP_plot$woodANPP_Mghayr <- round(vst_ANPP_plot$increment_Mgha + vst_ANPP_plot$mortality_Mgha,
                                             digits = 3)




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



