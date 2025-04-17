##############################################################################################
#' @title Estimate ANPP (Aboveground Net Primary Productivity) contributed by woody vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Calculate annual productivity of woody vegetation (and "non-woody perennial" vegetation, if included in inputDataList) using inputs from the companion estimateWoodMass() function. 
#' 
#' @details Input data can be filtered by plot type and plot priority. An input data list of woody biomass dataframes created by the companion estimateWoodMass() function is read in and aboveground net primary productivity (ANPP) is calculated for woody vegetation from NEON "Vegetation structure" (DP1.10098.001) data. Both a whole plot level approach and an individual-level approach calculation method are used, and output from the desired calculation method is returned. 
#' 
#' The individual-level approach is currently an underestimate since it includes the growth increment component but not the recruitment component. The recruitment component is currently very sensitive to stems that were overlooked in previous time periods, but when from true in-growth can be isolated the recruitment component will be added. Plots can be filtered to more frequently sampled plots using plotType and plotPriority arguments, and outlier observations can be removed.
#' 
#' @param inputDataList Specify a loaded R list object (e.g. estimateWoodMassOutputs) that was produced by companion estimateWoodMass function. [character]
#' 
#' @param plotType Optional filter for NEON plot type. Options are "tower" (default) or "all". A subset of 5 Tower plots are sampled annually (those plots with the highest plot priority), and remaining Tower plots are scheduled every 5 years. If "all" is selected, results include data from Distributed plots that are also sampled every 5 years. [character]
#' 
#' @param plotPriority NEON plots have a priority number to spatially balance plots by NLCD class, etc. in the event that not all scheduled plots can be sampled. The lower the number the higher the priority. Options are "all" or "5". The default is "5" which retains just the 5 highest priority Tower plots that are sampled annually. [character]
#' 
#' @param calcMethod Select individual-level (Approach 1) or plot-level (Approach 2) productivity calculation methods; the default is "approach_1". See Clark DA, S Brown, DW Kicklighter, JQ Chambers, JR Thomlinson, and J Ni. 2001. Measuring Net Primary Production in Forests: Concepts and Field Methods. Ecological Applications 11:356-370. [character]
#' 
#' @param outlier Specify how much (if any) outlier removal should be performed. The default is 1.5. [numeric]
#' 
#' @param outlierType Specify the type of outlier, either SD (standard deviations) or IQR (interquartile range). The default is "IQR". [character]
#' 
#' @return A list that includes productivity summary data frames. Output tables include:
#'   * vst_ANPP_plot_w_taxa - Summarises woody ANPP for each plot x year x taxonID combination ("Mg/ha/yr").
#'   * vst_ANPP_plot - Summarizes woody ANPP for each plot x year combination ("Mg/ha/yr").
#'   * vst_ANPP_site - Summarizes woody ANPP for each site x year combination ("Mg/ha/yr").
#'   * increment_all - Contains the increments for each woody individual (approach 1 only).
#'   * increment_outlier - Contains the woody individual increments that were removed during outlier removal (approach 1 only).
#' 
#'  If calcMethod "approach_2" is selected then only the first three dataframes are returned, and they include an "_2" suffix.
#' 
#' @examples
#' \dontrun{
#' # If list is not in memory, load woody biomass list of dataframes from local file:
#' load('estimateWoodMassOutputs.rds') # load list of dataframes created by estimateWoodMass function
#'
#' # example with arguments at default values
#' estimateWoodProdOutputs <- estimateWoodProd(input = estimateWoodMassOutputs)
#'                       
#' # example specifying many non-default arguments
#' estimateWoodProdOutputs <- estimateWoodProd(
#' input = estimateWoodMassOutputs, 
#' plotType = "all", 
#' plotPriority = 50, 
#' calcMethod = "approach_2", 
#' outlier = 2, 
#' outlierType = "SD"
#' )
#' 
#' }
#' 
#' @export estimateWoodProd

estimateWoodProd = function(inputDataList,
                            plotType = "tower",
                            plotPriority = "5",
                            calcMethod = "approach_1",
                            outlier = 1.5,
                            outlierType = "IQR") {
  
  options(dplyr.summarise.inform = FALSE)
  
  
  
  ### Check that input arguments meet assumptions ####
  if (!methods::is(inputDataList, class = "list" )) {
    stop("The 'inputDataList' argument is expected to be a list generated via the 'estimateWoodMass()' function. A character, data.frame, or NA argument is not allowed.")
  }
  
  list2env(inputDataList ,.GlobalEnv)
  vst_agb_zeros <- inputDataList$vst_agb_zeros
  vst_plot_w_0s <- inputDataList$vst_plot_w_0s
  vst_site <- inputDataList$vst_site

  #   Check that required tables within list match expected names
  listExpNames <- c("vst_agb_kg", "vst_plot_w_0s", "vst_agb_zeros", "vst_site")
  
  if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from inputDataList list:",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
  } 

  # Error if invalid plotType option selected
   if (!plotType %in% c("tower", "all")) {
      stop("The only valid plotType options are 'tower' or 'all'.")
    }  
    
  # Error if invalid plotPriority option selected
  if (!(as.character(plotPriority)) %in% c("all", "5")) {
    stop("The plotPriority argument should be either 'all' (all plots) or '5' (the 5 highest priority plots).")
  }  
  plotPriority <- ifelse(plotPriority == "5", 5, 30) # convert to numeric (30 is highest plotPriority)

  
  # Error if invalid calcMethod selected
  if(calcMethod != "approach_1" & calcMethod != "approach_2"){
    stop("The only valid outlierType options are 'approach_1' or 'approach_2'.")
  }

  # Error if invalid outlierType selected
  if(outlierType != "SD" & outlierType != "IQR"){
    stop("The only valid calcMethod options are 'SD' or 'IQR'.")
  }

  
  
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
  
  
  
  ### Verify user-supplied vst_agb_zeros table contains required data
  #   Check for required columns
  vst_agb_zeros_ExpCols <- c("siteID", "plotID", "eventID", "year",
                             "plot_eventID", "plotType")
  
  if (length(setdiff(vst_agb_zeros_ExpCols, colnames(vst_agb_zeros))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_agb_zeros':", 
                    '{paste(setdiff(vst_agb_zeros_ExpCols, colnames(vst_agb_zeros)), collapse = ", ")}',
                    .sep = " "))
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
  
  
  start <- min(vst_plot_w_0s$year)
  end  <- max(vst_plot_w_0s$year)
  
  
  ### Error if not at least 2 years of data
  if (as.numeric(end) - as.numeric(start) < 1) {
    
    stop(glue::glue("At least 2 years of data are needed to calculate woody productivity (more for plots with a sampling interval longer than annual). Current dataset only has woody biomass data from: {unique(vst_agb_kg$year)}"))
    
  }

  
  
  ### Identify plotIDs and associated plotType in the dataset
  plotType_df <- unique(vst_plot_w_0s %>% dplyr::select("plotID", "plotType"))
  
  
  
  ### PLOT-LEVEL BIOMASS INCREMENT (Clark et al. 2001 approach 2 - stand level productivity calculation) ####
  message("Calculating woody increment component of productivity at the plot-level (approach 2) ..... ")
  
  #   Some taxonIDs are represented in multiple growthForms (e.g., sapling and single bole tree): This sums the growthForms
  vst_agb_Live <- vst_plot_w_0s %>% 
    dplyr::group_by(.data$domainID, 
                    .data$siteID, 
                    .data$plotID, 
                    .data$plotType, 
                    .data$specificModuleSamplingPriority, 
                    .data$eventID, 
                    .data$year, 
                    .data$plot_eventID, 
                    .data$nlcdClass, 
                    .data$taxonID) %>% 
    dplyr::summarise(Live_Mgha = sum(.data$Live_Mgha, na.rm = TRUE), 
                     Dead_or_Lost_Mgha = sum(.data$Dead_or_Lost_Mgha, na.rm = TRUE),
                     .groups = "drop")
  
  vst_agb_Live$Mgha_live <- vst_agb_Live$Live_Mgha
  vst_agb_Live$Live_Mgha <- vst_agb_Live$Dead_or_Lost_Mgha <- NULL
  vst_agb_Live <- vst_agb_Live[order(vst_agb_Live$year),]
  
  #   Sort list of years
  yearList <- unique(sort(vst_agb_Live$year))
  
  #   Convert 'vst_agb_Live' from long to wide format (all years in same row)
  vst_increment <- tidyr::pivot_wider(vst_agb_Live, 
                                      id_cols = c("siteID", "plotID", "plotType", "nlcdClass", "taxonID"), 
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
    tidyr::pivot_longer(cols = !c("plotID", "siteID", "taxonID", "nlcdClass", "plotType"), 
                        names_to = "year",  
                        names_prefix = "Mgha_increment_", 
                        values_to = "Mgha_increment")

  
  
  ### CALCULATE PLOT-LEVEL MORTALITY
  message("Calculating woody mortality component of productivity at the plot-level (approach 2) ..... ")

  if (nrow(vst_agb_kg) >0) {
    
    #   Remove records that cannot be scaled to a per area basis
    vst_agb_kg <- vst_agb_kg %>% 
      dplyr::filter(!is.na(.data$sampledAreaM2) & .data$sampledAreaM2 > 0 )
    
    #   convert kg to Mg
    vst_agb_kg$agb_Mgha <- round(vst_agb_kg$agb_kg * 0.001 * (10000/vst_agb_kg$sampledAreaM2), 
                                 digits = 4)
    
    #   Categorize individualIDs based on their changes (or not) in plantStatus2
    input_to_transitions <- vst_agb_kg %>% 
      dplyr::select("plot_eventID",
                    "siteID",
                    "plotID",
                    "individualID",
                    "taxonID",
                    "plantStatus2",
                    "year")
    
    input_to_transitions <- input_to_transitions %>% dplyr::distinct(.data$individualID, .data$taxonID, .data$year, .data$plantStatus2, .keep_all = TRUE) 
    input_to_transitions <- input_to_transitions[order(input_to_transitions$year),]
    
    yearList <- unique(sort(vst_agb_kg$year)) # sort list of years
    
    transitions <- tidyr::pivot_wider(input_to_transitions, id_cols = c("siteID", "plotID", "individualID", "taxonID"), names_from = "year", names_prefix = "status_", values_from = "plantStatus2", values_fn = list)  
    
    transitions[transitions == '", "'] <- '""' # Remove the comma. Otherwise one or more of the following lines may give error about converting list to character.
    transitions[transitions == '("Live" "Live")'] <- "Live"
    transitions[transitions == '("Dead_or_Lost" "Dead_or_Lost")'] <- "Dead_or_Lost"
    transitions[transitions == 'c("Dead_or_Lost" "Live")'] <- "Live"
    transitions[transitions == '("Live" "Dead_or_Lost")'] <- "Live"
    transitions[transitions == '("Dead_or_Lost" "Live" "Live")'] <- "Live"
    
    for(i in 2:length(yearList)){
      column_name_prev <- paste0("status_", yearList[i-1])
      column_name <- paste0("status_", yearList[i])
      transitionType_column_name <- paste0("transitionType_", yearList[i])
      transitions <- transitions %>% dplyr::mutate(!!transitionType_column_name := dplyr::case_when(
        (!!sym(column_name)) == 'Dead_or_Lost' & !!sym(column_name_prev) == 'Live' ~ 'mortality',
      ))
    }
    
    mortality <- merge(vst_agb_kg, transitions, by=c("plotID", "siteID", "individualID", "taxonID"), all.x=TRUE)
    mortality$mortality_Mgha <- NA
    
    # if transitionType for a given year is "mortality" then assign a mortality value based on the biomass at the PREVIOUS year
    for(i in 2:length(yearList)){
      year_previous <- yearList[i-1]
      column_name <- paste0("transitionType_", yearList[i])
      mortality <- mortality %>% dplyr::mutate(mortality_Mgha = dplyr::case_when(
        (!!sym(column_name)) == 'mortality' & yearList[i] == year_previous ~ .data$agb_Mgha, TRUE ~ .data$mortality_Mgha
      ))
    }
    
    mortality$year <- as.numeric(mortality$year + 1)
    
    plot_mortality <- mortality %>% dplyr::group_by(.data$siteID, .data$plotID, .data$taxonID, .data$year) %>% dplyr::summarise(Mgha_mortality = sum(.data$mortality_Mgha, na.rm = TRUE)) 
  } else {
    plot_mortality <- data.frame(siteID = character(), plotID = character(), taxonID = character(), year = character(), Mgha_mortality = numeric()) # create placeholder if vst_agb_kg is empty
  }
  
  vst_ANPP_plot_w_taxa_2 <- merge (vst_increment_long, plot_mortality, by=c("siteID","plotID","taxonID","year"), all.x=TRUE )
  vst_ANPP_plot_w_taxa_2$year <- as.numeric(vst_ANPP_plot_w_taxa_2$year)
  vst_ANPP_plot_w_taxa_2 <- vst_ANPP_plot_w_taxa_2 %>% dplyr::filter(!is.na(.data$Mgha_increment) ) # remove records with NA increment; this line is VERY important - without it ~75% of lines have NAs for both increment and mortality, which then turn into false zeros during group_by
  vst_ANPP_plot_w_taxa_2$Mgha_mortality <- dplyr::if_else(is.na(vst_ANPP_plot_w_taxa_2$Mgha_mortality) & !is.na(vst_ANPP_plot_w_taxa_2$Mgha_increment), 0, vst_ANPP_plot_w_taxa_2$Mgha_mortality, vst_ANPP_plot_w_taxa_2$Mgha_mortality)
  vst_ANPP_plot_w_taxa_2 <- vst_ANPP_plot_w_taxa_2 %>% dplyr::group_by(.data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, .data$year) %>%  dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  vst_ANPP_plot_w_taxa_2$woodANPP_Mghayr <- vst_ANPP_plot_w_taxa_2$Mgha_increment + vst_ANPP_plot_w_taxa_2$Mgha_mortality
  
  vst_ANPP_plot_2 <- vst_ANPP_plot_w_taxa_2 %>% dplyr::group_by(.data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$year) %>% dplyr::summarise(woodANPP_Mghayr = round(sum(.data$woodANPP_Mghayr, na.rm = TRUE),3)) %>% dplyr::ungroup()
  vst_ANPP_plot_2 <- vst_ANPP_plot_2 %>% dplyr::filter(!is.na(.data$woodANPP_Mghayr)) %>% dplyr::select("siteID","plotID","plotType","year","woodANPP_Mghayr") # remove records with missing productivity
  
  if(nrow(vst_agb_zeros) >0){
    vst_agb_zeros_plot <- vst_agb_zeros
    vst_agb_zeros_plot$eventID <- vst_agb_zeros_plot$plot_eventID <- NULL
    vst_agb_zeros_plot$woodANPP_Mghayr <- 0 
    vst_ANPP_plot_2 <- rbind(vst_ANPP_plot_2, vst_agb_zeros_plot)}
  
  priority_plots_add <- vst_plot_w_0s %>% dplyr::select("plotID", "specificModuleSamplingPriority")
  priority_plots_add <- unique(priority_plots_add)
  vst_ANPP_plot_2 <- merge(vst_ANPP_plot_2, priority_plots_add, by = c("plotID"), all.x=TRUE)
  if(plotType == "tower") {vst_ANPP_plot_2 <- vst_ANPP_plot_2 %>% dplyr::filter(.data$plotType == "tower") } # if arg plotType = "tower" then filter to just tower plots, otherwise keep all plots from input data
  vst_ANPP_plot_2 <- vst_ANPP_plot_2 %>% dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority) # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))
  
  vst_NPP_plot_yearFirst <- vst_ANPP_plot_2 %>% dplyr::group_by(.data$siteID, .data$plotID, .data$plotType) %>% dplyr::summarise(wood_N = dplyr::n(), 
                                                                                                                                 woodANPP_Mghayr_sd = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE), 2), 
                                                                                                                                 woodANPP_Mghayr_se = round((.data$woodANPP_Mghayr_sd / sqrt(.data$wood_N)), 2),  woodANPP_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),4) ) %>% dplyr::mutate(wood_count_type = "years")
  vst_ANPP_site_2 <- vst_ANPP_plot_2 %>% dplyr::group_by(.data$siteID, .data$year) %>% dplyr::summarise(woodPlotNum = dplyr::n(), 
                                                                                                        woodANPPSD_Mghayr = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE),2), 
                                                                                                        woodANPPMean_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),4) ) %>% dplyr::ungroup()
  
  
  if(calcMethod == "approach_1")    {
  
  
 
  ### CALCULATE WOODY BIOMASS INCREMENT AT THE LEVEL OF INDIVIDUALID, AS RECOMMENDED BY TWG MEMBERS (Clark et al 2001 Approach 1) ####
  
  print("Calculating woody increment productivity at the level of individualID (approach 1) ..... ")
  
  # Filter to year-pairs that were live at earlier years and still live at target year for increment, and year-pairs that were live at earlier years and dead at target year for mortality
  # Calculating in this way accommodated individualIDs that had more than one status for the same year (e.g., if one bole grew and another died)
  # assumption: if data are not from consecutive years then assume equal rate of growth and divide increment and mortality by the number of elapsed years      
  
  if(nrow(vst_agb_kg) > 0){
  
  vst_agb_kg <- vst_agb_kg[order(vst_agb_kg$year),]
  
  vst_agb_plot_list <- unique(vst_agb_kg$plotID); length(vst_agb_plot_list)
  vst_agb_indID_list <- unique(vst_agb_kg$individualID); length(vst_agb_indID_list)
  
  startAGB <- min(vst_agb_kg$year)
  endAGB  <- max(vst_agb_kg$year)
  dummy_rows <- data.frame(plot_eventID = rep(NA, 7), eventID = rep(NA, 7), siteID = rep(NA, 7), plotID = rep(NA, 7), plotType = rep(NA, 7), nlcdClass = rep(NA, 7), individualID = rep(NA, 7),
    taxonID = rep(NA, 7), plantStatus2 = rep(NA, 7), year = ( (startAGB-7):(startAGB-1) ), agb_Mgha = rep(NA, 7)) # placeholder for any years prior to start that are missing
  yearLength <- length(startAGB:endAGB)
  dummy_rows_2 <- data.frame(plot_eventID = rep(NA, yearLength), eventID = rep(NA, yearLength), siteID = rep(NA, yearLength), plotID = rep(NA, yearLength), 
    plotType = rep(NA, yearLength), nlcdClass = rep(NA, yearLength), individualID = rep(NA, yearLength),
    taxonID = rep(NA, yearLength), plantStatus2 = rep(NA, yearLength), year = ( startAGB:endAGB ), agb_Mgha = rep(NA, yearLength)) # placeholder for any years within start to end year range that are missing
  dummy_rows <- rbind(dummy_rows, dummy_rows_2)
  dummy_rows <- dummy_rows[!dummy_rows$year %in% unique(vst_agb_kg$year),] # remove the years that ARE in the data
  
  
  increment_all = data.frame()
  increment_qf_all = data.frame()
  endYear <- as.numeric((startAGB) : endAGB )
  for(i in 2:length(endYear)){
  transitions <- vst_agb_kg %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, 
            .data$individualID,  .data$plantStatus2, .data$year) %>% dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE)) 
  # some individualIDs are represented in multiple growthForms (e.g. sapling and single bole tree): This sums the growthForms
  
  transitions$keep <- dplyr::if_else(transitions$year < endYear[i] & transitions$plantStatus2 == "Live", "keep","discard","discard")
    transitions$keep <- dplyr::if_else(transitions$year == endYear[i] & !is.na(transitions$plantStatus2), "keep",transitions$keep,transitions$keep)
    transitions <- transitions %>% dplyr::filter(.data$keep == "keep") %>% dplyr::select(-"keep") # remove records that aren't live in earlier years 
  increment <- transitions %>% dplyr::filter(!(.data$year == endYear[i] & .data$plantStatus2 == "Dead_or_Lost") ) # remove records that are dead in end year
    increment <- rbind(increment, dummy_rows) # only year populated, workaround to get blank columns for all years
    increment <- increment[increment$year <= endYear[i],] 
    increment$year <- abs(increment$year - endYear[i])
    increment <- tidyr::pivot_wider(increment, id_cols = c("siteID", "plotID", "individualID", "taxonID"), names_from = "year", names_prefix = "agb_Mgha_", values_from = "agb_Mgha")
    increment <- increment %>% dplyr::filter(!is.na(.data$plotID) ) # remove artifact row with NAs from dummy row
    increment$increment1 <- increment$agb_Mgha_0 - increment$agb_Mgha_1
      increment_stats <- increment %>% dplyr::group_by(.data$siteID) %>% dplyr::filter(!is.na(.data$increment1)) %>% 
        dplyr::summarise("increment1_n" = dplyr::n(), "increment1_sd" = round(stats::sd(.data$increment1, na.rm = TRUE),3), increment1_se = round(.data$increment1_sd / sqrt(.data$increment1_n), 3), increment1_mn = round(mean(.data$increment1, na.rm = TRUE),3),
         "increment1_firstQuart" = stats::quantile(.data$increment1, probs = c(0.25), na.rm=TRUE), "increment1_thirdQuart" = stats::quantile(.data$increment1, probs = c(0.75), na.rm=TRUE), increment1_IQR = .data$increment1_thirdQuart - .data$increment1_firstQuart)
  increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment1_IQRqf <- dplyr::if_else((increment$increment1 < increment$increment1_firstQuart-(increment$increment1_IQR * outlier)) | (increment$increment1 > increment$increment1_thirdQuart+(increment$increment1_IQR * outlier)  ), "flag","ok","ok")
    increment$increment1_SDqf <- dplyr::if_else(abs(increment$increment1 - increment$increment1_mn) > (increment$increment1_sd * outlier), "flag","ok","ok")
  increment$increment2 <- (increment$agb_Mgha_0 - increment$agb_Mgha_2)/2
    increment_stats <- increment %>% dplyr::group_by(.data$siteID) %>% dplyr::filter(!is.na(.data$increment2)) %>% 
      dplyr::summarise(increment2_n = dplyr::n(), increment2_sd = round(stats::sd(.data$increment2, na.rm = TRUE),3), increment2_se = round(.data$increment2_sd / sqrt(.data$increment2_n), 3), increment2_mn = round(mean(.data$increment2, na.rm = TRUE),3),
         increment2_firstQuart = stats::quantile(.data$increment2, probs = c(0.25), na.rm=TRUE), increment2_thirdQuart = stats::quantile(.data$increment2, probs = c(0.75), na.rm=TRUE), increment2_IQR = .data$increment2_thirdQuart - .data$increment2_firstQuart)
  increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment2_IQRqf <- dplyr::if_else((increment$increment2 < increment$increment2_firstQuart-(increment$increment2_IQR * outlier)) | (increment$increment2 > increment$increment2_thirdQuart+(increment$increment2_IQR * outlier)  ), "flag","ok","ok")
    increment$increment2_SDqf <- dplyr::if_else(abs(increment$increment2 - increment$increment2_mn) > (increment$increment2_sd * outlier), "flag","ok","ok")
  increment$increment3 <- (increment$agb_Mgha_0 - increment$agb_Mgha_3)/3
    increment_stats <- increment %>% dplyr::group_by(.data$siteID) %>% dplyr::filter(!is.na(.data$increment3)) %>% 
      dplyr::summarise(increment3_n = dplyr::n(), increment3_sd = round(stats::sd(.data$increment3, na.rm = TRUE),3), increment3_se = round(.data$increment3_sd / sqrt(.data$increment3_n), 3), increment3_mn = round(mean(.data$increment3, na.rm = TRUE),3),
         increment3_firstQuart = stats::quantile(.data$increment3, probs = c(0.25), na.rm=TRUE), increment3_thirdQuart = stats::quantile(.data$increment3, probs = c(0.75), na.rm=TRUE), increment3_IQR = .data$increment3_thirdQuart - .data$increment3_firstQuart)
  increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment3_IQRqf <- dplyr::if_else((increment$increment3 < increment$increment3_firstQuart-(increment$increment3_IQR * outlier)) | (increment$increment3 > increment$increment3_thirdQuart+(increment$increment3_IQR * outlier)  ), "flag","ok","ok")
    increment$increment3_SDqf <- dplyr::if_else(abs(increment$increment3 - increment$increment3_mn) > (increment$increment3_sd * outlier), "flag","ok","ok")
  increment$increment4 <- (increment$agb_Mgha_0 - increment$agb_Mgha_4)/4
    increment_stats <- increment %>% dplyr::group_by(.data$siteID) %>% dplyr::filter(!is.na(.data$increment4)) %>% 
      dplyr::summarise(increment4_n = dplyr::n(), increment4_sd = round(stats::sd(.data$increment4, na.rm = TRUE),3), increment4_se = round(.data$increment4_sd / sqrt(.data$increment4_n), 3), increment4_mn = round(mean(.data$increment4, na.rm = TRUE),3),
         increment4_firstQuart = stats::quantile(.data$increment4, probs = c(0.25), na.rm=TRUE), increment4_thirdQuart = stats::quantile(.data$increment4, probs = c(0.75), na.rm=TRUE), increment4_IQR = .data$increment4_thirdQuart - .data$increment4_firstQuart)
  increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment4_IQRqf <- dplyr::if_else((increment$increment4 < increment$increment4_firstQuart-(increment$increment4_IQR * outlier)) | (increment$increment4 > increment$increment4_thirdQuart+(increment$increment4_IQR * outlier)  ), "flag","ok","ok")
    increment$increment4_SDqf <- dplyr::if_else(abs(increment$increment4 - increment$increment4_mn) > (increment$increment4_sd * outlier), "flag","ok","ok")
  increment$increment5 <- (increment$agb_Mgha_0 - increment$agb_Mgha_5)/5
    increment_stats <- increment %>% dplyr::group_by(.data$siteID) %>% dplyr::filter(!is.na(.data$increment5)) %>% 
      dplyr::summarise(increment5_n = dplyr::n(), increment5_sd = round(stats::sd(.data$increment5, na.rm = TRUE),3), increment5_se = round(.data$increment5_sd / sqrt(.data$increment5_n), 3), increment5_mn = round(mean(.data$increment5, na.rm = TRUE),3),
        increment5_firstQuart = stats::quantile(.data$increment5, probs = c(0.25), na.rm=TRUE), increment5_thirdQuart = stats::quantile(.data$increment5, probs = c(0.75), na.rm=TRUE), increment5_IQR = .data$increment5_thirdQuart - .data$increment5_firstQuart)
  increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment5_IQRqf <- dplyr::if_else((increment$increment5 < increment$increment5_firstQuart-(increment$increment5_IQR * outlier)) | (increment$increment5 > increment$increment5_thirdQuart+(increment$increment5_IQR * outlier)  ), "flag","ok","ok")
    increment$increment5_SDqf <- dplyr::if_else(abs(increment$increment5 - increment$increment5_mn) > (increment$increment5_sd * outlier), "flag","ok","ok")
  increment$increment6 <- (increment$agb_Mgha_0 - increment$agb_Mgha_6)/6
    increment_stats <- increment %>% dplyr::group_by(.data$siteID) %>% dplyr::filter(!is.na(.data$increment6)) %>% 
      dplyr::summarise(increment6_n = dplyr::n(), increment6_sd = round(stats::sd(.data$increment6, na.rm = TRUE),3), increment6_se = round(.data$increment6_sd / sqrt(.data$increment6_n), 3), increment6_mn = round(mean(.data$increment6, na.rm = TRUE),3),
        increment6_firstQuart = stats::quantile(.data$increment6, probs = c(0.25), na.rm=TRUE), increment6_thirdQuart = stats::quantile(.data$increment6, probs = c(0.75), na.rm=TRUE), increment6_IQR = .data$increment6_thirdQuart - .data$increment6_firstQuart)
  increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment6_IQRqf <- dplyr::if_else((increment$increment6 < increment$increment6_firstQuart-(increment$increment6_IQR * outlier)) | (increment$increment6 > increment$increment6_thirdQuart+(increment$increment6_IQR * outlier)  ), "flag","ok","ok")
    increment$increment6_SDqf <- dplyr::if_else(abs(increment$increment6 - increment$increment6_mn) > (increment$increment6_sd * outlier), "flag","ok","ok")
  increment$increment7 <- (increment$agb_Mgha_0 - increment$agb_Mgha_7)/7
    increment_stats <- increment %>% dplyr::group_by(.data$siteID) %>% dplyr::filter(!is.na(.data$increment7)) %>% 
      dplyr::summarise(increment7_n = dplyr::n(), increment7_sd = round(stats::sd(.data$increment7, na.rm = TRUE),3), increment7_se = round(.data$increment7_sd / sqrt(.data$increment7_n), 3), increment7_mn = round(mean(.data$increment7, na.rm = TRUE),3),
        increment7_firstQuart = stats::quantile(.data$increment7, probs = c(0.25), na.rm=TRUE), increment7_thirdQuart = stats::quantile(.data$increment7, probs = c(0.75), na.rm=TRUE), increment7_IQR = .data$increment7_thirdQuart - .data$increment7_firstQuart)
  increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment7_IQRqf <- dplyr::if_else((increment$increment7 < increment$increment7_firstQuart-(increment$increment7_IQR * outlier)) | (increment$increment7 > increment$increment7_thirdQuart+(increment$increment7_IQR * outlier)  ), "flag","ok","ok")
    increment$increment7_SDqf <- dplyr::if_else(abs(increment$increment7 - increment$increment7_mn) > (increment$increment7_sd * outlier), "flag","ok","ok")
  increment$Mghayr <- ifelse(!is.na(increment$increment7), increment$increment7, NA)
    increment$bestIncrement <- ifelse(!is.na(increment$increment7), 7, NA)
  increment$Mghayr <- ifelse(!is.na(increment$increment6), increment$increment6, increment$Mghayr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment6), 6, increment$bestIncrement)
  increment$Mghayr <- ifelse(!is.na(increment$increment5), increment$increment5, increment$Mghayr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment5), 5, increment$bestIncrement)
  increment$Mghayr <- ifelse(!is.na(increment$increment4), increment$increment4, increment$Mghayr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment4), 4, increment$bestIncrement)
  increment$Mghayr <- ifelse(!is.na(increment$increment3), increment$increment3, increment$Mghayr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment3), 3, increment$bestIncrement)
  increment$Mghayr <- ifelse(!is.na(increment$increment2), increment$increment2, increment$Mghayr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment2), 2, increment$bestIncrement)
  increment$Mghayr <- ifelse(!is.na(increment$increment1), increment$increment1, increment$Mghayr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment1), 1, increment$bestIncrement)
  increment <- increment %>% dplyr::filter(!is.na(.data$Mghayr)) %>% dplyr::mutate(endYear = endYear[i])   # remove missing increment values and add endYear column
  increment <- dplyr::rename(increment, "Mghayr_inc" = "Mghayr", "bestIncrement_inc" = "bestIncrement")
  increment_all = dplyr::bind_rows(increment_all, increment) %>% dplyr::mutate(outlier_threshold = paste0(outlier,"_",outlierType))
  }
  
  if(nrow(increment_all) > 0){
  
  if(outlierType == "SD") {
  increment_ok <- increment_all %>% dplyr::filter("increment1_SDqf" != "flag" & "increment2_SDqf" != "flag"  & "increment3_SDqf" != "flag" & "increment4_SDqf" != "flag" & "increment5_SDqf" != "flag" & "increment6_SDqf" != "flag" & "increment7_SDqf" != "flag") # remove increments that are flagged as being excessive for the specified timeframes
  increment_outlier <- increment_all %>% dplyr::filter("increment1_SDqf" == "flag" | "increment2_SDqf" == "flag"  | "increment3_SDqf" == "flag" | "increment4_SDqf" == "flag" | "increment5_SDqf" == "flag" | "increment6_SDqf" == "flag" | "increment7_SDqf" == "flag") # increments that are flagged as being excessive for the specified timeframes
  increment_outlier <- increment_outlier %>% dplyr::select("outlier_threshold", "siteID", "plotID", "individualID", "taxonID", "Mghayr_inc", "bestIncrement_inc", "endYear")
  } 
    
  if(outlierType == "IQR") {
  increment_ok <- increment_all %>% dplyr::filter("increment1_IQRqf" != "flag" & "increment2_IQRqf" != "flag"  & "increment3_IQRqf" != "flag" & "increment4_IQRqf" != "flag" & "increment5_IQRqf" != "flag" & "increment6_IQRqf" != "flag" & "increment7_IQRqf" != "flag") # remove increments that are flagged as being excessive for the specified timeframes
  increment_outlier <- increment_all %>% dplyr::filter("increment1_IQRqf" == "flag" | "increment2_IQRqf" == "flag"  | "increment3_IQRqf" == "flag" | "increment4_IQRqf" == "flag" | "increment5_IQRqf" == "flag" | "increment6_IQRqf" == "flag" | "increment7_IQRqf" == "flag") # increments that are flagged as being excessive for the specified timeframes
  increment_outlier <- increment_outlier %>% dplyr::select("outlier_threshold", "siteID", "plotID", "individualID", "taxonID", "Mghayr_inc", "bestIncrement_inc", "endYear")
  }
  
  outlier_count <- length(increment_outlier$individualID)
  increment_all_count <- length(increment_all$individualID)
  percent_outliers <- round(100 * outlier_count / increment_all_count, 1)
  
  print(paste0("Note: The chosen outlier criteria removed ", outlier_count, " records (", percent_outliers, "% of all records)"))
  
  increment_ok <- increment_ok %>% dplyr::select("outlier_threshold", "siteID", "plotID", "individualID", "taxonID", "Mghayr_inc", "bestIncrement_inc", "endYear")
    increment_ok <- increment_ok %>% dplyr::filter(.data$bestIncrement_inc == 1 | .data$bestIncrement_inc == 2 | .data$bestIncrement_inc == 3)  #(.data$bestIncrement_inc == 5 & (siteID =="JORN" | siteID =="MOAB" | siteID =="ONAQ" | siteID =="SRER") ) )
    # remove increments without the allowed increment timeframe; we only need a single year increment for the 5 priority tower plots, but the 2 year increment means we don't lose 2 years worth of increment from missing just 1 measurement of an individual,
                                         
  increment_indID_list <- unique(increment_ok$individualID); length(increment_indID_list)
  increment_eventID_list <- unique(paste0(increment_ok$plotID,"_vst_",substr(increment_ok$plotID, 1,4), "_", increment_ok$endYear))
  
  print("Finish calculating woody productivity at the level of individualID (approach 1) ..... ")
  
  #product_all <- merge(increment_all, recruitment_all, by=c("siteID", "plotID", "individualID", "taxonID","endYear"), all=TRUE)  # recruitment not yet incorporated since sensitive to large individ. missed in previous bout
  #product_all <- product_all %>% dplyr::mutate_all(~replace(., is.na(.), 0)) %>% dplyr::mutate(wood_ANPP_Mghayr = .data$Mghayr_inc + .data$Mghayr_rec)  
  product_all <- increment_ok        # placeholder until incorporate recruitment
  product_all <- dplyr::rename(product_all, "woodANPP_Mghayr" = "Mghayr_inc")     
  product_all <- dplyr::rename(product_all, "year" = "endYear")
  productivity <- merge(product_all, plotType_df, by = "plotID", all.x = T)
  
  # sum the individualIDs by plot and taxonID
  vst_ANPP_plot_w_taxa <- productivity %>% dplyr::select(-"bestIncrement_inc") %>% dplyr::group_by(.data$outlier_threshold, .data$siteID, .data$plotID, .data$plotType, .data$year, .data$taxonID) %>%  dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  
  # sum the individualIDs by plot
  vst_ANPP_plot <- vst_ANPP_plot_w_taxa %>% dplyr::group_by(.data$outlier_threshold, .data$siteID, .data$plotID, .data$plotType, .data$year) %>%  dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% dplyr::ungroup()
  
  vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(!is.na(.data$woodANPP_Mghayr)) %>% dplyr::select("outlier_threshold","siteID","plotID","plotType","year","woodANPP_Mghayr") # remove records with missing productivity
  
  # ADD PLOTS WITH 0 BIOMASS AND HERB BIOMASS 
  
  if(nrow(vst_agb_zeros) >0){
  vst_agb_zeros_ind <- vst_agb_zeros
    vst_agb_zeros_ind$eventID <- vst_agb_zeros_ind$plot_eventID <- NULL
    vst_agb_zeros_ind$outlier_threshold <- paste0(outlier,"_",outlierType)
    vst_agb_zeros_ind$woodANPP_Mghayr <- 0 
    vst_ANPP_plot <- rbind(vst_ANPP_plot, vst_agb_zeros_ind)}
  
  vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(.data$year >= start) # make sure that records from before the start year have been removed
  vst_ANPP_plot <- merge(vst_ANPP_plot, priority_plots_add, by = c("plotID"), all.x=TRUE)
  if(plotType == "tower") {vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(.data$plotType == "tower")} # if plotType argument is "tower" then remove distributed plots
  if(!is.na(plotPriority)) {vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(.data$specificModuleSamplingPriority <= 5)} # as specified in argument to function, remove lower priority plots that aren't required to be sampled every year
  
  vst_ANPP_site <- vst_ANPP_plot %>% dplyr::group_by(.data$outlier_threshold, .data$siteID, .data$year) %>% dplyr::summarise(woodPlotNum = dplyr::n(), 
          woodANPPSD_Mghayr = round(stats::sd(.data$woodANPP_Mghayr, na.rm = TRUE),2), 
          woodANPPMean_Mghayr = round(mean(.data$woodANPP_Mghayr, na.rm = TRUE),4) ) %>% dplyr::ungroup()
  
  print("Returning productivity summary data frames as a list object, calculated using approach 1  ..... ")
  } else {print("Current data subset has no individualID that has been measured more than once, so woody increment and productivity can not be calculated using approach 1.")}
  
  } else {print("Current data subset has no biomass, so woody increment and productivity can not be calculated using approach 1.")}
  
  output.list <- list(
    increment_all = increment_all,
    increment_outlier = increment_outlier,
    vst_ANPP_plot_w_taxa = vst_ANPP_plot_w_taxa,
    vst_ANPP_plot = vst_ANPP_plot,
    vst_ANPP_site = vst_ANPP_site
  )
  }
  
  if(calcMethod == "approach_2")    {
  
  print("Returning productivity summary data frames as a list object, calculated using approach 2  ..... ")
  
  output.list <- list(
    vst_ANPP_plot_w_taxa_2 = vst_ANPP_plot_w_taxa_2,
    vst_ANPP_plot_2 = vst_ANPP_plot_2,
    vst_ANPP_site_2 = vst_ANPP_site_2
  )
  }
  return(output.list)
  
}
