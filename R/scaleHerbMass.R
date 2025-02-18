#' @title Scale herbaceous biomass by functional group data to mass per area
#' 
#' @author Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Join NEON Herbaceous Clip Harvest data tables (DP1.10023.001) to calculate herbaceous biomass by functional group per unit area as well as total herbaceous biomass per unit area. Biomass outputs can be used in the neonPlants estimateMass() function, and the estimateHerbProd() and estimateProd() productivity functions.
#' 
#' Data inputs are "Herbaceous clip harvest" data (DP1.10023.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#' 
#' @details Input data can be filtered by site, date, plot type, and plot priority. Herbaceous biomass are scaled to an area basis at the hierarchical levels of sampling cell, plot, and site. Input data may be provided either as a list or as individual tables. However, if both list and table inputs are provided at the same time the function will error out. For all output data, columns with the same name as input data have identical units and definitions; where needed, new columns contain new units information.
#' 
#' NEON weighs a minimum of 5% of samples a second time so that data users can estimate the uncertainty associated with different technicians weighing dried herbaceous biomass; QA samples of this nature are identified via qaDryMass == "Y". The function calculates the mean when QA masses exist and any 'remarks' are concatenated. Samples with Sampling Impractical values other than "OK" are removed prior to generating output data.
#' 
#' @param inputDataList A list object comprised of "Herbaceous clip harvest" tables (DP1.10023.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 'inputBout', and 'inputMass' arguments. [list]
#' 
#' @param inputBout The 'hbp_perbout' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputMass The 'hbp_massdata' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param plotType Optional filter based on NEON plot type. Options are "all" (both Tower and Distributed plot types) or "tower". Defaults to "all". [character]
#' 
#' @param plotPriority NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is NA. [integer]
#' 
#' @return A list that includes biomass summary data at multiple scales. Output tables include:
#'   * hbp_agb - Summarizes above-ground herbaceous biomass for each record ("g/m2").
#'   * hbp_plot - Summarizes above-ground herbaceous biomass for each plot by year combination (both "g/m2" and "Mg/ha").
#'   * hbp_site - Summarizes above-ground herbaceous biomass for each site by year combination (both "g/m2" and "Mg/ha").
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
                         plotType = "all",
                         plotPriority = NA) {
  
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
  boutExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "clipArea", "exclosure", "samplingImpractical", "targetTaxaPresent")
  
  if (length(setdiff(boutExpCols, colnames(inputBout))) > 0) {
    stop(glue::glue("Required columns missing from 'inputBout':", '{paste(setdiff(boutExpCols, colnames(inputBout)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(inputBout) == 0) {
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
  if (nrow(inputMass) == 0) {
    stop(glue::glue("Table 'inputMass' has no data."))
  }
  
  
  
  ###  Create 'year' column for grouping output at plot and site scales across data products
  inputBout$year <- as.numeric(substr(inputBout$eventID, 5, 8))
  
  
  
  ### Aggregate the herbaceous data ####
  
  #   Assign 'unknown' to missing herbGroup
  inputMass$herbGroup <- ifelse(is.na(inputMass$herbGroup),
                                "Unknown",
                                inputMass$herbGroup)
  
  #   Filter mass data to retain only qaDryMass == N (to avoid duplicates when there is a qa sample too)
  inputMass <- inputMass %>% 
    dplyr::filter(.data$qaDryMass == 'N' & .data$herbGroup != "Bryophyte") %>% 
    dplyr::select("sampleID", 
                  "subsampleID",
                  "herbGroup",
                  "dryMass")
  
  #   Calculate the average of any duplicates within 'herbGroup'
  inputMass <- inputMass  %>% 
    dplyr::group_by_at(dplyr::vars("sampleID", 
                                   "subsampleID", 
                                   "herbGroup")) %>% 
    dplyr::summarise(dryMass = mean(.data$dryMass),
                     .groups = "drop")
  
  #   Reduce 'hbp_perbout' columns to those needed for join
  inputBout <- inputBout %>% 
    dplyr::select("namedLocation", 
                  "domainID", 
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
                  "sampleID",
                  "clipArea",
                  "exclosure")
  
  #   Join tables and calculate mass per area
  hbp <- dplyr::right_join(inputBout, 
                           inputMass, 
                           by = "sampleID") %>% 
    dplyr::mutate(dryMass_gm2 = .data$dryMass / .data$clipArea)
  
  #   Categorize mass as "peak" biomass or "offPeak"
  hbp$peak <- ifelse(hbp$herbGroup == 'All herbaceous plants',
                     "offPeak",
                     "atPeak")
  
  #   Standardize herbGroups to remove spaces
  hbp$herbGroup <- gsub(pattern = "All herbaceous plants",
                        replacement = "AllHerbaceousPlants",
                        hbp$herbGroup)
  
  hbp$herbGroup <- gsub(pattern = "Cool Season Graminoids",
                        replacement = "CoolSeasonGraminoids",
                        hbp$herbGroup)
  
  hbp$herbGroup <- gsub(pattern = "Woody-stemmed Plants",
                        replacement = "WoodyStemmedPlants",
                        hbp$herbGroup)
  
  hbp$herbGroup <- gsub(pattern = "Warm Season Graminoids",
                        replacement = "WarmSeasonGraminoids",
                        hbp$herbGroup)
  
  hbp$herbGroup <- gsub(pattern = "N-fixing Plants",
                        replacement = "NFixingPlants",
                        hbp$herbGroup)
  
  hbp$herbGroup <- gsub(pattern = "Annual and Perennial Forbs",
                        replacement = "AnnualAndPerennialForbs",
                        hbp$herbGroup)
  
  #   Transpose herbGroup rows into separate columns
  hbp_wide <- tidyr::pivot_wider(data = hbp,
                                 id_cols = c("namedLocation",
                                             "domainID",
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
                                             "sampleID",
                                             "clipArea",
                                             "exclosure",
                                             "peak"), 
                                 names_from = "herbGroup",
                                 names_prefix = "dryMass_gm2_", 
                                 #names_glue = "{herbGroup}_gm2",
                                 values_from = "dryMass_gm2")
  
  #   Aggregate dryMass among herbGroups in peak biomass bouts
  hbp_peak_biomass_herb_groups <- hbp %>% 
    dplyr::filter(.data$herbGroup != 'AllHerbaceousPlants') %>% 
    dplyr::mutate(peak = "atPeak")
  
  hbp_peak_biomass_sum_groups <- hbp_peak_biomass_herb_groups %>% 
    dplyr::group_by_at(dplyr::vars("sampleID")) %>% 
    dplyr::summarise(dryMassSum = sum(.data$dryMass_gm2)) 
  
  #   Populate "All herbaceous plants" column for peak biomass bouts
  hbp2 <- merge(hbp_wide, 
                hbp_peak_biomass_sum_groups, 
                by = "sampleID", 
                all.x = TRUE)
  
  hbp2$dryMass_gm2_AllHerbaceousPlants <- ifelse(is.na(hbp2$dryMass_gm2_AllHerbaceousPlants),
                                                 hbp2$dryMassSum,
                                                 hbp2$dryMass_gm2_AllHerbaceousPlants)
  
  
  
  ### Calculate plot-level means by year ####
  #   Separate "eventID" into components
  hbp_standing_biomass_in_clip_cells <- hbp2 %>% 
    dplyr::select(-"dryMassSum") %>%
    tidyr::separate(col = "eventID", 
                    into = c("data_prod", "year", "siteID2", "bout"),
                    sep = "\\.", 
                    remove = FALSE, 
                    extra = "drop")
  
  hbp_standing_biomass_in_clip_cells$siteID2 <- NULL
  
  #   Group by eventID and average across sampling cells (across plots) within a treatment group (exclosure Y/N)
  #   Possible for consumption to be negative. To get total NPP = last bout Standing biomass + consumption estimated for each time step. 
  
  hbp_agb <- hbp_standing_biomass_in_clip_cells # %>% dplyr::filter(.data$peak == "atPeak")
  
  hbp_plot <- hbp_standing_biomass_in_clip_cells %>% 
    dplyr::filter(.data$peak == 'atPeak' & .data$exclosure == 'N') %>% 
    dplyr::group_by(.data$siteID, 
                    .data$plotID, 
                    .data$year, 
                    .data$nlcdClass) %>% 
    
    #   Calculate average per plot if there are multiple subplots
    dplyr::summarise(
      herbPeakMassTotal_gm2 = round(mean(.data$dryMass_gm2_AllHerbaceousPlants, na.rm = TRUE),
                                    digits = 3),
      herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$dryMass_gm2_CoolSeasonGraminoids, na.rm = TRUE),
                                                   digits = 3),
      herbPeakMassWoodyStemmedPlants_gm2 = round(mean(.data$dryMass_gm2_WoodyStemmedPlants, na.rm = TRUE),
                                                 digits = 3),
      herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$dryMass_gm2_CoolSeasonGraminoids, na.rm = TRUE),
                                                   digits = 3),
      herbPeakMassWarmSeasonGraminoids = round(mean(.data$dryMass_gm2_WarmSeasonGraminoids, na.rm = TRUE),
                                               digits = 3),
      herbPeakMassNFixingPlants_gm2 = round(mean(.data$dryMass_gm2_NFixingPlants, na.rm = TRUE),
                                            digits = 3),
      herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$dryMass_gm2_CoolSeasonGraminoids, na.rm = TRUE),
                                                   digits = 3),
      herbPeakMassAnnualAndPerennialForbs_gm2 = round(mean(.data$dryMass_gm2_AnnualAndPerennialForbs, na.rm = TRUE),
                                                      digits = 3),
      .groups = "drop")
  
  #   Calculate "Mg/ha" for total herbaceous peak biomass; g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
  hbp_plot$herbPeakMassTotal_Mgha <- hbp_plot$herbPeakMassTotal_gm2 * 10000 * 0.000001
  
  
  ##  Filter output according to user-supplied arguments
  #   Load 'priority_plots' data frame into environment from 'data' folder and merge with plot-level data
  priority_plots <- priority_plots
  
  hbp_plot <- merge(hbp_plot,
                    priority_plots, 
                    by = c("plotID"), 
                    all.x = TRUE)
  
  #   Remove 'distributed' plots if focus is on 'tower' plots
  if(plotType == "tower") {
    
    hbp_plot <- hbp_plot %>% 
      dplyr::filter(.data$plotType == "tower")
  }
  
  #   Remove plots that do not meet 'priority' thresholds if 'plotPriority' supplied (default is 5 highest priority plots)
  if(!is.na(plotPriority)) {
    
    hbp_plot <- hbp_plot %>% 
      dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)
  } 
  
  #   Reorder columns and keep latest year
  hbp_plot <- hbp_plot %>% 
    dplyr::relocate("herbPeakMassTotal_gm2", 
                    .before = "herbPeakMassTotal_Mgha")
  
  hbp_plot <- hbp_plot %>% 
    dplyr::relocate(dplyr::any_of(c("plotType", "specificModuleSamplingPriority")), 
                    .after = "nlcdClass")
  
  hbp_plot$year <- as.numeric(hbp_plot$year)
  hbp_plot <- hbp_plot[order(hbp_plot$year), ]
  
  
  
  ### Calculate site-level peak biomass by year ####
  hbp_site <- hbp_plot %>% 
    dplyr::group_by(.data$siteID, 
                    .data$year) %>% 
    dplyr::summarise(herbPlotNum = length(stats::na.omit(.data$herbPeakMassTotal_gm2)),
                     herbPeakMassMean_gm2 = round(mean(.data$herbPeakMassTotal_gm2, na.rm = TRUE),
                                                  digits = 3),
                     herbPeakMassSD_gm2 = round(stats::sd(.data$herbPeakMassTotal_gm2, na.rm = TRUE),
                                                digits = 3),
                     herbPeakMassMean_Mgha = round(mean(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),
                                                   digits = 3),
                     herbPeakMassSD_Mgha = round(stats::sd(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),
                                                 digits = 3),
                     .groups = "drop")
  
  
  
  ### Bundle output as list and return
  output.list <- list(hbp_agb = hbp_agb,
                      hbp_plot = hbp_plot,
                      hbp_site = hbp_site)
  
  return(output.list)
}
