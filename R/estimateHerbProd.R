##############################################################################################
#' @title Estimate NEON plot and site-level ANPP (Aboveground Net Primary Productivity) contributed by herbaceous vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Using inputs from companion scaleHerbMass function calculate herbaceous aboveground productivity.
#' The data input is a list of biomass dataframes created by the companion scaleHerbMass function (e.g. scaleHerbMassOutputs).
#' 
#' @details A list of herbaceous biomass dataframes created by the companion scaleHerbMass function is read in and aboveground net primary 
#' productivity (ANPP) is calculated for NEON "Herbaceous clip harvest" (DP1.10023.001) data. Plots can be filtered to more frequently 
#' sampled plots using plotType and plotPriority arguments. Where grazing exclosures have been established consumption is calculated as
#' biomass within exclosures minus biomass outside of exclosures. Consumption at each sampling bout is summed and then added to the 
#' standing biomass of the last bout of the season for an estimate of herbaceous ANPP. Where grazing exclosures have not been established
#' the estimate of herbaceous ANPP is simply the standing biomass of the last bout of the season.

#' @param input  Specify a loaded R list object (e.g. scaleHerbMassOutputs) that was produced by companion scaleHerbMass function. [character]
#' @param plotType Optional filter based on NEON plot type. Defaults to "tower" plots, which are sampled annually. Otherwise "distributed" plots are examined also, if included in the input .rds. [character]
#' @param plotPriority NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is 5. [numeric]
#' 
#' @return This function returns a list that includes productivity summary data frames.
#' 'herb_ANPP_plot' herbaceous ANPP for each plot by year combination (units are Megagrams per hectare per year).
#' 'herb_ANPP_site' herbaceous ANPP for each site by year combination (units are Megagrams per hectare per year).
#' 
#' @examples
#' \dontrun{
#' # If list is not in memory, load herbaceous biomass list of dataframes from local file:
#' load('scaleHerbMassOutputs.rds') # load list of dataframes created by scaleHerbMass function

#' # example with arguments at default values
#'estimateHerbProdOutputs <-estimateHerbProd(input = scaleHerbMassOutputs)
#'                       
#' # example specifying many non-default arguments
#'estimateHerbProdOutputs <-estimateHerbProd(input = scaleHerbMassOutputs, 
#'                plotType = "all", plotPriority = 50)
#' 
#' list2env(estimateHerbProdOutputs ,.GlobalEnv) # unlist all data frames for easier viewing
#' saveRDS(estimateHerbProdOutputs, 'estimateHerbProdOutputs.rds') # save output locally
#' }

# changelog and author contributions / copyrights
#   Samuel M Simkin (2021-03-30)  original creation
#   Samuel M Simkin (2025-01-15)  revised
#################################################################################  

estimateHerbProd = function(
                         input,
                         plotType = "tower",
                         plotPriority = 5
                         ) {
  
options(dplyr.summarise.inform = FALSE)

if(!methods::is(input, class = "list" )){
  stop("The input argument is expected to be a list. A character, data.frame, or NA argument is not allowed.")
  }

list2env(input ,.GlobalEnv)
hbp_agb <- input$hbp_agb
hbp_plot <- input$hbp_plot


listExpNames_Hbp <- c("hbp_agb", "hbp_plot", "hbp_site")
if (length(setdiff(listExpNames_Hbp, names(input))) > 0) {
      stop(glue::glue("Required tables missing from input list:",
                      '{paste(setdiff(listExpNames_Hbp, names(input)), collapse = ", ")}',
                      .sep = " "))
} 


# Error if invalid plotType option selected
  if(plotType != "tower" & plotType != "all"){
    stop("The only valid plotType options are 'tower' or 'all'.")
  }  
  
# Error if invalid plotPriority option selected
  if(plotPriority < 1){
    stop("The minimum plotPriority value is 1, and 5 or greater is the recommended default.")
  }  
  

### Verify input tables contain required columns and data ####


### Verify user-supplied hbp_agb contains required data
#   Check for required columns
  hbp_agb_ExpCols <- c("domainID","siteID", "plotID","clipID","eventID","year","nlcdClass","plotType","plotSize","data_prod","bout",
                  "sampleID","clipArea","exclosure","peak","dryMass_gm2_AllHerbaceousPlants")
  if (length(setdiff(hbp_agb_ExpCols, colnames(hbp_agb))) > 0) {
    stop(glue::glue("Required columns missing from 'hbp_agb':", '{paste(setdiff(hbp_agb_ExpCols, colnames(hbp_agb)), collapse = ", ")}',
                    .sep = " "))
  }
  
#   Check for data
  if (nrow(hbp_agb) == 0) {
    stop(glue::glue("Table 'hbp_agb' has no data."))
  }
  
### Verify user-supplied hbp_plot contains required data
#   Check for required columns
  hbp_plot_ExpCols <- c("siteID","plotID","year","nlcdClass","herbPeakMassTotal_Mgha")
  
  if (length(setdiff(hbp_plot_ExpCols, colnames(hbp_plot))) > 0) {
    stop(glue::glue("Required columns missing from 'hbp_plot':", '{paste(setdiff(hbp_plot_ExpCols, colnames(hbp_plot)), collapse = ", ")}',
                    .sep = " "))
  }
  
#   Check for data
  if (nrow(hbp_plot) == 0) {
    stop(glue::glue("Table 'hbp_plot' has no data."))
  }


start <- min(hbp_plot$year)
end  <- max(hbp_plot$year)


###### Error if not at least 2 years of data
  if(as.numeric(end) - as.numeric(start) < 1){
    print("At least 2 years of data are needed to calculate productivity (more for plots sampled less frequently than annually). Current dataset only has biomass data from: ")
    print(unique(hbp_plot$year))
    stop( )
  }

print("Summarizing above-ground herbaceous productivity  ..... ")

### productivity by site and year
# hbp_event_means <- hbp_agb %>% 
#   dplyr::group_by(.data$domainID, .data$siteID, .data$plotID, .data$plotType, .data$eventID, .data$year, .data$bout, .data$nlcdClass, .data$exclosure, .data$peak) %>% 
#   dplyr::summarise(mean_herb_gm2 = mean(.data$dryMass_gm2_AllHerbaceousPlants, na.rm = TRUE)) # calc the mean biomass of the individual plots within eventID
# hbp_event_means$exclosure <- ifelse(is.na(hbp_event_means$exclosure), "N", hbp_event_means$exclosure)

if(nrow(hbp_agb %>% dplyr::filter(.data$exclosure == "Y")) > 0 ) {
hbp_agb_long <- hbp_agb %>% tidyr::pivot_longer(cols=c("dryMass_gm2_AllHerbaceousPlants", "dryMass_gm2_CoolSeasonGraminoids", "dryMass_gm2_WoodyStemmedPlants", "dryMass_gm2_WarmSeasonGraminoids",
        "dryMass_gm2_NFixingPlants","dryMass_gm2_AnnualAndPerennialForbs"), names_to="herbGroup", names_prefix = "dryMass_gm2_", values_to="gm2")
  
consumption <- hbp_agb_long %>% 
  tidyr::pivot_wider(id_cols = c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "subplotID", "year", "peak", "herbGroup", "eventID", "bout"), names_from = "exclosure",
                     values_from = "gm2", names_prefix = "gm2_") %>% 
     # convert from long to wide format (exclosure Y and N in same row)
    dplyr::mutate(gm2_C = .data$gm2_Y - .data$gm2_N)    # gm2_C is consumption, calculated as biomass in exclosure (Y) minus biomass outside of exclosure (N)
} else {
consumption <- hbp_agb_long %>% 
     dplyr::mutate(gm2_C = 0) %>% # if no exclosure then set consumption to 0
     dplyr::rename("gm2_N" = "gm2")
}

# all the same grouping variables as in the pivot_longer except subplotID, so the mean of subplots (typically 2) within plot is calculated
plot_consumption <- consumption %>% dplyr::group_by(.data$domainID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$year, .data$peak, .data$herbGroup, .data$eventID, .data$bout) %>%
  dplyr::summarise(gm2_Y = mean(stats::na.omit(.data$gm2_Y)), gm2_N = mean(stats::na.omit(.data$gm2_N)), gm2_C = mean(stats::na.omit(.data$gm2_C)) )
    
    
# all the same grouping variables used to create plot_consumption except for eventID and bout, allowing us to count the number of bouts or extract the values of the last bout
herb_ANPP_total <- plot_consumption %>% dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>% dplyr::group_by(.data$domainID, .data$siteID, .data$plotID, .data$nlcdClass, .data$year, .data$herbGroup) %>%
  dplyr::summarise(
    totalConsumption_gm2 = sum(stats::na.omit(.data$gm2_C)),
    n_bouts_used_for_consumption = length(stats::na.omit(.data$gm2_C)),
    last_bout = dplyr::last(.data$bout),
    "last_bout_mean_herb_gm2" = dplyr::last(.data$gm2_N),
    "herbANPP_gm2yr" = .data$last_bout_mean_herb_gm2 + .data$totalConsumption_gm2  # add standing biomass and, if applicable, consumption calculated from exclosures
  ) %>% dplyr::ungroup()
herb_ANPP_total$bout <- "allBouts"

herb_ANPP_herbGroup <- plot_consumption %>% dplyr::filter(.data$herbGroup != "AllHerbaceousPlants" & .data$peak =="atPeak") %>% dplyr::group_by(.data$domainID, .data$siteID, .data$plotID, .data$nlcdClass, .data$year, .data$herbGroup) %>%
  dplyr::summarise(
    totalConsumption_gm2 = sum(stats::na.omit(.data$gm2_C)),
    n_bouts_used_for_consumption = length(stats::na.omit(.data$gm2_C)),
    last_bout = dplyr::last(.data$bout),
    "last_bout_mean_herb_gm2" = dplyr::last(.data$gm2_N),
    "herbANPP_gm2yr" = .data$last_bout_mean_herb_gm2 + .data$totalConsumption_gm2  # add standing biomass and, if applicable, consumption calculated from exclosures
  ) %>% dplyr::ungroup() # important note herbGroup stats almost always based on just the one peak bout
herb_ANPP_herbGroup$bout <- "peakBout"

herb_ANPP <- rbind(herb_ANPP_total, herb_ANPP_herbGroup)

herb_ANPP$year <- as.numeric(herb_ANPP$year)
priority_plots <- priority_plots # load into environment
herb_ANPP <- merge(herb_ANPP, priority_plots, by = "plotID", all.x = TRUE)

if(plotType == "tower") {herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$plotType == "tower")} # if plotType argument is "tower" then remove distributed plots
if(!is.na(plotPriority)) {herb_ANPP <- herb_ANPP %>% dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)} # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))
herb_ANPP <- herb_ANPP %>% dplyr::filter(!is.na(.data$herbANPP_gm2yr) ) # remove records that are missing productivity 

herb_ANPP <- herb_ANPP %>% dplyr::relocate(dplyr::any_of(c("plotType", "specificModuleSamplingPriority")), .after = "nlcdClass")
herb_ANPP <- herb_ANPP %>% dplyr::relocate(dplyr::any_of(c("bout", "n_bouts_used_for_consumption")), .after = "herbGroup")
herb_ANPP$herbANPP_Mghayr <- round(herb_ANPP$herbANPP_gm2yr * 10000 * 0.000001,3) # convert g/m2 to Mg/ha ;   g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha

herb_ANPP_site <- herb_ANPP %>% dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>% dplyr::group_by(.data$siteID, .data$year, .data$herbGroup) %>% dplyr::summarise(herbPlotNum = dplyr::n(), 
          herbANPPSD_gm2yr = round(stats::sd(.data$herbANPP_gm2yr, na.rm = TRUE),3), 
          herbANPPMean_gm2yr = round(mean(.data$herbANPP_gm2yr, na.rm = TRUE),3), 
          herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,3),  # convert g/m2 to Mg/ha ;   g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha
          herbANPPMean_Mghayr = round(.data$herbANPPMean_gm2yr * 10000 * 0.000001,3) )

output.list <- list(
   herb_ANPP_plot = herb_ANPP,
   herb_ANPP_site = herb_ANPP_site
   )
 return(output.list)

}

