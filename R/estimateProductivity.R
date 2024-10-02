##############################################################################################
#' @title Estimate NEON plot and site-level ANPP (Aboveground Primary Productivity) contributed by woody and herbaceous vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Using inputs from companion estimateAGBiomass function calculate VST woody yearly increment 
#' and mortality, and thereby annual productivity. Optionally, also calculate HBP productivity and add 
#' to VST for a VST + HBP summary.
#' 

#' @param input  Specify a loaded R list object (e.g. estimateBiomassOutputs) that was produced by companion estimateBiomass.R function. [character]
#' @param plotType Optional filter based on NEON plot type. Defaults to "tower" plots, which are sampled annually. Otherwise "distributed" plots are examined also, if included in the input .rds. [character]
#' @param plotPriority NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is 5. [numeric]
#' @param calcMethod Select plot-level (approach 2) or individual-level (approach 1) productivity calculations. The default is "approach_1" [character]
#' @param outlier Specify how much (if any) outlier removal should be performed. The default is 1.5. [numeric]
#' @param outlier_type Specify the type of outlier, either SD (standard deviations) or IQR (interquartile range). The default is "IQR". [character]
#' @param dataProducts Select whether to analyse only woody biomass "Vst" or woody plus herbaceous "VstHbp" [character]
#' 
#' @details Biomass data input files (in loaded R list object) created by the companion estimateBiomass.R function are read in and aboveground net primary 
#' productivity (ANPP) is calculated for woody vegetation (from NEON "Vegetation structure" data product (dpID "DP1.10098.001")). Herbaceous vegetation ANPP 
#' (from NEON "Herbaceous clip harvest (dpID "DP1.10023.001")) is also included if herbaceous data is present in the input file). Both a whole plot 
#' level approach and an individual-level approach calculation method are used, and output from the desired calculation method is returned. The individual-level 
#' approach is currently an underestimate since it includes the growth increment component but not the recruitment component. The recruitment component is 
#' currently very sensitive to stems that were overlooked in previous time periods, but when from true ingrowth can be isolated the recruitment component will be
#' added. Plots can be filtered to more frequently sampled plots using plotType and plotPriority arguments, and outlier observations can be removed.
#' 
#' @return This function returns a list that includes productivity summary data frames.
#' 'vst_ANPP_plot_w_taxa' summarises woody ANPP for each year x plot x taxonID combination
#' 'vst_ANPP_plot's woody ANPP for each year x plot combination (taxonIDs are aggregated)
#' 'vst_ANPP_site' summarizes woody ANPP for each site (default is to calculate multi-year mean of each plot and then the mean of all plots at site)
#' 'ANPP_site' adds woody and herbaceous ANPP together (if herbaceous data were in the data input)
#' 'increment_all' contains the increments for each woody individual (approach 1 only)
#' 'increment_outlier' contains the woody individual increments that were removed during outlier removal (approach 1 only)
#'  If calcMethod "approach_2" is selected then only the first four dataframes are returned, and they include an "_2" suffix.

#' @examples
#' \dontrun{
#' # example with arguments at default values
#' estimateProductivityOutputs <- estimateProductivity(input = estimateBiomassOutputs)
#'                       
#' # example specifying many non-default arguments
#' estimateProductivityOutputs <- estimateProductivity(input = "estimateBiomassOutputs", 
#'                plotType = "all", plotPriority = 50, 
#'                calcMethod = "approach_2", outlier = 2, outlier_type = "SD", 
#'                dataProducts = "Vst")
#' 
#' list2env(estimateProductivityOutputs ,.GlobalEnv) # unlist all data frames for easier viewing
#' saveRDS(estimateProductivityOutputs, 'estimateProductivityOutputs.rds') # save output locally
#' }

# changelog and author contributions / copyrights
#   Samuel M Simkin (2021-03-30)  original creation
#   Samuel M Simkin (2022-07-12)  revised
#   Samuel M Simkin (2023-08-04)  revised
#   Samuel M Simkin (2024-09-30)  revised
#################################################################################  

estimateProductivity = function(
                         input = estimateBiomassOutputs,
                         plotType = "tower",
                         plotPriority = 5,
                         calcMethod = "approach_1",
                         outlier = 1.5,
                         outlier_type = "IQR",
                         dataProducts = "VstHbp"
                         ) {
  
options(dplyr.summarise.inform = FALSE)

list2env(input ,.GlobalEnv)

# Error if invalid plotType option selected
  if(plotType != "tower" & plotType != "all"){
    stop("The only valid plotType options are 'tower' or 'all'.")
  }  
  
# Error if invalid plotPriority option selected
  if(plotPriority < 1){
    stop("The minimum plotPriority value is 1, and 5 or greater is the recommended default.")
  }  
  
# Error if invalid calcMethod selected
  if(calcMethod != "approach_1" & calcMethod != "approach_2"){
    stop("The only valid outlier_type options are 'approach_1' or 'approach_2'.")
  }

# Error if invalid outlier_type selected
  if(outlier_type != "SD" & outlier_type != "IQR"){
    stop("The only valid calcMethod options are 'SD' or 'IQR'.")
  }

# Error if invalid dataProducts option selected
  if(dataProducts != "VstHbp" & dataProducts != "Vst"){
    stop("Currently the only valid dataProducts options are 'VstHbp' or 'Vst'.")
  }

if(dataProducts == "VstHbp" & !exists("hbp_agb_per_ha")){
  print("The dataProducts argument was 'VstHbp' but the input data does not include Hbp data so the dataProducts argument has automatically been changed to 'Vst'.")
  dataProducts = "Vst"
  }

start <- min(vst_plot_w_0s$year)
end  <- max(vst_plot_w_0s$year)

### Verify user-supplied inputMass table contains correct data
  #   Check for required columns
  vst_plot_w_0s_ExpCols <- c("domainID", "siteID", "plotID", "eventID","year","plot_eventID","nlcdClass","taxonID","agb_Mg_per_ha__Live","agb_Mg_per_ha__Dead_or_Lost",
                   "specificModuleSamplingPriority","plotType")
  
  if (length(setdiff(vst_plot_w_0s_ExpCols, colnames(vst_plot_w_0s))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_plot_w_0s':", '{paste(setdiff(vst_plot_w_0s_ExpCols, colnames(vst_plot_w_0s)), collapse = ", ")}',
                    .sep = " "))
  }
  

# Error if not at least 2 years of data
  if(as.numeric(end) - as.numeric(start) < 1){
    print("At least 2 years of data are needed to calculate productivity (more for plots sampled less frequently than annually). Current dataset only has woody biomass data from: ")
    print(unique(vst_agb_per_ha$year))
    stop( )
  }

plotType_df <- unique(vst_plot_w_0s %>% dplyr::select(plotID, plotType))

### PLOT-LEVEL BIOMASS INCREMENT (Clark et al. 2001 approach 2) 
print("Calculating woody increment component of productivity at the plot-level (approach 2) ..... ")

 vst_agb_Live <- vst_plot_w_0s 
  vst_agb_Live$Mg_per_ha_live <- vst_agb_Live$agb_Mg_per_ha__Live
  vst_agb_Live$agb_Mg_per_ha__Live <- vst_agb_Live$agb_Mg_per_ha__Dead_or_Lost <- NULL
  vst_agb_Live <- vst_agb_Live[order(vst_agb_Live$year),]

yearList <- unique(sort(vst_agb_Live$year))# sort list of years
vst_increment <- tidyr::pivot_wider(vst_agb_Live, id_cols = c(siteID, plotID, plotType, nlcdClass, taxonID), names_from = year, names_prefix = "Mg_per_ha_", values_from = Mg_per_ha_live) # convert from long to wide format (both years in same row)

# plot-level increment (before incorporating mortality) calculated here
for(i in 2:length(yearList)){
  column_name_prev <- paste0("Mg_per_ha_", yearList[i-1])
  column_name <- paste0("Mg_per_ha_", yearList[i])
  increment_column_name <- paste0("Mg_per_ha_increment_",yearList[i])
  vst_increment <- vst_increment %>% dplyr::mutate(!!increment_column_name := (!!sym(column_name)) - !!sym(column_name_prev) ) 
}

vst_increment_long <- vst_increment %>% dplyr::select(-contains("Mg_per_ha_2"))  %>% 
  tidyr::pivot_longer( cols = !c(plotID,siteID,taxonID,nlcdClass,plotType), names_to = "year",  names_prefix = "Mg_per_ha_increment_", values_to = "Mg_per_ha_increment" )

### PLOT-LEVEL MORTALITY
print("Calculating woody mortality component of productivity at the plot-level (approach 2) ..... ")

if(nrow(vst_agb_per_ha) >0) {
# Categorize individual IDs based on their changes (or not) in plantStatus2
input_to_transitions <- vst_agb_per_ha %>% dplyr::select(plot_eventID,siteID,plotID,individualID,taxonID,plantStatus2,year)
input_to_transitions <- input_to_transitions %>% dplyr::distinct(individualID, taxonID, year, plantStatus2, .keep_all = TRUE) 
input_to_transitions <- input_to_transitions[order(input_to_transitions$year),]

yearList <- unique(sort(vst_agb_per_ha$year)) # sort list of years
suppressWarnings( transitions <- tidyr::pivot_wider(input_to_transitions, id_cols = c(siteID, plotID, individualID, taxonID), names_from = year, names_prefix = "status_", values_from = plantStatus2)  )

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
  transitions <- transitions %>% dplyr::mutate(!!transitionType_column_name := case_when(
  (!!sym(column_name)) == 'Dead_or_Lost' & !!sym(column_name_prev) == 'Live' ~ 'mortality',
    ))
}

mortality <- merge(vst_agb_per_ha, transitions, by=c("plotID", "siteID", "individualID", "taxonID"), all.x=TRUE)
mortality$mortality_Mg_per_ha <- NA

# if transitionType for a given year is "mortality" then assign a mortality value based on the biomass at the PREVIOUS year
for(i in 2:length(yearList)){
  year_previous <- yearList[i-1]
  column_name <- paste0("transitionType_", yearList[i])
  mortality <- mortality %>% dplyr::mutate(mortality_Mg_per_ha = case_when(
  (!!sym(column_name)) == 'mortality' & yearList[i] == year_previous ~ agb_Mg_per_ha, TRUE ~ mortality_Mg_per_ha
    ))
}

mortality$year <- as.numeric(mortality$year + 1)

plot_mortality <- mortality %>% dplyr::group_by(siteID, plotID, taxonID, year) %>% dplyr::summarise(Mg_per_ha_mortality = sum(mortality_Mg_per_ha, na.rm = TRUE)) 
#plot_mortality <- plot_mortality %>% dplyr::filter(mortality_Mg_per_ha != 0)
} else {
  plot_mortality <- data.frame(siteID = character(), plotID = character(), taxonID = character(), year = character(), Mg_per_ha_mortality = numeric()) # create placeholder if vst_agb_per_ha is empty
}

vst_ANPP_plot_w_taxa_2 <- merge (vst_increment_long, plot_mortality, by=c("siteID","plotID","taxonID","year"), all.x=TRUE )
 vst_ANPP_plot_w_taxa_2$year <- as.numeric(vst_ANPP_plot_w_taxa_2$year)
 vst_ANPP_plot_w_taxa_2 <- vst_ANPP_plot_w_taxa_2 %>% dplyr::filter(!is.na(Mg_per_ha_increment) ) # remove records with NA increment; this line is VERY important - without it ~75% of lines have NAs for both increment and mortality, which then turn into false zeros during group_by
 vst_ANPP_plot_w_taxa_2$Mg_per_ha_mortality <- if_else(is.na(vst_ANPP_plot_w_taxa_2$Mg_per_ha_mortality) & !is.na(vst_ANPP_plot_w_taxa_2$Mg_per_ha_increment), 0, vst_ANPP_plot_w_taxa_2$Mg_per_ha_mortality, vst_ANPP_plot_w_taxa_2$Mg_per_ha_mortality)
 vst_ANPP_plot_w_taxa_2 <- vst_ANPP_plot_w_taxa_2 %>% dplyr::group_by(siteID, plotID, plotType, nlcdClass, taxonID, year) %>%  dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
vst_ANPP_plot_w_taxa_2$wood_ANPP__Mg_ha_yr <- vst_ANPP_plot_w_taxa_2$Mg_per_ha_increment + vst_ANPP_plot_w_taxa_2$Mg_per_ha_mortality

vst_ANPP_plot_2 <- vst_ANPP_plot_w_taxa_2 %>% dplyr::group_by(siteID, plotID, plotType, nlcdClass, year) %>% dplyr::summarise(wood_ANPP__Mg_ha_yr = round(sum(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3)) %>% ungroup()
vst_ANPP_plot_2 <- vst_ANPP_plot_2 %>% dplyr::filter(!is.na(wood_ANPP__Mg_ha_yr)) %>% dplyr::select(siteID,plotID,plotType,year,wood_ANPP__Mg_ha_yr) # remove records with missing productivity

if(nrow(vst_agb_zeros) >0){
vst_agb_zeros_plot <- vst_agb_zeros
    vst_agb_zeros_plot$eventID <- vst_agb_zeros_plot$plot_eventID <- NULL
  vst_agb_zeros_plot$wood_ANPP__Mg_ha_yr <- 0 
vst_ANPP_plot_2 <- rbind(vst_ANPP_plot_2, vst_agb_zeros_plot)}

priority_plots_add <- vst_plot_w_0s %>% dplyr::select(plotID, specificModuleSamplingPriority)
priority_plots_add <- unique(priority_plots_add)
vst_ANPP_plot_2 <- merge(vst_ANPP_plot_2, priority_plots_add, by = c("plotID"), all.x=TRUE)
if(plotType == "tower") {vst_ANPP_plot_2 <- vst_ANPP_plot_2 %>% dplyr::filter(plotType == "tower") } # if arg plotType = "tower" then filter to just tower plots, otherwise keep all plots from input data
vst_ANPP_plot_2 <- vst_ANPP_plot_2 %>% dplyr::filter(specificModuleSamplingPriority <= plotPriority) # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))

vst_NPP_plot_yearFirst <- vst_ANPP_plot_2 %>% dplyr::group_by(siteID, plotID, plotType) %>% dplyr::summarise(wood_N = n(), 
          wood_ANPP__Mg_ha_yr_min = round(min(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_max = round(max(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_sd = round(sd(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), 
          wood_ANPP__Mg_ha_yr_se = round((wood_ANPP__Mg_ha_yr_sd / sqrt(wood_N)), 3),  wood_ANPP__Mg_ha_yr = round(mean(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(wood_count_type = "years")
vst_ANPP_site_2 <- vst_NPP_plot_yearFirst %>% dplyr::group_by(siteID) %>% dplyr::summarise(wood_N = n(), 
          wood_ANPP__Mg_ha_yr_min = round(min(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_max = round(max(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_sd = round(sd(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), 
          wood_ANPP__Mg_ha_yr_se = round((wood_ANPP__Mg_ha_yr_sd / sqrt(wood_N)), 3),  wood_ANPP__Mg_ha_yr = round(mean(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(wood_count_type = "plots")

if(grepl("Hbp", dataProducts) )    {

print("Summarizing above-ground herbaceous productivity  ..... ")

### productivity by site and year
hbp_event_means <- hbp_agb_per_ha %>% 
  dplyr::group_by(domainID, siteID, plotID, plotType, eventID, year, bout, nlcdClass, exclosure, peak) %>% # 
  dplyr::summarise(mean_herb_Mg_per_ha = mean(herb_Mg_per_ha, na.rm = TRUE),
            n_obs = length(na.omit(herb_Mg_per_ha))) # calc the mean biomass of the individual plots within eventID and identify of plots contributing to that mean

if(nrow(hbp_event_means %>% filter(exclosure == "Y")) > 0 ) {
df_ <- hbp_event_means %>% 
  dplyr::select(-n_obs) %>%
  tidyr::spread(exclosure, mean_herb_Mg_per_ha) %>% # convert from long format with exclosures and non-exclosures in rows, to wide format with exclosure (Y) and non-exclosure (N) columns
  dplyr::mutate(C = Y - N)    # C is consumption, calculated as biomass in exclosure (Y) minus biomass outside of exclosure (N)

herb_NPP <- df_ %>% dplyr::group_by(domainID, siteID, plotID, plotType, nlcdClass, year) %>%
  dplyr::summarise(
    total_C = sum(na.omit(C)),
    n_bouts_used_for_C = length(na.omit(C)),
    last_bout = last(bout),
    last_bout_mean_herb_Mg_per_ha = last(N),
    herb_NPP__Mg_per_ha_per_yr = total_C + last_bout_mean_herb_Mg_per_ha
  ) %>% ungroup()

 herb_NPP <- dplyr::rename(herb_NPP, "herb_NPP__Mg_ha_yr" = "herb_NPP__Mg_per_ha_per_yr") 
 herb_NPP$siteID <- substr(herb_NPP$plotID, 1, 4)
 herb_NPP <- herb_NPP %>% dplyr::filter(plotType == "tower") # toggle on to remove distributed plots
 herb_NPP <- herb_NPP %>% dplyr::filter(!is.na(herb_NPP__Mg_ha_yr) ) # remove records that are missing productivity 
 herb_NPP <- herb_NPP %>% dplyr::select(siteID,plotID,plotType,year,herb_NPP__Mg_ha_yr)
herb_NPP_temporal_ave <- herb_NPP %>% dplyr::group_by(plotID) %>% dplyr::summarise(herb_NPP__Mg_ha_yr_temporal_ave = round(mean(herb_NPP__Mg_ha_yr, na.rm = TRUE),3))

herb_yearFirst <- herb_NPP %>% dplyr::group_by(siteID, plotID, plotType) %>% dplyr::summarise(herb_N = n(), 
          herb_NPP__Mg_ha_yr_min = round(min(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_max = round(max(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_sd = round(sd(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), 
          herb_NPP__Mg_ha_yr_se = round((herb_NPP__Mg_ha_yr_sd / sqrt(herb_N)), 3),  herb_NPP__Mg_ha_yr = round(mean(herb_NPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(herb_count_type ="years")
herb_yearThenSite <- herb_yearFirst %>% dplyr::group_by(siteID) %>% dplyr::summarise(herb_N = n(), 
          herb_NPP__Mg_ha_yr_min = round(min(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_max = round(max(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_sd = round(sd(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), 
          herb_NPP__Mg_ha_yr_se = round((herb_NPP__Mg_ha_yr_sd / sqrt(herb_N)), 3),  herb_NPP__Mg_ha_yr = round(mean(herb_NPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(herb_count_type ="plots")

herb_siteFirst <- herb_NPP %>% dplyr::group_by(siteID, year) %>% dplyr::summarise(herb_N = n(), 
          herb_NPP__Mg_ha_yr_min = round(min(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_max = round(max(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_sd = round(sd(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), 
          herb_NPP__Mg_ha_yr_se = round((herb_NPP__Mg_ha_yr_sd / sqrt(herb_N)), 3),  herb_NPP__Mg_ha_yr = round(mean(herb_NPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(herb_count_type ="plots")
herb_siteThenYear <- herb_siteFirst %>% dplyr::group_by(siteID) %>% dplyr::summarise(herb_N = n(), 
          herb_NPP__Mg_ha_yr_min = round(min(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_max = round(max(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), herb_NPP__Mg_ha_yr_sd = round(sd(herb_NPP__Mg_ha_yr, na.rm = TRUE),3), 
          herb_NPP__Mg_ha_yr_se = round((herb_NPP__Mg_ha_yr_sd / sqrt(herb_N)), 3),  herb_NPP__Mg_ha_yr = round(mean(herb_NPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(herb_count_type ="years")


print("Combining above-ground woody and herbaceous productivity (approach 2) ..... ")
ANPP_site_2 <- merge(vst_ANPP_site_2, herb_yearThenSite, by=c("siteID"), all=TRUE); 
ANPP_site_2$ave_order <- "yearThenSite" 


############# At plot level -    Add woody and herb NPP and calculate percent contributed by herbs #############
ANPP_site_2 <- ANPP_site_2 %>% dplyr::filter(!is.na(wood_ANPP__Mg_ha_yr))  # remove records with missing productivity
   
ANPP_site_2$vstHbp_NPP__Mg_ha_yr <- ANPP_site_2$wood_ANPP__Mg_ha_yr + ANPP_site_2$herb_NPP__Mg_ha_yr
ANPP_site_2$vstHbp_NPP__Mg_ha_yr_yr_se <- ANPP_site_2$wood_ANPP__Mg_ha_yr_se + ANPP_site_2$herb_NPP__Mg_ha_yr_se
ANPP_site_2$herb_percent_of_NPP <- round((100 * ANPP_site_2$herb_NPP__Mg_ha_yr / ANPP_site_2$vstHbp_NPP__Mg_ha_yr ), 1)
ANPP_site_2$herb_percent_of_NPP_se <- round((abs(ANPP_site_2$herb_percent_of_NPP) * sqrt((ANPP_site_2$herb_NPP__Mg_ha_yr_se / ANPP_site_2$herb_NPP__Mg_ha_yr)^2 + 
                                                    (ANPP_site_2$vstHbp_NPP__Mg_ha_yr_yr_se / ANPP_site_2$vstHbp_NPP__Mg_ha_yr)^2) ), 1)

ANPP_site_2 <- ANPP_site_2 %>% dplyr::select(siteID, wood_N, wood_count_type, wood_ANPP__Mg_ha_yr_se, wood_ANPP__Mg_ha_yr, herb_N, herb_NPP__Mg_ha_yr_se, herb_NPP__Mg_ha_yr, vstHbp_NPP__Mg_ha_yr, 
                                          herb_percent_of_NPP_se, herb_percent_of_NPP) %>% arrange(herb_percent_of_NPP)

} else {
  dataProducts = "Vst"
  print("No herbaceous exclosure data available in current data subset for calculation of consumption, so herbaceous productivity not summarized.")
}

}



if(calcMethod == "approach_1")    {


####################################################################################################################################################
##########  CALCULATE WOODY BIOMASS INCREMENT AT THE LEVEL OF INDIVIDUALID, AS RECOMMENDED BY TWG MEMBERS (This is Clark et al 2001 Approach 1) ##########

print("Calculating woody increment productivity at the level of individualID (approach 1) ..... ")

# Filter to year-pairs that were live at earlier years and still live at target year for increment, and year-pairs that were live at earlier years and dead at target year for mortality
# Calculating in this way accommodated individualIDs that had more than one status for the same year (e.g., if one bole grew and another died)
# assumption: if data are not from consecutive years then assume equal rate of growth and divide increment and mortality by the number of elapsed years      

if(nrow(vst_agb_per_ha) > 0){

vst_agb_per_ha <- vst_agb_per_ha[order(vst_agb_per_ha$year),]

vst_agb_plot_list <- unique(vst_agb_per_ha$plotID); length(vst_agb_plot_list)
vst_agb_indID_list <- unique(vst_agb_per_ha$individualID); length(vst_agb_indID_list)

startAGB <- min(vst_agb_per_ha$year)
endAGB  <- max(vst_agb_per_ha$year)
dummy_rows <- data.frame(plot_eventID = rep(NA, 7), eventID = rep(NA, 7), siteID = rep(NA, 7), plotID = rep(NA, 7), plotType = rep(NA, 7), nlcdClass = rep(NA, 7), individualID = rep(NA, 7),
  taxonID = rep(NA, 7), plantStatus2 = rep(NA, 7), year = ( (startAGB-7):(startAGB-1) ), agb_Mg_per_ha = rep(NA, 7)) # placeholder for any years prior to start that are missing
yearLength <- length(startAGB:endAGB)
dummy_rows_2 <- data.frame(plot_eventID = rep(NA, yearLength), eventID = rep(NA, yearLength), siteID = rep(NA, yearLength), plotID = rep(NA, yearLength), 
                           plotType = rep(NA, yearLength), nlcdClass = rep(NA, yearLength), individualID = rep(NA, yearLength),
  taxonID = rep(NA, yearLength), plantStatus2 = rep(NA, yearLength), year = ( startAGB:endAGB ), agb_Mg_per_ha = rep(NA, yearLength)) # placeholder for any years within start to end year range that are missing
dummy_rows <- rbind(dummy_rows, dummy_rows_2)
dummy_rows <- dummy_rows[!dummy_rows$year %in% unique(vst_agb_per_ha$year),] # remove the years that ARE in the data


increment_all = data.frame()
increment_qf_all = data.frame()
endYear <- as.numeric((startAGB) : endAGB )
for(i in 2:length(endYear)){
transitions <- vst_agb_per_ha
 transitions$keep <- if_else(transitions$year < endYear[i] & transitions$plantStatus2 == "Live", "keep","discard","discard")
 transitions$keep <- if_else(transitions$year == endYear[i] & !is.na(transitions$plantStatus2), "keep",transitions$keep,transitions$keep)
 transitions <- transitions %>% dplyr::filter(keep == "keep") %>% dplyr::select(-keep) # remove records that aren't live in earlier years 
increment <- transitions %>% dplyr::filter(!(year == endYear[i] & plantStatus2 == "Dead_or_Lost") ) # remove records that are dead in end year
 increment <- rbind(increment, dummy_rows) # only year populated, workaround to get blank columns for all years
 increment <- increment[increment$year <= endYear[i],] 
 increment$year <- abs(increment$year - endYear[i])
 increment <- tidyr::pivot_wider(increment, id_cols = c(siteID, plotID, individualID, taxonID), names_from = year, names_prefix = "agb_Mg_per_ha_", values_from = agb_Mg_per_ha)
 increment <- increment %>% dplyr::filter(!is.na(plotID) ) # remove artifact row with NAs from dummy row
 increment$increment1 <- increment$agb_Mg_per_ha_0 - increment$agb_Mg_per_ha_1
   increment_stats <- increment %>% dplyr::group_by(siteID) %>% dplyr::filter(!is.na(increment1)) %>% 
      dplyr::summarise(increment1_n = n(), increment1_sd = round(sd(increment1, na.rm = TRUE),3), increment1_se = round(increment1_sd / sqrt(increment1_n), 3), increment1_mn = round(mean(increment1, na.rm = TRUE),3),
         increment1_firstQuart = quantile(increment1, probs = c(0.25), na.rm=TRUE), increment1_thirdQuart = quantile(increment1, probs = c(0.75), na.rm=TRUE), increment1_IQR = increment1_thirdQuart - increment1_firstQuart)
   increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment1_IQRqf <- if_else((increment$increment1 < increment$increment1_firstQuart-(increment$increment1_IQR * outlier)) | (increment$increment1 > increment$increment1_thirdQuart+(increment$increment1_IQR * outlier)  ), "flag","ok","ok")
    increment$increment1_SDqf <- if_else(abs(increment$increment1 - increment$increment1_mn) > (increment$increment1_sd * outlier), "flag","ok","ok")
increment$increment2 <- (increment$agb_Mg_per_ha_0 - increment$agb_Mg_per_ha_2)/2
   increment_stats <- increment %>% dplyr::group_by(siteID) %>% dplyr::filter(!is.na(increment2)) %>% 
      dplyr::summarise(increment2_n = n(), increment2_sd = round(sd(increment2, na.rm = TRUE),3), increment2_se = round(increment2_sd / sqrt(increment2_n), 3), increment2_mn = round(mean(increment2, na.rm = TRUE),3),
         increment2_firstQuart = quantile(increment2, probs = c(0.25), na.rm=TRUE), increment2_thirdQuart = quantile(increment2, probs = c(0.75), na.rm=TRUE), increment2_IQR = increment2_thirdQuart - increment2_firstQuart)
   increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment2_IQRqf <- if_else((increment$increment2 < increment$increment2_firstQuart-(increment$increment2_IQR * outlier)) | (increment$increment2 > increment$increment2_thirdQuart+(increment$increment2_IQR * outlier)  ), "flag","ok","ok")
    increment$increment2_SDqf <- if_else(abs(increment$increment2 - increment$increment2_mn) > (increment$increment2_sd * outlier), "flag","ok","ok")
 increment$increment3 <- (increment$agb_Mg_per_ha_0 - increment$agb_Mg_per_ha_3)/3
   increment_stats <- increment %>% dplyr::group_by(siteID) %>% dplyr::filter(!is.na(increment3)) %>% 
      dplyr::summarise(increment3_n = n(), increment3_sd = round(sd(increment3, na.rm = TRUE),3), increment3_se = round(increment3_sd / sqrt(increment3_n), 3), increment3_mn = round(mean(increment3, na.rm = TRUE),3),
         increment3_firstQuart = quantile(increment3, probs = c(0.25), na.rm=TRUE), increment3_thirdQuart = quantile(increment3, probs = c(0.75), na.rm=TRUE), increment3_IQR = increment3_thirdQuart - increment3_firstQuart)
   increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment3_IQRqf <- if_else((increment$increment3 < increment$increment3_firstQuart-(increment$increment3_IQR * outlier)) | (increment$increment3 > increment$increment3_thirdQuart+(increment$increment3_IQR * outlier)  ), "flag","ok","ok")
    increment$increment3_SDqf <- if_else(abs(increment$increment3 - increment$increment3_mn) > (increment$increment3_sd * outlier), "flag","ok","ok")
 increment$increment4 <- (increment$agb_Mg_per_ha_0 - increment$agb_Mg_per_ha_4)/4
   increment_stats <- increment %>% dplyr::group_by(siteID) %>% dplyr::filter(!is.na(increment4)) %>% 
      dplyr::summarise(increment4_n = n(), increment4_sd = round(sd(increment4, na.rm = TRUE),3), increment4_se = round(increment4_sd / sqrt(increment4_n), 3), increment4_mn = round(mean(increment4, na.rm = TRUE),3),
         increment4_firstQuart = quantile(increment4, probs = c(0.25), na.rm=TRUE), increment4_thirdQuart = quantile(increment4, probs = c(0.75), na.rm=TRUE), increment4_IQR = increment4_thirdQuart - increment4_firstQuart)
   increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment4_IQRqf <- if_else((increment$increment4 < increment$increment4_firstQuart-(increment$increment4_IQR * outlier)) | (increment$increment4 > increment$increment4_thirdQuart+(increment$increment4_IQR * outlier)  ), "flag","ok","ok")
    increment$increment4_SDqf <- if_else(abs(increment$increment4 - increment$increment4_mn) > (increment$increment4_sd * outlier), "flag","ok","ok")
 increment$increment5 <- (increment$agb_Mg_per_ha_0 - increment$agb_Mg_per_ha_5)/5
   increment_stats <- increment %>% dplyr::group_by(siteID) %>% dplyr::filter(!is.na(increment5)) %>% 
      dplyr::summarise(increment5_n = n(), increment5_sd = round(sd(increment5, na.rm = TRUE),3), increment5_se = round(increment5_sd / sqrt(increment5_n), 3), increment5_mn = round(mean(increment5, na.rm = TRUE),3),
         increment5_firstQuart = quantile(increment5, probs = c(0.25), na.rm=TRUE), increment5_thirdQuart = quantile(increment5, probs = c(0.75), na.rm=TRUE), increment5_IQR = increment5_thirdQuart - increment5_firstQuart)
   increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment5_IQRqf <- if_else((increment$increment5 < increment$increment5_firstQuart-(increment$increment5_IQR * outlier)) | (increment$increment5 > increment$increment5_thirdQuart+(increment$increment5_IQR * outlier)  ), "flag","ok","ok")
    increment$increment5_SDqf <- if_else(abs(increment$increment5 - increment$increment5_mn) > (increment$increment5_sd * outlier), "flag","ok","ok")
 increment$increment6 <- (increment$agb_Mg_per_ha_0 - increment$agb_Mg_per_ha_6)/6
   increment_stats <- increment %>% dplyr::group_by(siteID) %>% dplyr::filter(!is.na(increment6)) %>% 
      dplyr::summarise(increment6_n = n(), increment6_sd = round(sd(increment6, na.rm = TRUE),3), increment6_se = round(increment6_sd / sqrt(increment6_n), 3), increment6_mn = round(mean(increment6, na.rm = TRUE),3),
         increment6_firstQuart = quantile(increment6, probs = c(0.25), na.rm=TRUE), increment6_thirdQuart = quantile(increment6, probs = c(0.75), na.rm=TRUE), increment6_IQR = increment6_thirdQuart - increment6_firstQuart)
   increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
     increment$increment6_IQRqf <- if_else((increment$increment6 < increment$increment6_firstQuart-(increment$increment6_IQR * outlier)) | (increment$increment6 > increment$increment6_thirdQuart+(increment$increment6_IQR * outlier)  ), "flag","ok","ok")
    increment$increment6_SDqf <- if_else(abs(increment$increment6 - increment$increment6_mn) > (increment$increment6_sd * outlier), "flag","ok","ok")
 increment$increment7 <- (increment$agb_Mg_per_ha_0 - increment$agb_Mg_per_ha_7)/7
   increment_stats <- increment %>% dplyr::group_by(siteID) %>% dplyr::filter(!is.na(increment7)) %>% 
      dplyr::summarise(increment7_n = n(), increment7_sd = round(sd(increment7, na.rm = TRUE),3), increment7_se = round(increment7_sd / sqrt(increment7_n), 3), increment7_mn = round(mean(increment7, na.rm = TRUE),3),
         increment7_firstQuart = quantile(increment7, probs = c(0.25), na.rm=TRUE), increment7_thirdQuart = quantile(increment7, probs = c(0.75), na.rm=TRUE), increment7_IQR = increment7_thirdQuart - increment7_firstQuart)
   increment <- merge(increment, increment_stats, by="siteID", all.x=TRUE)
    increment$increment7_IQRqf <- if_else((increment$increment7 < increment$increment7_firstQuart-(increment$increment7_IQR * outlier)) | (increment$increment7 > increment$increment7_thirdQuart+(increment$increment7_IQR * outlier)  ), "flag","ok","ok")
    increment$increment7_SDqf <- if_else(abs(increment$increment7 - increment$increment7_mn) > (increment$increment7_sd * outlier), "flag","ok","ok")
 increment$Mg_per_ha_per_yr <- ifelse(!is.na(increment$increment7), increment$increment7, NA)
    increment$bestIncrement <- ifelse(!is.na(increment$increment7), 7, NA)
 increment$Mg_per_ha_per_yr <- ifelse(!is.na(increment$increment6), increment$increment6, increment$Mg_per_ha_per_yr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment6), 6, increment$bestIncrement)
 increment$Mg_per_ha_per_yr <- ifelse(!is.na(increment$increment5), increment$increment5, increment$Mg_per_ha_per_yr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment5), 5, increment$bestIncrement)
 increment$Mg_per_ha_per_yr <- ifelse(!is.na(increment$increment4), increment$increment4, increment$Mg_per_ha_per_yr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment4), 4, increment$bestIncrement)
 increment$Mg_per_ha_per_yr <- ifelse(!is.na(increment$increment3), increment$increment3, increment$Mg_per_ha_per_yr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment3), 3, increment$bestIncrement)
 increment$Mg_per_ha_per_yr <- ifelse(!is.na(increment$increment2), increment$increment2, increment$Mg_per_ha_per_yr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment2), 2, increment$bestIncrement)
 increment$Mg_per_ha_per_yr <- ifelse(!is.na(increment$increment1), increment$increment1, increment$Mg_per_ha_per_yr)
    increment$bestIncrement <- ifelse(!is.na(increment$increment1), 1, increment$bestIncrement)
 increment <- increment %>% dplyr::filter(!is.na(Mg_per_ha_per_yr)) %>% mutate(endYear = endYear[i])   # remove missing increment values and add endYear column
 increment <- dplyr::rename(increment, "Mg_per_ha_per_yr_inc" = "Mg_per_ha_per_yr", "bestIncrement_inc" = "bestIncrement")
increment_all = dplyr::bind_rows(increment_all, increment) %>% mutate(outlier_threshold = paste0(outlier,"_",outlier_type))
}

if(nrow(increment_all) > 0){

if(outlier_type == "SD") {
increment_ok <- increment_all %>% dplyr::filter(increment1_SDqf != "flag" & increment2_SDqf != "flag"  & increment3_SDqf != "flag" & increment4_SDqf != "flag" & increment5_SDqf != "flag" & increment6_SDqf != "flag" & increment7_SDqf != "flag") # remove increments that are flagged as being excessive for the specified timeframes
increment_outlier <- increment_all %>% dplyr::filter(increment1_SDqf == "flag" | increment2_SDqf == "flag"  | increment3_SDqf == "flag" | increment4_SDqf == "flag" | increment5_SDqf == "flag" | increment6_SDqf == "flag" | increment7_SDqf == "flag") # increments that are flagged as being excessive for the specified timeframes
increment_outlier <- increment_outlier %>% dplyr::select(outlier_threshold, siteID, plotID, individualID, taxonID, Mg_per_ha_per_yr_inc, bestIncrement_inc, endYear)
} 
if(outlier_type == "IQR") {
increment_ok <- increment_all %>% dplyr::filter(increment1_IQRqf != "flag" & increment2_IQRqf != "flag"  & increment3_IQRqf != "flag" & increment4_IQRqf != "flag" & increment5_IQRqf != "flag" & increment6_IQRqf != "flag" & increment7_IQRqf != "flag") # remove increments that are flagged as being excessive for the specified timeframes
increment_outlier <- increment_all %>% dplyr::filter(increment1_IQRqf == "flag" | increment2_IQRqf == "flag"  | increment3_IQRqf == "flag" | increment4_IQRqf == "flag" | increment5_IQRqf == "flag" | increment6_IQRqf == "flag" | increment7_IQRqf == "flag") # increments that are flagged as being excessive for the specified timeframes
increment_outlier <- increment_outlier %>% dplyr::select(outlier_threshold, siteID, plotID, individualID, taxonID, Mg_per_ha_per_yr_inc, bestIncrement_inc, endYear)
}

outlier_count <- length(increment_outlier$individualID)
increment_all_count <- length(increment_all$individualID)
percent_outliers <- round(100 * outlier_count / increment_all_count, 1)

print(paste0("Note: The chosen outlier criteria removed ", outlier_count, " records (", percent_outliers, "% of all records)"))

increment_ok <- increment_ok %>% dplyr::select(outlier_threshold, siteID, plotID, individualID, taxonID, Mg_per_ha_per_yr_inc, bestIncrement_inc, endYear)
 increment_ok <- increment_ok %>% dplyr::filter(bestIncrement_inc == 1 | bestIncrement_inc == 2 | bestIncrement_inc == 3)  #(bestIncrement_inc == 5 & (siteID =="JORN" | siteID =="MOAB" | siteID =="ONAQ" | siteID =="SRER") ) )
  # remove increments without the allowed increment timeframe; we only need a single year increment for the 5 priority tower plots, but the 2 year increment means we don't lose 2 years worth of increment from missing just 1 measurement of an individual,
                                         
increment_indID_list <- unique(increment_ok$individualID); length(increment_indID_list)
increment_eventID_list <- unique(paste0(increment_ok$plotID,"_vst_",substr(increment_ok$plotID, 1,4), "_", increment_ok$endYear))

print("Finish calculating woody productivity at the level of individualID (approach 1) ..... ")

#product_all <- merge(increment_all, recruitment_all, by=c("siteID", "plotID", "individualID", "taxonID","endYear"), all=TRUE)  # recruitment not yet incorporated since sensitive to large individ. missed in previous bout
#product_all <- product_all %>% dplyr::mutate_all(~replace(., is.na(.), 0)) %>% dplyr::mutate(wood_ANPP__Mg_ha_yr = Mg_per_ha_per_yr_inc + Mg_per_ha_per_yr_rec)  
product_all <- increment_ok        # placeholder until incorporate recruitment
product_all <- dplyr::rename(product_all, "wood_ANPP__Mg_ha_yr" = "Mg_per_ha_per_yr_inc")     
product_all <- dplyr::rename(product_all, "year" = "endYear")
productivity <- merge(product_all, plotType_df, by = "plotID", all.x = T)

# sum the individualIDs by plot and taxonID
vst_ANPP_plot_w_taxa <- productivity %>% dplyr::select(-bestIncrement_inc) %>% dplyr::group_by(outlier_threshold, siteID, plotID, plotType, year, taxonID) %>%  dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

# sum the individualIDs by plot
vst_ANPP_plot <- vst_ANPP_plot_w_taxa %>% dplyr::group_by(outlier_threshold, siteID, plotID, plotType, year) %>%  dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% ungroup()

vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(!is.na(wood_ANPP__Mg_ha_yr)) %>% dplyr::select(outlier_threshold,siteID,plotID,plotType,year,wood_ANPP__Mg_ha_yr) # remove records with missing productivity

# ADD PLOTS WITH 0 BIOMASS AND HERB BIOMASS 
  
if(nrow(vst_agb_zeros) >0){
  vst_agb_zeros_ind <- vst_agb_zeros
    vst_agb_zeros_ind$eventID <- vst_agb_zeros_ind$plot_eventID <- NULL
    vst_agb_zeros_ind$outlier_threshold <- paste0(outlier,"_",outlier_type)
    vst_agb_zeros_ind$wood_ANPP__Mg_ha_yr <- 0 
    vst_ANPP_plot <- rbind(vst_ANPP_plot, vst_agb_zeros_ind)}

vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(year >= start) # make sure that records from before the start year have been removed
vst_ANPP_plot <- merge(vst_ANPP_plot, priority_plots_add, by = c("plotID"), all.x=TRUE)
vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(plotType == "tower") # if plotType argument to function is "tower" then remove distributed plots
vst_ANPP_plot <- vst_ANPP_plot %>% dplyr::filter(specificModuleSamplingPriority <= 5) # as specified in argument to function, remove lower priority plots that aren't required to be sampled every year

wood_siteGrandMean <- vst_ANPP_plot %>% dplyr::group_by(outlier_threshold, siteID) %>% dplyr::summarise(wood_N = n(), 
          wood_ANPP__Mg_ha_yr_min = round(min(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_max = round(max(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_sd = round(sd(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), 
          wood_ANPP__Mg_ha_yr_se = round((wood_ANPP__Mg_ha_yr_sd / sqrt(wood_N)), 3),  wood_ANPP__Mg_ha_yr = round(mean(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(wood_count_type = "plotXYearcombos")

wood_siteFirst <- vst_ANPP_plot %>% dplyr::group_by(outlier_threshold, siteID, year) %>% dplyr::summarise(wood_N = n(), 
          wood_ANPP__Mg_ha_yr_min = round(min(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_max = round(max(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_sd = round(sd(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), 
          wood_ANPP__Mg_ha_yr_se = round((wood_ANPP__Mg_ha_yr_sd / sqrt(wood_N)), 3),  wood_ANPP__Mg_ha_yr = round(mean(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(wood_count_type = "plots")
wood_siteThenYear <- wood_siteFirst %>% dplyr::group_by(outlier_threshold, siteID) %>% dplyr::summarise(wood_N = n(), 
          wood_ANPP__Mg_ha_yr_min = round(min(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_max = round(max(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_sd = round(sd(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), 
          wood_ANPP__Mg_ha_yr_se = round((wood_ANPP__Mg_ha_yr_sd / sqrt(wood_N)), 3),  wood_ANPP__Mg_ha_yr = round(mean(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(wood_count_type = "years")

wood_yearFirst <- vst_ANPP_plot %>% dplyr::group_by(outlier_threshold, siteID, plotID, plotType) %>% dplyr::summarise(wood_N = n(), 
          wood_ANPP__Mg_ha_yr_min = round(min(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_max = round(max(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_sd = round(sd(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), 
          wood_ANPP__Mg_ha_yr_se = round((wood_ANPP__Mg_ha_yr_sd / sqrt(wood_N)), 3),  wood_ANPP__Mg_ha_yr = round(mean(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(wood_count_type = "years")
vst_ANPP_site <- wood_yearFirst %>% dplyr::group_by(outlier_threshold, siteID) %>% dplyr::summarise(wood_N = n(), 
          wood_ANPP__Mg_ha_yr_min = round(min(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_max = round(max(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), wood_ANPP__Mg_ha_yr_sd = round(sd(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3), 
          wood_ANPP__Mg_ha_yr_se = round((wood_ANPP__Mg_ha_yr_sd / sqrt(wood_N)), 3),  wood_ANPP__Mg_ha_yr = round(mean(wood_ANPP__Mg_ha_yr, na.rm = TRUE),3) ) %>% dplyr::mutate(wood_count_type = "plots")

# toggle on one of the three choices above to pick which ANPP_optimize input is used: 1) average across plots and years all in one step, 2)average within years then by site, or 3) average within sites then by year (if all toggled on then the last will be used)

###### If option to also examine herbaceous productivity has been selected then make the necessary calculations and combine with woody productivity  ##############

print("Combining above-ground woody and herbaceous productivity (approach 1) ..... ")

if(grepl("Hbp", dataProducts) )    {
 
ANPP_site <- merge(vst_ANPP_site, herb_yearThenSite, by=c("siteID"), all=TRUE); ANPP_site$ave_order <- "yearThenSite" # toggle to average years within site, and only then across plots


############# Add woody and herb NPP and calculate percent contributed by herbs #############
ANPP_site <- ANPP_site %>% dplyr::filter(!is.na(wood_ANPP__Mg_ha_yr))  # remove records with missing productivity
   
ANPP_site$vstHbp_NPP__Mg_ha_yr <- ANPP_site$wood_ANPP__Mg_ha_yr + ANPP_site$herb_NPP__Mg_ha_yr
ANPP_site$vstHbp_NPP__Mg_ha_yr_yr_se <- ANPP_site$wood_ANPP__Mg_ha_yr_se + ANPP_site$herb_NPP__Mg_ha_yr_se
ANPP_site$herb_percent_of_NPP <- round((100 * ANPP_site$herb_NPP__Mg_ha_yr / ANPP_site$vstHbp_NPP__Mg_ha_yr ), 1)
ANPP_site$herb_percent_of_NPP_se <- round((abs(ANPP_site$herb_percent_of_NPP) * sqrt((ANPP_site$herb_NPP__Mg_ha_yr_se / ANPP_site$herb_NPP__Mg_ha_yr)^2 + 
                                                    (ANPP_site$vstHbp_NPP__Mg_ha_yr_yr_se / ANPP_site$vstHbp_NPP__Mg_ha_yr)^2) ), 1)

ANPP_site <- ANPP_site %>% dplyr::select(siteID, wood_N, wood_count_type, wood_ANPP__Mg_ha_yr_se, wood_ANPP__Mg_ha_yr, herb_N, herb_NPP__Mg_ha_yr_se, herb_NPP__Mg_ha_yr, vstHbp_NPP__Mg_ha_yr, 
                                          herb_percent_of_NPP_se, herb_percent_of_NPP) %>% arrange(herb_percent_of_NPP)
}

print("Returning productivity summary data frames as a list object, calculated using approach 1  ..... ")
} else {print("Current data subset has no individualID that has been measured more than once, so woody increment and productivity can not be calculated using approach 1.")}

} else {print("Current data subset has no biomass, so woody increment and productivity can not be calculated using approach 1.")}

if(grepl("Hbp", dataProducts) )    {
output.list <- list(
   increment_all = increment_all,
   increment_outlier = increment_outlier,
   vst_ANPP_plot_w_taxa = vst_ANPP_plot_w_taxa,
   vst_ANPP_plot = vst_ANPP_plot,
   vst_ANPP_site = vst_ANPP_site,
   ANPP_site = ANPP_site
   )
 return(output.list)
  } else {
  output.list <- list(
   increment_all = increment_all,
   increment_outlier = increment_outlier,
   vst_ANPP_plot_w_taxa = vst_ANPP_plot_w_taxa,
   vst_ANPP_plot = vst_ANPP_plot,
   vst_ANPP_site = vst_ANPP_site
)
 return(output.list)
  }
}

if(calcMethod == "approach_2")    {

print("Returning productivity summary data frames as a list object, calculated using approach 2  ..... ")

if(grepl("Hbp", dataProducts) )    {
output.list <- list(
   vst_ANPP_plot_w_taxa_2 = vst_ANPP_plot_w_taxa_2,
   vst_ANPP_plot_2 = vst_ANPP_plot_2,
   vst_ANPP_site_2 = vst_ANPP_site_2,
   ANPP_site_2 = ANPP_site_2
   )
 return(output.list)
  } else {
  output.list <- list(
   vst_ANPP_plot_w_taxa_2 = vst_ANPP_plot_w_taxa_2,
   vst_ANPP_plot_2 = vst_ANPP_plot_2,
   vst_ANPP_site_2 = vst_ANPP_site_2,
)
 return(output.list)
  }
}

}

#estimateProductivityOutputs <- estimateProductivity(input = "NEONbiomassOutputs.rds", calcMethod = "approach_1", outlier = 1.5, outlier_type = "IQR")
#estimateProductivityOutputs <- estimateProductivity(input = "NEONbiomassOutputs.rds", calcMethod = "approach_1", outlier = 2, outlier_type = "SD")

#list2env(estimateProductivityOutputs ,.GlobalEnv) # unlist all data frames for easier viewing or additional analysis
#saveRDS(estimateProductivityOutputs, 'estimateProductivityOutputs.rds') # save all outputs locally for further examination

