##############################################################################################
#' @title Scale NEON aboveground biomass (AGB) contributed by herbaceous vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Summarise aboveground herbaceous biomass. Biomass outputs can, if desired, be used in the follow-up estimateHerbProd productivity function. 
#' Data inputs are "Herbaceous clip harvest" (DP1.10023.001) retrieved using the neonUtilities::loadByProduct() function (preferred), or data downloaded from the NEON Data Portal.

#' @details Input data can be filtered by site, date, plot type, and plot priority. Herbaceous biomass are scaled to an area basis at the hierarchical levels of clip cell, plot, and site.

#' @param inputDataList A list object comprised of "Herbaceous clip harvest" tables (DP1.10023.001) downloaded using the neonUtilities::loadByProduct function. 
#' If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 
#' 'hbp_perbout', and 'hbp_massdata' arguments. [list]
#' @param hbp_perbout The 'hbp_perbout' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' @param hbp_massdata The 'hbp_massdata' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' @param plotType Optional filter based on NEON plot type. Options are "all" (both tower and distributed plot types) or "tower". Defaults to "all". [character]
#' @param plotPriority NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is NA. [numeric]

#' @return This function returns a list that includes biomass summary data frames.
#' 'hbp_agb' summarizes above-ground herbaceous biomass for each record (units are g per m2).
#' 'hbp_plot' summarizes above-ground live herbaceous biomass for each plot by year combination (units are both g per m2 and Megagrams per hectare).
#' 'hbp_site' summarizes above-ground live herbaceous biomass for each site by year combination (units are both g per m2 and Megagrams per hectare).

#' @examples
#' \dontrun{
#' # Obtain NEON Herbaceous clip harvest data
#' HbpDat <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
#'      package = "basic", check.size = FALSE)
#'      
#' # If list is not in memory, load HBP list of dataframes from local file:
#' load('HbpDat.rds') # load NEON HBP portal data from a local file
#' 
#' # example with arguments at default values
#' scaleHerbMassOutputs <- scaleHerbMass(inputDataList = HbpDat, hbp_perbout = NA, hbp_massdata = NA)
#' 
#' list2env(scaleHerbMassOutputs ,.GlobalEnv) # unlist all data frames
#' saveRDS(scaleHerbMassOutputs, 'scaleHerbMassOutputs.rds') # save all outputs locally
#' }

# changelog and author contributions / copyrights
# Samuel M Simkin (2021-03-30)  original creation
# Samuel M Simkin (2025-01-15)  revised
##############################################################################################

scaleHerbMass = function(inputDataList, hbp_perbout = NA, hbp_massdata = NA,
                       plotType = "all",
                       plotPriority = NA
                       ) {

options(dplyr.summarise.inform = FALSE)
  
  
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
  if (inherits(inputDataList, "list") & (!is.logical(hbp_perbout) | !is.logical(hbp_massdata))) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }
  
  ### Verify 'hbp_perbout' and 'hbp_massdata' are data frames if 'inputDataList' is missing
  if (is.null(inputDataList) & 
      (!inherits(hbp_perbout, "data.frame") | !inherits(hbp_massdata, "data.frame"))) {
    
    stop("Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
  }
  

if (inherits(inputDataList, "list")) {
hbp_perbout <- inputDataList$hbp_perbout
hbp_massdata <- inputDataList$hbp_massdata
}


 ### Verify input tables contain required columns and data ####
  
  ### Verify 'hbp_perbout' table contains required data
  #   Check for required columns
  boutExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "clipArea", "exclosure", "samplingImpractical", "targetTaxaPresent")

  if (length(setdiff(boutExpCols, colnames(hbp_perbout))) > 0) {
    stop(glue::glue("Required columns missing from 'hbp_perbout':", '{paste(setdiff(boutExpCols, colnames(hbp_perbout)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(hbp_perbout) == 0) {
    stop(glue::glue("Table 'hbp_perbout' has no data."))
  }


  ### Verify 'hbp_massdata' table contains required data
  #   Check for required columns
  massExpCols <- c("domainID", "siteID", "plotID", "plotType", "sampleCondition", "herbGroup", "dryMass", "qaDryMass")

  if (length(setdiff(massExpCols, colnames(hbp_massdata))) > 0) {
    stop(glue::glue("Required columns missing from 'hbp_massdata':", '{paste(setdiff(massExpCols, colnames(hbp_massdata)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(hbp_massdata) == 0) {
    stop(glue::glue("Table 'hbp_massdata' has no data."))
  }

  
hbp_perbout$year <- as.numeric(substr(hbp_perbout$eventID,5,8) ) # year component of eventID

##### Aggregate the herbaceous data #############################
# Original hbp scripts by Eric Sokol (esokol@battelleecology) in May 2020. Merged and modified by Sam Simkin (ssimkin@battelleecology.org) in Jul 2023


# filter mass data to retain only qaDryMass == N (to avoid duplicates when there is a qa sample too)
hbp_massdata$herbGroup <- ifelse(is.na(hbp_massdata$herbGroup), "Unknown", hbp_massdata$herbGroup)
hbp_massdata <- hbp_massdata %>% dplyr::filter(.data$qaDryMass == 'N' & .data$herbGroup != "Bryophyte") %>% dplyr::select("sampleID", "subsampleID", "herbGroup", "dryMass")
                                        
hbp_perbout <- hbp_perbout %>% dplyr::select("namedLocation", "domainID", "siteID", "plotID", "subplotID", "clipID", "nlcdClass", "plotType", "plotSize", 
                                             "plotManagement", "collectDate", "eventID", "sampleID", "clipArea", "exclosure")

hbp <- dplyr::right_join(hbp_perbout, hbp_massdata, by = "sampleID") %>% 
    dplyr::mutate(dryMass_gm2 = .data$dryMass / .data$clipArea)  # express biomass on a meter squared basis (clipArea is typically either 0.2 or 1 m2)
hbp$peak <- ifelse(hbp$herbGroup == 'All herbaceous plants', "offPeak", "atPeak")
hbp$herbGroup <- gsub("All herbaceous plants", "AllHerbaceousPlants", hbp$herbGroup)
hbp$herbGroup <- gsub("Cool Season Graminoids", "CoolSeasonGraminoids", hbp$herbGroup)
hbp$herbGroup <- gsub("Woody-stemmed Plants", "WoodyStemmedPlants", hbp$herbGroup)
hbp$herbGroup <- gsub("Warm Season Graminoids", "WarmSeasonGraminoids", hbp$herbGroup)
hbp$herbGroup <- gsub("N-fixing Plants", "NFixingPlants", hbp$herbGroup)
hbp$herbGroup <- gsub("Annual and Perennial Forbs", "AnnualAndPerennialForbs", hbp$herbGroup)

hbp_wide <- tidyr::pivot_wider(hbp, id_cols = c("namedLocation","domainID","siteID","plotID","subplotID","clipID","nlcdClass","plotType","plotSize","plotManagement", "collectDate","eventID",
                                                "sampleID","clipArea","exclosure","peak"), 
                               names_from = "herbGroup", names_prefix = "dryMass_gm2_", values_from = "dryMass_gm2")  # transpose functional group rows into separate columns

# aggregate dryMass among herbGroups in peak biomass bouts
hbp_peak_biomass_herb_groups <- hbp %>% dplyr::filter(.data$herbGroup != 'AllHerbaceousPlants') %>% dplyr::mutate(peak = "atPeak")
hbp_peak_biomass_sum_groups <- hbp_peak_biomass_herb_groups %>% dplyr::group_by_at(dplyr::vars("sampleID")) %>% dplyr::summarise(dryMassSum = sum(.data$dryMass_gm2)) 

# populate "All herbaceous plants" column for peak biomass bouts
hbp2 <- merge(hbp_wide, hbp_peak_biomass_sum_groups, by = "sampleID", all.x = TRUE)
hbp2$dryMass_gm2_AllHerbaceousPlants <- ifelse(is.na(hbp2$dryMass_gm2_AllHerbaceousPlants), hbp2$dryMassSum, hbp2$dryMass_gm2_AllHerbaceousPlants)

hbp_standing_biomass_in_clip_cells <- hbp2 %>% dplyr::select(-"dryMassSum") %>%
  tidyr::separate("eventID", into = c('data_prod', 'year','siteID2','bout'), sep = '\\.', remove = FALSE, extra = 'drop')
hbp_standing_biomass_in_clip_cells$siteID2 <- NULL

# group by event ID (that's the bout) and average across clipcells (across plots) within a treatment group (exclosure Y/N)
# Possible for consumption to be negative. To get total NPP = last bout Standing biomass + consumption estimated for each time step. 

hbp_agb <- hbp_standing_biomass_in_clip_cells # %>% dplyr::filter(.data$peak == "atPeak")

hbp_plot <- hbp_standing_biomass_in_clip_cells %>% 
  dplyr::filter(.data$peak == 'atPeak' & .data$exclosure == 'N') %>% 
  dplyr::group_by(.data$siteID, .data$plotID, .data$year, .data$nlcdClass) %>% dplyr::summarise(
  herbPeakMassTotal_gm2 = round(mean(.data$dryMass_gm2_AllHerbaceousPlants, na.rm = TRUE),3), # calculate ave per plot if there are multiple subplots
  herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$dryMass_gm2_CoolSeasonGraminoids, na.rm = TRUE),3),
  herbPeakMassWoodyStemmedPlants_gm2 = round(mean(.data$dryMass_gm2_WoodyStemmedPlants, na.rm = TRUE),3),
  herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$dryMass_gm2_CoolSeasonGraminoids, na.rm = TRUE),3),
  herbPeakMassWarmSeasonGraminoids = round(mean(.data$dryMass_gm2_WarmSeasonGraminoids, na.rm = TRUE),3),
  herbPeakMassNFixingPlants_gm2 = round(mean(.data$dryMass_gm2_NFixingPlants, na.rm = TRUE),3),
  herbPeakMassCoolSeasonGraminoids_gm2 = round(mean(.data$dryMass_gm2_CoolSeasonGraminoids, na.rm = TRUE),3),
  herbPeakMassAnnualAndPerennialForbs_gm2 = round(mean(.data$dryMass_gm2_AnnualAndPerennialForbs, na.rm = TRUE),3),
  ) 
hbp_plot$herbPeakMassTotal_Mgha <- hbp_plot$herbPeakMassTotal_gm2 * 10000 * 0.000001
 # convert g/m2 to Mg/ha ;   g/m2 x 10,000 m2/ha x 0.000001 Mg/g = Mg/ha

priority_plots <- priority_plots # load into environment
hbp_plot <- merge(hbp_plot,priority_plots, by = c("plotID"), all.x = TRUE)
if(plotType == "tower") {hbp_plot <- hbp_plot %>% dplyr::filter(.data$plotType == "tower")} # if plotType argument to function is "tower" then remove distributed plots
if(!is.na(plotPriority)) {hbp_plot <- hbp_plot %>% dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)} # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))

hbp_plot <- hbp_plot %>% dplyr::relocate("herbPeakMassTotal_gm2", .before = "herbPeakMassTotal_Mgha")
hbp_plot <- hbp_plot %>% dplyr::relocate(dplyr::any_of(c("plotType", "specificModuleSamplingPriority")), .after = "nlcdClass")

hbp_plot$year <- as.numeric(hbp_plot$year)
hbp_plot <- hbp_plot[order(hbp_plot$year),] # intent here is to sort by date and then keep latest year

### peak biomass by site and year
hbp_site <- hbp_plot %>% dplyr::group_by(.data$siteID, .data$year) %>% dplyr::summarise(herbPlotNum = length(stats::na.omit(.data$herbPeakMassTotal_gm2)), 
    herbPeakMassMean_gm2 = round(mean(.data$herbPeakMassTotal_gm2, na.rm = TRUE),3), herbPeakMassSD_gm2 = round(stats::sd(.data$herbPeakMassTotal_gm2, na.rm = TRUE),3),
   herbPeakMassMean_Mgha = round(mean(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),3), herbPeakMassSD_Mgha = round(stats::sd(.data$herbPeakMassTotal_Mgha, na.rm = TRUE),3) ) 
 # calc the mean biomass of the individual plots within eventID and identify number of plots contributing to that mean after filtering to just peak biomass bout outside exclosures
  # calculate site-level averages of plots (handling needed to include plots with 0 biomass as with VST?), along with plot min, max, standard deviation and plot count

print("Returning herbaceous biomass output data frames as a list object  ..... ")

output.list <- list(
   hbp_agb = hbp_agb,
   hbp_plot = hbp_plot,
   hbp_site = hbp_site
   )
 return(output.list)
}
