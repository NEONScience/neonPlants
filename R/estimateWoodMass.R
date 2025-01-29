##############################################################################################
#' @title Estimate NEON aboveground biomass (AGB) contributed by woody vegetation

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Use allometric equations to calculate biomass for each VST woody record and summarise biomass by siteID, plotID, and taxonID. 
#' Biomass outputs can, if desired, be used in the follow-up estimateWoodProd productivity function. Data inputs are "Vegetation structure" 
#' (DP1.10098.001) retrieved using the neonUtilities::loadByProduct() function (preferred), or data downloaded from the NEON Data Portal.

#' @details Input data can be filtered by plot type and plot priority. Input data are combined with supplemental static tables 
#' with allometric equation parameters and taxon specific characteristics, and biomass is estimated by allometric equations for each individual. 
#' Generalized allometric equations are applied first, and replaced by taxon specific equations if available. Only the set of growth forms selected via the 
#' growthForm parameter are included in outputs. The nonWoody "cactus" and "ferns" (non-tree) growthForms are not yet included, pending selection of 
#' appropriate allometric equations. Biomass is summarized on an areal basis at the hierarchical level of the plot and site.

#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct function. 
#' If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 
#' 'vst_apparentindividual', 'vst_mappingandtagging', and 'vst_perplotperyear' arguments. [list]
#' @param vst_apparentindividual The 'vst_apparentindividual' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' @param vst_mappingandtagging The 'vst_mappingandtagging' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' @param vst_perplotperyear The 'vst_perplotperyear' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' @param growthForm Select which growth forms to analyse. Options are "all", "tree", "shrubLiana", or "other". [character]
#' 
#' @param plotType Optional filter based on NEON plot type. Defaults to "tower" plots, which are sampled annually. Otherwise "distributed" plots are examined also. [character]
#' @param plotPriority NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is 5. [numeric]

#' @return This function returns a list that includes biomass summary data frames and a helper data frame
#' for companion productivity script.
#' 'vst_agb_per_ha' summarizes above-ground live woody biomass for each individual (units are Megagrams per hectare).
#' 'vst_plot_w_0s' summarizes above-ground live and dead woody biomass for each plot (units are Megagrams per hectare).
#' 'vst_agb_zeros' is a helper for productivity that contains plot by year combos with biomass of 0.
#' 'vst_site' summarizes above-ground live and dead woody biomass for each site (units are Megagrams per hectare).

#' @examples
#' \dontrun{
#' # Obtain NEON Vegetation structure
#' VstDat <- neonUtilities::loadByProduct(dpID="DP1.10098.001", 
#'      package = "basic", check.size = FALSE)
#'      
#' # If list is not in memory, load VST list of dataframes from local file:
#' load('VstDat.rds') # load NEON VST portal data from a local file
#' 
#' # example with arguments at default values
#' estimateWoodMassOutputs <- estimateWoodMass(inputDataList = VstDat)
#' 
#' # example specifying several non-default arguments
#' estimateWoodMassOutputs <- estimateWoodMass(inputDataList = VstDat,
#'                                              growthForm = "tree", plotPriority = 4)
#' 
#' list2env(estimateWoodMassOutputs ,.GlobalEnv) # unlist all data frames
#' saveRDS(estimateWoodMassOutputs, 'estimateWoodMassOutputs.rds') # save all outputs locally
#' }

# changelog and author contributions / copyrights
# Samuel M Simkin (2021-03-30)  original creation
# Samuel M Simkin (2025-01-15)  revised
##############################################################################################

estimateWoodMass = function(inputDataList, 
                       vst_apparentindividual = NA, vst_mappingandtagging = NA, vst_perplotperyear = NA, 
                       growthForm = "all",
                       plotType = "tower",
                       plotPriority = 5
                         ) {

#taxon_tmp <- taxon_tmp # remove this and later reference to taxon_tmp once family and genus in RELEASE data
    
### Verify user-supplied 'inputDataList' object contains correct data if not missing
  if (!missing(inputDataList)) {
    
    #   Check that input is a list
    if (!inherits(inputDataList, "list")) {
      stop(glue::glue("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"))
    }
    
    #   Check that required tables within list match expected names
    listExpNames <- c("vst_apparentindividual", "vst_mappingandtagging", "vst_perplotperyear", "vst_non-woody")
    

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
  if (inherits(inputDataList, "list") & (!is.logical(vst_apparentindividual) | !is.logical(vst_mappingandtagging) | !is.logical(vst_perplotperyear))) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }
  
  ### Verify 'vst_apparentindividual' 'vst_mappingandtagging', and 'vst_perplotperyear' are data frames if 'inputDataList' is missing
  if (is.null(inputDataList) & 
      (!inherits(vst_apparentindividual, "data.frame") | !inherits(vst_mappingandtagging, "data.frame")  | !inherits(vst_perplotperyear, "data.frame"))) {
    
    stop("Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
  }
  

if (inherits(inputDataList, "list")) {
vst_mappingandtagging <- inputDataList$vst_mappingandtagging
vst_perplotperyear <- inputDataList$vst_perplotperyear
vst_apparentindividual <- inputDataList$vst_apparentindividual
'vst_non-woody' <- inputDataList$'vst_non-woody'
}
  

 ### Verify input tables contain required columns and data ####
  
  ### Verify 'vst_mappingandtagging' table contains required data
  #   Check for required columns
  mapExpCols <- c("domainID", "siteID", "plotID", "individualID", "taxonID")

  if (length(setdiff(mapExpCols, colnames(vst_mappingandtagging))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_mappingandtagging':", '{paste(setdiff(mapExpCols, colnames(vst_mappingandtagging)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(vst_mappingandtagging) == 0) {
    stop(glue::glue("Table 'vst_mappingandtagging' has no data."))
  }


  ### Verify 'vst_perplotperyear' table contains required data
  #   Check for required columns
  plotExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana", 
                  "totalSampledAreaFerns", "totalSampledAreaOther")

  if (length(setdiff(plotExpCols, colnames(vst_perplotperyear))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_perplotperyear':", '{paste(setdiff(plotExpCols, colnames(vst_perplotperyear)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(vst_perplotperyear) == 0) {
    stop(glue::glue("Table 'vst_perplotperyear' has no data."))
  }


  ### Verify 'vst_apparentindividual' table contains required data
  #   Check for required columns
  appIndExpCols <- c("domainID", "siteID", "plotID", "individualID", "growthForm", "plantStatus", "date", "eventID", "stemDiameter", "basalStemDiameter")
  
  if (length(setdiff(appIndExpCols, colnames(vst_apparentindividual))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_apparentindividual':", '{paste(setdiff(appIndExpCols, colnames(vst_apparentindividual)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(vst_apparentindividual) == 0) {
    stop(glue::glue("Table 'vst_apparentindividual' has no data."))
  }


### Verify 'vst_non-woody' table contains required data
#   Check for required columns
nonwoodyExpCols <- c("domainID", "siteID", "plotID", "individualID", "growthForm", "plantStatus", "date", "stemDiameter", "basalStemDiameter", "taxonID", 
                       "height", "leafNumber", "meanLeafLength", "meanPetioleLength", "meanBladeLength")
  
#  if (class('vst_non-woody') == "data.frame"){if(length(setdiff(nonwoodyExpCols, colnames('vst_non-woody'))) > 0) {
  if(methods::is('vst_non-woody', class = "data.frame" )){if(length(setdiff(nonwoodyExpCols, colnames('vst_non-woody'))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_non-woody':", '{paste(setdiff(nonwoodyExpCols, colnames(vst_non-woody), collapse = ", ")}',
                    .sep = " "))
  }
}
  
# Error if invalid growthForm option selected
  if(growthForm != "all" & growthForm != "tree" & growthForm != "shrubLiana" & growthForm != "other"){
    stop("Currently the only valid growthForm options are 'all', 'tree', 'shrubLiana', or 'other'.")
  }  

# Error if invalid plotType option selected
  if(plotType != "tower" & plotType != "all"){
    stop("The only valid plotType options are 'tower' or 'all'.")
  }  
  
# Error if invalid plotPriority option selected
  if(plotPriority < 1){
    stop("The minimum plotPriority value is 1, and 5 or greater is the recommended default.")
  }  

  
perplot <- inputDataList$vst_perplotperyear ## read in plot sample area for each growthForm by eventID combo from vst_perplotperyear table
perplot$year <- as.numeric(substr(perplot$eventID,10,13) )
start_from_input <- as.character(min(as.numeric(substr(perplot$year, 1,4)))) # year component of eventID, which on rare occasions may be before the year component of earliest collect date)
end_from_input <- as.character(max(as.numeric(substr(perplot$date, 1,4))))


# obtained "UniquePlotIDsSamplingModulesPriorityLists" from GitHub NEON-OS_spatial_data repo and then filtered and selected to relevant records and columns: 
#                   dplyr::filter(str_detect(.data$specificModule, "vst"))  %>% dplyr::select("plotID","specificModuleSamplingPriority", "plotType") 
#priority_plots <- read.table('../suppl_data/UniquePlotIDsSamplingModulesPriorityLists.NEON-OS_spatial_data.repo.vst.csv', header=TRUE, sep = ",", stringsAsFactors = F) 
  # The specificModuleSamplingPriority field is used in the productivity script to optionally filter only to plots with priority 1-5 (the plots that are most likely to have been sampled the year they were scheduled)
priority_plots_all = data.frame() 
endYear <- as.numeric((as.numeric(end_from_input)) : (as.numeric(start_from_input)) )
for(i in 1:length(endYear)){ # loop through all possible years
priority_plots$year <- endYear[i]
priority_plots_all <- rbind(priority_plots_all, priority_plots)
}
priority_plots_all$eventID <- paste0("vst_",substr(priority_plots_all$plotID,1,4),"_",priority_plots_all$year)
perplot <- merge(perplot, priority_plots_all, by=c("plotID","plotType","eventID","year"), all.x=TRUE)
 perplot$plot_eventID <- paste0(perplot$plotID, "_", perplot$eventID)
perplot <- perplot[order(perplot$date),] # intent here is to sort by date before removing duplicates so that if duplicates are from different dates the record from latest date will be retained
    perplot <- perplot[!duplicated(perplot$plot_eventID, fromLast=TRUE), ] # sorting by date and then using fromLast=TRUE should retain the most recent version of duplicates

perplot_not_SI <- perplot %>% dplyr::filter(.data$samplingImpractical == "OK" | .data$samplingImpractical == "" | is.na(.data$samplingImpractical)) %>% dplyr::filter(.data$year >= as.numeric(start_from_input) & .data$year <= as.numeric(end_from_input) ) # & .data$siteID %in% site)
  # create list of ALL plot by eventID combos from the vst_perplotperyear tables from vst woody, vst non-herb perennial, or both, regardless of whether they have biomass
 plot_eventID_list <- unique(perplot_not_SI$plot_eventID) # list of all unique combos of plotID and eventID where full sampling should have taken place
perplot <- perplot %>% dplyr::select("plot_eventID", "plotID", "eventID", "year", "nlcdClass", "plotType", "eventType", "dataCollected", "targetTaxaPresent", 
                                     "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana", "totalSampledAreaOther")

plotType_df <- inputDataList$vst_perplotperyear 

  plotType_df <- plotType_df %>% dplyr::select("plotID","plotType")
  plotType_df <- plotType_df[!duplicated(plotType_df$plotID),]

#################################################################
#### Calculate biomass from vst_non-woody table
vst_agb_other <- inputDataList$'vst_non-woody'

vst_agb_other <- merge(vst_agb_other, perplot, by=c("plotID", "eventID"), all.x=TRUE) # add total sampled areas
vst_agb_other$agb_source <- "no source"

vst_agb_other$agb_yucca <- ifelse(vst_agb_other$growthForm == "yucca", round(((0.0022*vst_agb_other$height) + (0.00096*(vst_agb_other$height^2)) + 0.04)/1000, digits=6), NA) 
# White, J.D., K.J. Gutzwiller, W.C. Barrow, L.J. Randall, and P. Swint. 2008. Modeling mechanisms of vegetation change due to fire in a semi-arid ecosystem. Ecological Modelling 214:181-200.
#  divide by 1000 to convert from g to kg
vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_yucca), "White_et_al_2008_yucca", vst_agb_other$agb_source)
vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "White_et_al_2008_yucca", vst_agb_other$agb_yucca, NA)

vst_agb_other$tot_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo", exp(-0.2889 + 2.2222*log(vst_agb_other$height) ), NA) 
vst_agb_other$agb_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo", round(1/(exp(-0.63 - 0.18*log(vst_agb_other$tot_ocotillo))+1) * vst_agb_other$tot_ocotillo, digits=6), NA) 
  vst_agb_other$tot_ocotillo <- NULL # don't need total biomass (aboveground + belowground) once we have aboveground biomass
# Bobich, E.G., and T.E. Huxman. 2009. Dry mass partitioning and gas exhange for young ocotillos (Fouquieria splendends) in the Sonoran Desert. International Journal of Plant Science 170:283-289.
# log(height_m) = 0.13 + 0.45 * log(total above and below ground biomass in kg); log(total above and below ground biomass in kg) = (log(height_m) - 0.13)/0.45 = -0.2889 +  (2.2222 * log(height_m) )
# log(root/shoot) = -0.63 + 0.18 * log(total above and below ground biomass in kg)
# aboveground biomass in kg = 1(1+exp(log(root/shoot))) * exp(log(total above and below ground biomass in kg)) = fraction aboveground * total biomass
vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_ocotillo), "Bobich_and_Huxman_2009", vst_agb_other$agb_source)
vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Bobich_and_Huxman_2009", vst_agb_other$agb_ocotillo, vst_agb_other$agb)

vst_agb_other$agb_Gholz_XETE <- ifelse(vst_agb_other$growthForm == "xerophyllum", round((18.873 + (0.0280*((vst_agb_other$basalStemDiameter^2)*vst_agb_other$meanLeafLength)))/1000, digits=6), NA) 
 # Gholz, H.L., C.C. Grier, A.G. Campbell, and A.T. Brown. 1979. Equations for estimating biomass and leaf area of plants in the pacific northwest. Research paper 41. Forest Research Laboratory, School of Forestry at Oregon State University, Corvallis. 
 # use Gholz et al 1979 for Xerophyllum tenax; divide by 1000 to convert from g to kg
vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_Gholz_XETE), "Gholz_et_al_1979_XETE", vst_agb_other$agb_source)
vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Gholz_et_al_1979_XETE", vst_agb_other$agb_Gholz_XETE, vst_agb_other$agb)

# most NEON records with growthFrom "small palm" are Serenoa repens: A small number are Coccothrinax barbadensis, Leucothrinax morrisii, or Sabal etonia
vst_agb_other$agb_palm <- ifelse(!is.na(vst_agb_other$meanPetioleLength) & vst_agb_other$growthForm == "small palm", round((exp(-10.38 + 2.72*log(vst_agb_other$meanPetioleLength)) + (-13.31 + 0.85*vst_agb_other$meanBladeLength))/1000, digits=6), NA) 
 # Gholz, H.L., D.N. Guerin, and W.P. Cropper. 1999. Phenology and productivity of saw palmetto (Serenoa repens) in a north Florida slash pine plantation. Canadian Journal of Forest Research 29:1248-1253.
 # use Gholz et al 1999 for Serenoa repens (and other palms); separate equations for rachis/petiole biomass (g) and blade/leaf biomass (g); add together and divide by 1000 to get total biomass (kg)
vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_palm), "Gholz_et_al_1999", vst_agb_other$agb_source)
vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Gholz_et_al_1999", vst_agb_other$agb_palm, vst_agb_other$agb)

vst_agb_other$agb_Cibotium <- ifelse(substring(vst_agb_other$scientificName,1,8) == "Cibotium", round(0.2085*(pi*(vst_agb_other$stemDiameter/2)^2*vst_agb_other$height*100*0.22/1000), digits=3), NA)
   # hard code Cibotium wood density (spg_gcm3) as 0.22, the value for Cibotium glaucum
  vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_Cibotium), "Ostertag_et_al_2014", vst_agb_other$agb_source)
 vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Ostertag_et_al_2014", vst_agb_other$agb_Cibotium, vst_agb_other$agb)
 # Cibotium genus (taxonIDs CIBOT, CIGL, and CIME8) has allometric equation in Ostertag et al 2014 (in suppl material)
 # citation: Ostertag, R, F Inman-Narahari, S Cordell, CP Giardina, and L Sack. 2014. Forest Structure in low-diversity tropical forests: A study of Hawaiian wet and dry forests. PLOS One. 9:e103268


vst_agb_other$plot_eventID <- paste0(vst_agb_other$plotID, "_", vst_agb_other$eventID)
agb_other_plot_eventID <- unique(vst_agb_other$plot_eventID)
vst_agb_other$year <- as.numeric(substr(vst_agb_other$eventID,10,13) )
vst_agb_other <- vst_agb_other %>% dplyr::filter(!is.na(.data$plantStatus) & .data$plantStatus != "No longer qualifies" & .data$plantStatus != "Removed" & .data$plantStatus != "Lost, fate unknown" & 
                  .data$plantStatus != "Lost, tag damaged" & .data$plantStatus != "Lost, herbivory" & .data$plantStatus != "Lost, burned" & .data$plantStatus != "Lost, presumed dead" & 
                  .data$plantStatus != "Downed") # remove records with status that doesn't include unambiguous live or dead individuals
vst_agb_other$plantStatus2 <- dplyr::if_else(vst_agb_other$plantStatus %in% c("Live", "Live, disease damaged", "Live, insect damaged", "Live,  other damage", "Live, physically damaged", "Live, broken bole"), "Live", "Dead_or_Lost", "Live")
vst_agb_other <- vst_agb_other %>% dplyr::filter(!is.na(.data$agb) ) # remove records that have NA for biomass, before they become misleading zeros as a result of group_by function
vst_agb_other <- vst_agb_other %>% dplyr::select(-"uid",-"namedLocation",-"publicationDate",-"stemCount",-"branchCount",-"meanBranchLength",-"identificationReferences",
             -"identificationQualifier",-"morphospeciesID",-"measuredBy",-"recordedBy",-"subplotID")
# Aggregate vst non-herbaceous perennial (other) biomass data and express on a Mg/ha basis
vst_agb_final_other <- vst_agb_other %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$taxonID, .data$individualID, .data$plantStatus2, .data$growthForm, .data$year) %>% dplyr::summarise(agb = sum(.data$agb, na.rm = TRUE)) 
  # if there are multiple records per individualID, sum them
 vst_agb_final_other  <- merge(vst_agb_final_other, perplot, by=c("plot_eventID", "eventID", "year", "plotID"), all.x=TRUE) # add total sampled areas
  vst_agb_final_other <- vst_agb_final_other %>% dplyr::filter(!is.na(.data$totalSampledAreaOther) ) # remove records that can't be scaled to area basis
    vst_agb_final_other$agb_Mgha <- round(vst_agb_final_other$agb * 0.001 * (10000/vst_agb_final_other$totalSampledAreaOther), 5)

    vst_agb_final_other <- vst_agb_final_other %>% dplyr::filter(.data$growthForm != "cactus" & .data$growthForm != "fern") # waiting to pick appropriate allometric equations for these growthForms

# vst_agb_per_ha_other <- vst_agb_final_other %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, .data$individualID, .data$plantStatus2, .data$year) %>% dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE)) %>% dplyr::ungroup()
#     # add up biomass per unit area for each plot x individualID x year x plantStatus2 x nlcdClass x taxonID combo (growthForm no longer needed since area has been calculated in previous steps)

vst_agb_per_ha_other <- vst_agb_final_other %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, .data$growthForm, .data$individualID, .data$plantStatus2, .data$year) %>% dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE)) %>% dplyr::ungroup()
    # add up biomass per unit area for each plot x individualID x year x plantStatus2 x nlcdClass x taxonID x growthForm combo

#################################################################
#### Calculate biomass for vst_apparentindividual table
## read in taxonID from vst_mappingandtagging table 
map <- inputDataList$vst_mappingandtagging 
map <- map[order(map$date),]
map <- map[!duplicated(map$individualID, fromLast=TRUE), ] # keep the most recent record which should have a more specific or considered taxonID; in QA older dupes are periodically deleted
#map <- merge(map, taxon_tmp, by = "taxonID") ### temporary until family and genus are in RELEASE data
    taxonID_df <- map %>% dplyr::select("taxonID", "scientificName", "family", "genus", "taxonRank") %>% unique()  
    vst_taxonIDs <- taxonID_df$taxonID
 map <- map %>% dplyr::select("individualID", "taxonID")

appInd <- inputDataList$vst_apparentindividual
appInd <- appInd %>% dplyr::filter(as.numeric(substr(.data$date,1,4)) >= as.numeric(start_from_input) & as.numeric(substr(.data$date,1,4)) <= as.numeric(end_from_input) ) # & .data$siteID %in% site)


print("Assembling allometric equation parameters ..... ")

# temporary fix of some eventIDs until fixes are made to portal data
appInd$month <- as.numeric(substr(appInd$date, 6, 7))
appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2019 & as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "WREF", "vst_WREF_2019", appInd$eventID) # fixes 182 records
appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & as.numeric(substr(appInd$eventID, 10, 13)) == 2016 & appInd$siteID == "UKFS", "vst_UKFS_2018", appInd$eventID) # fixes 73 records
appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & appInd$month >= 7 & as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "RMNP" , "vst_RMNP_2018", appInd$eventID) # fixes 398 records
appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & appInd$month >= 7 & as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "UNDE" , "vst_UNDE_2018", appInd$eventID) # fixes 300 records
appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2017 & appInd$month == 12 & as.numeric(substr(appInd$eventID, 10, 13)) == 2018 & appInd$siteID == "GUAN" , "vst_GUAN_2017", appInd$eventID) # fixes 115 records
appInd$month <- NULL

## Merge vst_apparentindividual table with map and perplot to obtain taxonID field and area fields
appInd <- merge(appInd, map, by="individualID", all.x=TRUE) # add taxonID to appInd table
appInd <- merge(appInd, perplot, by=c("plotID", "eventID"), all.x=TRUE) # add total sampled areas
appInd$plotEvent <- paste(appInd$plotID, appInd$eventID, sep="_")
appInd$indEvent <- paste(appInd$individualID, appInd$eventID, sep="_")
  appInd_miss_taxonID <- appInd %>% dplyr::filter(is.na(.data$taxonID)) # create list of records that are missing taxonID
appInd$taxonID <- ifelse(is.na(appInd$taxonID), "2PLANT", appInd$taxonID) # if taxonID missing then assign the unknown "2PLANT" code
 appInd$taxonID <- ifelse(appInd$taxonID == "BEGL/BENA", "BEGL", appInd$taxonID) # Betula glandulosa - B. nana complex, assign to B. glandulosa

## read in the Chojnacky et al 2014 parameters for each of their 35 defined allometric groups
#parameters <- read.csv('../suppl_data/chojnacky_parameters.csv', stringsAsFactors = F)
parameters<- parameters %>% dplyr::select("allometry_ID", "b0", "b1", "minDiameter", "maxDiameter")
  
## load wood density, veg type, and other data needed to assign species to Chojnacky allometry groups
taxon_fields <- taxon_fields
  taxon_fields_list <- unique(taxon_fields$taxonID)
  
## load USDA Plants characteristics to get PLANTS.Floristic.Area and Native.Status  - filtered to records that have PLANTS.Floristic.Area, Native.Status, or both
plantIntTrop <- plantIntTrop # load from data folder
plant_char <- merge(taxonID_df, plantIntTrop, by="taxonID", all.x=TRUE) # add tropical floristic area and/or introduced status

## programatically assign a Chojnacky allometry_ID based on genus, family, specific gravity, deciduous vs. evergreen, and/or woodland vs. forest habit
Choj <- merge(taxon_fields,plant_char, by="taxonID", all=TRUE)
 Choj <- Choj[Choj$taxonID %in% vst_taxonIDs,] # retain only taxonIDs found in the vst mapping and tagging data 
Choj$nativeStatus <- dplyr::if_else(Choj$nativeStatus=="int", "introduced", "native", "native")
Choj$tropical <- dplyr::if_else(Choj$tropical == "trop", "tropical", "temperate", "temperate")
Choj$allometry_ID <- NA
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Abies" & Choj$spg_gcm3<0.35, "C1", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Abies" & Choj$spg_gcm3>=0.35, "C2", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$family=="Cupressaceae" & Choj$spg_gcm3<0.30, "C3", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$family=="Cupressaceae" & Choj$spg_gcm3>=0.30 & Choj$spg_gcm3<0.40, 
  "C4", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$family=="Cupressaceae" & Choj$spg_gcm3>=0.40, "C5", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Larix", "C6", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Picea" & Choj$spg_gcm3<0.35, "C7", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Picea" & Choj$spg_gcm3>=0.35, "C8", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Pinus" & Choj$spg_gcm3<0.45, "C9", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Pinus" & Choj$spg_gcm3>=0.45, "C10", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & (Choj$genus=="Pseudotsuga" | Choj$genus=="Taxus" | 
    Choj$genus=="Pseudotsuga"), "C11", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Tsuga" & Choj$spg_gcm3<0.40, "C12", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Tsuga" & Choj$spg_gcm3>=0.40, "C13", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$family=="Aceraceae" & Choj$spg_gcm3<0.50, "H1", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$family=="Aceraceae" & Choj$spg_gcm3>=0.50, "H2", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Betulaceae" & Choj$spg_gcm3<0.40, "H3", 
    Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Betulaceae" & Choj$spg_gcm3>=0.40 & 
    Choj$spg_gcm3<0.50, "H4", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Betulaceae" & Choj$spg_gcm3>=0.50 & 
    Choj$spg_gcm3<0.60, "H5", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Betulaceae" & Choj$spg_gcm3>=0.60, 
   "H6", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((Choj$family=="Cornaceae" | Choj$family=="Ericaceae" | Choj$family=="Lauraceae" | 
    Choj$family=="Platanaceae" | Choj$family=="Rosaceae" | Choj$family=="Ulmaceae"), "H7", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$genus=="Carya", "H8", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & (Choj$family=="Fabaceae" | Choj$family=="Juglandaceae") & 
  Choj$genus!="Carya", "H9", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$family=="Fagaceae" & Choj$decid_vs_ever=="decid", 
  "H10", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="forest" & Choj$family=="Fagaceae" & Choj$decid_vs_ever=="ever", 
  "H11", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$family=="Hamamelidaceae", "H12", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((Choj$family=="Hippocastanaceae" | Choj$family=="Tiliaceae"), "H13", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$family=="Magnoliaceae", "H14", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Oleaceae" & Choj$spg_gcm3<0.55, "H15", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Oleaceae" & Choj$spg_gcm3>=0.55, "H16", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Salicaceae" & Choj$spg_gcm3<0.35, "H17", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID=="") & Choj$family=="Salicaceae" & Choj$spg_gcm3>=0.35, "H18", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="woodland" & Choj$family=="Cupressaceae", "W1", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="woodland" & (Choj$family=="Fabaceae" | Choj$family=="Rosaceae"), 
  "W2", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="woodland" & Choj$family=="Fagaceae", "W3", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest=="woodland" & Choj$family=="Pinaceae", "W4", Choj$allometry_ID) 
Choj$allometry_ID <- ifelse(Choj$taxonID=="PINACE", "C9", Choj$allometry_ID) 
 # arbitrarily picked C9 (forest) over C10 (forest spg_gcm3>=0.45) or W4 (woodland)
Choj$allometry_ID <- ifelse(Choj$taxonID=="FABACE", "H9", Choj$allometry_ID) 
 # arbitrarily picked H9 (forest) over W2 (woodland)
Choj$source <- ifelse(!is.na(Choj$allometry_ID), "yes_ref_in_Choj", "not_ref_in_Choj")
Choj$allometry_ID <- ifelse(Choj$source=="not_ref_in_Choj", "H7", Choj$allometry_ID) 
no_Choj_allometry <- Choj %>% dplyr::filter(.data$source=="not_ref_in_Choj") %>% dplyr::select("allometry_ID", "taxonID", "family", "genus", "spg_gcm3", 
              "woodland_vs_forest", "decid_vs_ever", "tropical", "nativeStatus") # create list records where Choj allometry can't be calculated
Choj <- Choj %>% dplyr::select("taxonID","scientificName","allometry_ID","source","spg_gcm3","nativeStatus","tropical","family")
Choj <- merge(parameters, Choj, by="allometry_ID", all.y=TRUE)

## calculate biomass from various allometric equations after making some data corrections
vst_agb <- merge(appInd, Choj, by="taxonID", all.x=TRUE)

print("Calculating biomass from vst_apparentindividual table ..... ")

# code below includes a variety of assumptions (including estimating dbh from basal stem diameter or height) that provide estimates that should be better than the missing (usually)
# values that they replace. Reasoning here is that a biomass value with some positive value and percent error due to assumptions made is an improvement over a value of 0 whenever data needed for allometric 
# equation is missing.
 
# create stemDiameterFlag to flag which stemDiameters are estimates based on other fields such basalStemDiameter
vst_agb$stemDiameterFlag <- ifelse(is.na(vst_agb$stemDiameter), "estimate", "raw")

vst_agb$tropical <- ifelse( (vst_agb$taxonID == "2PLANT" | vst_agb$taxonID == "2PLANT-H" | vst_agb$taxonID == "ANAL12" | vst_agb$taxonID == "BOURR" | vst_agb$taxonID == "BUMI6" | vst_agb$taxonID == "CONVOL" | 
    vst_agb$taxonID == "CROSS" | vst_agb$taxonID == "FABACE" | vst_agb$taxonID == "JACQU" | vst_agb$taxonID == "JACQU2" | vst_agb$taxonID == "COPRO" | vst_agb$taxonID == "HYDRAN") &  
    (vst_agb$siteID == "GUAN" | vst_agb$siteID == "PUUM" | vst_agb$siteID == "LAJA"), 
    "tropical", vst_agb$tropical) # manually assign tropical for a handful of taxonIDs
 vst_agb$tropical <- ifelse( (vst_agb$taxonID == "AMAR5" | vst_agb$taxonID == "CELTI" | vst_agb$taxonID == "DAWR2" | vst_agb$taxonID == "LIJA" | vst_agb$taxonID == "MEAZ" | vst_agb$taxonID == "OPUNT" | 
    vst_agb$taxonID == "RHUS" | vst_agb$taxonID == "SAMBU" | vst_agb$taxonID == "SMSM" | vst_agb$taxonID == "SYMPL2" | vst_agb$taxonID == "VITIS") &  
    (vst_agb$siteID != "GUAN" & vst_agb$siteID != "PUUM" & vst_agb$siteID != "LAJA"), 
    "temperate", vst_agb$tropical) # manually assign temperate for a handful of taxonIDs
vst_agb$spg_gcm3 <- as.numeric(vst_agb$spg_gcm3)

# assumption: for tropical species, if specific gravity is not known then assume it is 0.5 g/cm3 to permit usage of Chave et al 2014, following precedent of Asner et al 2011
vst_agb$spg_gcm3 <- dplyr::if_else(is.na(vst_agb$spg_gcm3) & vst_agb$tropical =="tropical", 0.5, vst_agb$spg_gcm3, vst_agb$spg_gcm3) # default specific gravity used by Asner et al 2011 in the tropics
vst_agb$growthForm <- dplyr::if_else(is.na(vst_agb$growthForm), "unknown", vst_agb$growthForm, vst_agb$growthForm)

# assumption: missing stemDiameter values can be inferred from basalStemDiameter values and this is desirable since the allometric equations assume diameter at breast height rather than diameter at base
 vst_agb$stemDiameter <- dplyr::if_else(is.na(vst_agb$stemDiameter), exp(-0.35031 + 1.03991*log(vst_agb$basalStemDiameter)), vst_agb$stemDiameter, vst_agb$stemDiameter) # from Chojnacky et al 2014?
 
# assumption: if actual or estimated stemDiameter values are smaller than the minimum for their growthForm then replace with the minimum for that growthForm because record below the minimum wouldn't have been measured
vst_agb$stemDiameter <- dplyr::if_else((vst_agb$growthForm == "sapling" | vst_agb$growthForm == "small shrub") & vst_agb$stemDiameter < 0.1, 0.1, vst_agb$stemDiameter, vst_agb$stemDiameter) 
vst_agb$stemDiameter <- dplyr::if_else(!(vst_agb$growthForm == "sapling" | vst_agb$growthForm == "small shrub" | vst_agb$growthForm == "liana") & vst_agb$stemDiameter < 1 & is.na(vst_agb$basalStemDiameter), 1, 
                                vst_agb$stemDiameter, vst_agb$stemDiameter) 

vst_agb$stemDiameter <- dplyr::if_else( (vst_agb$stemDiameter > vst_agb$basalStemDiameter) & (vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub"), vst_agb$basalStemDiameter, vst_agb$stemDiameter, vst_agb$stemDiameter)

vst_agb_no_dia <- vst_agb %>% dplyr::filter(is.na(.data$stemDiameter) & is.na(.data$basalStemDiameter)) # create list of records with missing diameters

# assumption: it's better to replace extreme outlier stemDiameter values of lianas with a low-end estimate of 1 cm than to leave in place extremely large values or to assign a value of 0 or NA
    vst_agb$stemDiameter <- dplyr::if_else(vst_agb$growthForm =="liana" & vst_agb$stemDiameter > 20, 1, vst_agb$stemDiameter, vst_agb$stemDiameter) # instead of removing arbitrarily give them new diameter of 1 cm
          # there are 4 liana records with stemDiameter = 130 (looks like Field Science recorded measurementHeight in stemDiameter field here) and 2 other Vitis and Parthenocissus liana records greater than 20 cm assumed to be errors
          # should sometime assess in depth what diameter of liana is implausibly large and either add back in the 2 between 25 and 41 cm, or exclude additional stems less than 20 cm

vst_agb <- vst_agb %>% dplyr::select("taxonID", "scientificName", "individualID", "indEvent", "eventID", "date", "siteID","plotID", "growthForm", "nlcdClass", 
          "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana","plantStatus", "stemDiameter", "stemDiameterFlag", "height", 
          "measurementHeight", "basalStemDiameter", "basalStemDiameterMsrmntHeight","maxCrownDiameter","ninetyCrownDiameter","allometry_ID", "b0", "b1", 
          "minDiameter", "maxDiameter", "spg_gcm3","nativeStatus","tropical","family")
vst_agb$plantStatus2 <- ifelse(vst_agb$plantStatus %in% c("Live", "Live, disease damaged", "Live, insect damaged", "Live,  other damage", "Live, physically damaged","Live, broken bole"), "Live", "Dead_or_Lost")

## calculate AGB for each VST appInd record using Choj allometry_ID and Choj parameters  
# assumption: Chojnacky et al 2014 allometric equations are the best first estimate of biomass
 vst_agb$agb<- round(exp(vst_agb$b0 + vst_agb$b1*log(vst_agb$stemDiameter)), digits=3) 
   # calculate aboveground biomass based on Chojnacky et al 2014, using paramaters b0 and b1 assigned based on the allometry group that each species assigned to in Choj df
 vst_agb$agb_source <- "Chojnacky_et_al_2014"
 vst_agb$agb_Chojnacky  <- vst_agb$agb # Possible to calculate a Chojnakcy et al 2014 agb value for every record, but it was not intended for tropical or introduced species so subsequent steps update where appropriate
 
# assumption: when the necessary ancillary variables are available for tropical species, replace the Chojnacky et al 2014 biomass estimates with the Chave et al 2014 biomass estimates
 # update tropical species records based on Chave et al 2014 if wood specific gravity (or an approximation based on congeners) was available
 # instructions on extracting environmental stress value E at http://chave.ups-tlse.fr/pantropical_allometry.htm
 # Chave et al 2014 has pantropical allometric equations for tree biomass that require tree height. If tree height not available estimate it using their value E
 # install.packages("raster"); install.packages("ncdf4"); library("raster"); library("ncdf4")
 # source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
 # coord <- data.frame(siteID = c("GUAN", "LAJA", "PUUM"), longitude = c(-66.8687, -67.07689, -155.31731), latitude = c(17.96955, 18.02126, 19.55309) );  rownames(coord) <- coord$siteID; coord$siteID <- NULL
 # Chave_et_al_2014_E <- retrieve_raster("E",coord,plot=TRUE,format="nc") returns an E of 0.5074847 for GUAN, 0.4440793 for LAJA, and NA for PUUM
vst_agb$Chave_E <- ifelse(vst_agb$siteID == "GUAN", 0.5074847, NA)                # see code chunk on what Chave environmental stress E value is and how it was acquired
vst_agb$Chave_E <- ifelse(vst_agb$siteID == "LAJA", 0.4440793, vst_agb$Chave_E)   # see code chunk on what Chave environmental stress E value is and how it was acquired
vst_agb$height_eval <- ifelse(is.na(vst_agb$height), 0, 1 )
vst_agb$agb_trop <- ifelse(vst_agb$tropical == "tropical" & vst_agb$spg_gcm3 >= 0 & vst_agb$stemDiameter >= 0, 
   round(exp(-1.803 - (0.976*vst_agb$Chave_E) + (0.976*log(vst_agb$spg_gcm3)) + (2.673*log(vst_agb$stemDiameter)) - (0.0299*(log(vst_agb$stemDiameter))^2)  ), digits=3), NA)  
vst_agb$agb_trop <- dplyr::if_else(vst_agb$tropical == "tropical" & vst_agb$height_eval == 1 & vst_agb$spg_gcm3 >= 0 & vst_agb$stemDiameter >= 0, 
   round(0.0673 * (vst_agb$spg_gcm3*(vst_agb$stemDiameter^2) * vst_agb$height)^0.976, digits=3), vst_agb$agb_trop, vst_agb$agb_trop) #  & vst_agb$siteID != "GUAN" & vst_agb$siteID != "LAJA" # heights often inaccurate in D04 (GUAN and LAJA)
vst_agb$height_eval <- NULL # just needed this variable temporarily to allow ifelse statement to work on all records
  # Chojnacky is not intended for tropical species, so where possible use Chave et al 2014 which does have specific equations for tropical species
  # citation: Chave et al 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20:3177-3190
vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_trop), "Chave_et_al_2014", vst_agb$agb_source)
vst_agb$agb <- ifelse(vst_agb$agb_source == "Chave_et_al_2014", vst_agb$agb_trop, vst_agb$agb)

## Several shrub-specific equations from Conti et al. 2019 applied below
# if shrub basal diameter is NOT available but crown diameter and height are available
suppressWarnings(  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") & (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) & !is.na(vst_agb$height)),
                                      round(exp(-0.370 + 1.903 * log((vst_agb$maxCrownDiameter+vst_agb$ninetyCrownDiameter)/2) + 0.652 * log(vst_agb$height)) * 1.403, digits=3), NA)  )
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub), "Conti_et_al_2019", vst_agb$agb_source)
 vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019", vst_agb$agb_shrub, vst_agb$agb, vst_agb$agb)
 # citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to 
 #   predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.

# if only shrub basal diameter is available (improvement over crown diameter + height model)
suppressWarnings(  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") & !is.na(vst_agb$basalStemDiameter),
                                      round(exp(-2.869 + 2.584 * log(vst_agb$basalStemDiameter)), digits=3), vst_agb$agb_shrub)  )
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub), "Conti_et_al_2019", vst_agb$agb_source)
 vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019", vst_agb$agb_shrub, vst_agb$agb, vst_agb$agb)
 # citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to 
 #   predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.

# if shrub basal diameter AND crown diameter are available (improvement over just basal diameter)
suppressWarnings(  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") & (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) & !is.na(vst_agb$basalStemDiameter)),
                                      round(exp(-2.057 + 1.741 * log(vst_agb$basalStemDiameter) + 0.945 * log((vst_agb$maxCrownDiameter+vst_agb$ninetyCrownDiameter)/2)), digits=3), vst_agb$agb_shrub)  )
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub), "Conti_et_al_2019", vst_agb$agb_source)
 vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019", vst_agb$agb_shrub, vst_agb$agb, vst_agb$agb)
 # citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to 
 #   predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.

# if shrub basal diameter, crown diameter, AND height are all available (best Conti et al 2019 model)
suppressWarnings(  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") & (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) & 
                                        !is.na(vst_agb$basalStemDiameter) & !is.na(vst_agb$height)),
                                      round(exp(-2.281 + 1.525 * log(vst_agb$basalStemDiameter) + 0.831 * log((vst_agb$maxCrownDiameter+vst_agb$ninetyCrownDiameter)/2) + 0.523 * log(vst_agb$height)), digits=3), vst_agb$agb_shrub)  )
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub), "Conti_et_al_2019", vst_agb$agb_source)
 vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019", vst_agb$agb_shrub, vst_agb$agb, vst_agb$agb)
 # citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to 
 #   predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.

## assumption: where available, species-specific allometric equations are preferable to more generic ones, so update aboveground biomass where species - specific allometric equations exist (see multiple taxa below)
vst_agb$agb_MEPO5_Litton <- ifelse(vst_agb$taxonID == "MEPO5", round(0.88*(vst_agb$stemDiameter^1.86), digits=3), NA) # for MEPO5 with stemDiameter > 33 and no height value
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_MEPO5_Litton), "Litton_and_Kauffman_2008", vst_agb$agb_source)
 vst_agb$agb <- ifelse(vst_agb$agb_source == "Litton_and_Kauffman_2008", vst_agb$agb_MEPO5, vst_agb$agb)
  # taxonID MEPO5 is Metrosideros polymorpha, the most frequent tropical species in NEON VST dataset, and Litton and Kauffman 2008 have specific equation for MEPO5
  # citation: Litton and Kauffman 2008. Allometric Models for Predicting Aboveground Biomass in Two Widespread WoodyPlants in Hawaii. BIOTROPICA 40(3): 313-320.

vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter <=33, round(0.2085*(vst_agb$stemDiameter^2.318), digits=3), NA)
vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter >33, round(0.0776*((vst_agb$spg_gcm3*(vst_agb$stemDiameter^2)*vst_agb$height)^0.94), digits=3), vst_agb$agb_MEPO5)
vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_MEPO5), "Selmants_et_al_2014", vst_agb$agb_source)
 vst_agb$agb <- ifelse(vst_agb$agb_source == "Selmants_et_al_2014", vst_agb$agb_MEPO5, vst_agb$agb)
 # taxonID MEPO5 is Metrosideros polymorpha, the most frequent tropical species in NEON VST dataset, and Selmants et al 2014 (in suppl material) have specific equation for MEPO5
 # citation: Selmants, PC, CM Litton, CP Giardina, and GP Asner. 2014. Global Change Biology 20:2927-2937.

vst_agb$agb_RHDA <- ifelse(vst_agb$taxonID == "RHDA", round(0.001*exp(((5.237 + 1.996*log(vst_agb$stemDiameter))+(5.016 + 2.306*log(vst_agb$stemDiameter)))/2), digits=3), NA)
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_RHDA), "Zhang_et_al_2012", vst_agb$agb_source)
 vst_agb$agb <- ifelse(vst_agb$agb_source == "Zhang_et_al_2012", vst_agb$agb_RHDA, vst_agb$agb)
 # taxonID RHDA is Rhamnus davurica, the most frequent introduced species in NEON VST dataset, and Zhang et al 2012 have specific equation for RHDA
  # first equation is for males, second equation is for females. Took the average since sex was not recorded. Divided by 1000 to convert from g to kg used for Chojnacky et al 2014
  # citation: Zhang et al 2012. Sexual dimorphism in reproductive and vegetative allometry for two dioecious Rhamnus plants in north-eastern China. Eur J Forest Res (2012) 131:1287-1296

suppressWarnings(  vst_agb$agb_ARTR2 <- ifelse(vst_agb$taxonID == "ARTR2", round(exp(7.889 + 0.8539*log(4/3*pi*(vst_agb$maxCrownDiameter/2)*(vst_agb$ninetyCrownDiameter/2)))/1000, digits=3), NA)  )
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_ARTR2), "Cleary_et_al_2008_ARTR2", vst_agb$agb_source)
 vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Cleary_et_al_2008_ARTR2", vst_agb$agb_ARTR2, vst_agb$agb, vst_agb$agb)
 # taxonID ARTR2 is Artemisia tridentata, the most frequent shrub in NEON VST dataset - divide final result by 1,000 to convert from g to kg
 # citation: Cleary, M.B., E. Pendall, and B.E. Ewers. 2008. Testing sagebrush allometric relationships across three fire chronosequences in Wyoming, USA. Journal of Arid Environments 72:285-301

vst_agb$agb_LATR2 <- ifelse(vst_agb$taxonID == "LATR2", round((24.76 + 0.0014*(4/3*pi*(vst_agb$maxCrownDiameter*100)*(vst_agb$ninetyCrownDiameter*100)))/1000, digits=3), NA)
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_LATR2), "Huenneke_et_al_2001_LATR2", vst_agb$agb_source)
 vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Huenneke_et_al_2001_LATR2", vst_agb$agb_LATR2, vst_agb$agb, vst_agb$agb)
 # taxonID LATR2 is Larrea tridentata, the third most frequent shrub in NEON VST dataset - multiple diameters by 100 to convert from m to cm and divide final result by 1,000 to convert from g to kg
 # citation: Huenneke, L.F., D. Clason, and E. Muldavin. 2001. Spatial heterogeneity in Chihuahan Desert vegetation: implications for sampling methods in semi-arid ecosystems. Journal of Arid Environments 47:257-270
 # equation estimated from regression line in Figure 3a

vst_agb$agb_Cornus <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") & substr(vst_agb$scientificName,1,6) == "Cornus", round(exp(3.315 + 2.647*log(vst_agb$basalStemDiameter))/1000, digits=3), NA)
vst_agb$agb_Cornus <- dplyr::if_else((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") & substr(vst_agb$scientificName,1,6) == "Cornus" & is.na(vst_agb$basalStemDiameter), round(exp(5.089 + 1.883*log(vst_agb$stemDiameter))/1000, digits=3),
        vst_agb$agb_Cornus, vst_agb$agb_Cornus)
 vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_Cornus), "Lutz_et_al_2014", vst_agb$agb_source)
 vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Lutz_et_al_2014", vst_agb$agb_Cornus, vst_agb$agb, vst_agb$agb)
 # apply Cornus sericea equation to other Cornus shrubs
 # citation: Lutz, J.A., K.A. Schwindt, T.J. Furniss, J.A. Freund, M.E Swanson, K.J. Hogan, G.E. Kenagy, and A.J. Larson. 2014. Community composition and allometry of Leucothoe davisiae, Cornus sericea, and Chrysolepis sempervirens. Canadian
 #   Jornal of Forest Research 44:677-683

# assumption: allometric equations developed specifically for lianas are better than generic allometric equations used above for trees and shrubs
# update aboveground biomass for lianas from Schnitzer_et_al_2006 (Chojnacky is not intended for lianas, or for introduced or tropical species, and there are numerous introduced and tropical liana species, see below)
vst_agb$agb_liana <- ifelse(vst_agb$growthForm=="liana", round(exp(-1.484 + 2.657*log(vst_agb$stemDiameter)), digits=3), NA)
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_liana), "Schnitzer_et_al_2006", vst_agb$agb_source)
  vst_agb$agb <- ifelse(vst_agb$agb_source == "Schnitzer_et_al_2006", vst_agb$agb_liana, vst_agb$agb)
  # taxonID CEOR7 (Celastrus orbiculatus) & LOJA (Lonicera japonica) are 2nd & 3rd most frequent introduced species in NEON VST dataset, and they are lianas
  # taxonID DILA10 (Distictis lactiflora) is the most frequent tropical species, and is a liana
  # Citation: Schnitzer, SA, SJ DeWalt, and J Chave. 2006. Censusing and measuring lianas: A quantitative comparison of the common methods. Biotropica 38:581-591
  # Schnitzer_et_al_2006 develop allometric equation for lianas in several tropical regions, and I'm applying to temperate lianas as well

  vst_agb$agb_Cibotium <- ifelse(substring(vst_agb$scientificName,1,8) == "Cibotium", round(0.2085*(pi*(vst_agb$stemDiameter/2)^2*vst_agb$height*100*vst_agb$spg_gcm3/1000), digits=3), NA)
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_Cibotium), "Ostertag_et_al_2014", vst_agb$agb_source)
 vst_agb$agb <- ifelse(vst_agb$agb_source == "Ostertag_et_al_2014", vst_agb$agb_Cibotium, vst_agb$agb)
 # Cibotium genus (taxonIDs CIBOT, CIGL, and CIME8) has allometric equation in Ostertag et al 2014 (in suppl material)
 # citation: Ostertag, R, F Inman-Narahari, S Cordell, CP Giardina, and L Sack. 2014. Forest Structure in low-diversity tropical forests: A study of Hawaiian wet and dry forests. PLOS One. 9:e103268

# additional quality flag(s)
vst_agb$agb_in_dia_range <- dplyr::if_else(vst_agb$stemDiameter >= vst_agb$minDiameter & vst_agb$stemDiameter <= vst_agb$maxDiameter & vst_agb$agb_source == "Chojnacky_et_al_2014", "yes", "no", "")
 vst_agb$agb_in_dia_range <- ifelse(vst_agb$agb_source != "Chojnacky_et_al_2014", "NA", vst_agb$agb_in_dia_range)

# assumption: it's better to make assumptions about growthForm based on stemDiameter than to leave out records when scaling to area basis (which requires knowing what the the growthForm is)
vst_agb$growthForm <- dplyr::if_else(vst_agb$growthForm == "unknown" & vst_agb$stemDiameter >= 10, "single bole tree", vst_agb$growthForm, vst_agb$growthForm )
vst_agb$growthForm <- dplyr::if_else(vst_agb$growthForm == "unknown" & vst_agb$stemDiameter < 10, "small tree", vst_agb$growthForm, vst_agb$growthForm )

vst_agb$plot_eventID <- paste0(vst_agb$plotID, "_", vst_agb$eventID)
    vst_agb$year <- as.numeric(substr(vst_agb$eventID,10,13) )

vst_agb <- vst_agb %>% dplyr::filter(!is.na(.data$plantStatus) & .data$plantStatus != "No longer qualifies" & .data$plantStatus != "Removed" & .data$plantStatus != "Lost, fate unknown" & .data$plantStatus != "Lost, tag damaged" & .data$plantStatus != "Lost, herbivory" & 
                                .data$plantStatus != "Lost, burned" & .data$plantStatus != "Lost, presumed dead" & .data$plantStatus != "Downed") # remove records with status that doesn't include unambiguous live or dead individuals
vst_agb <- vst_agb %>% dplyr::filter(!is.na(.data$agb) ) # remove records that have NA for biomass, before they become misleading zeros as a result of group_by function

# Aggregate vst woody biomass data (assume that multiple instances of same individualID are true multiple boles and not accidental duplicates)
vst_agb_final <- vst_agb %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$taxonID, .data$individualID, .data$plantStatus2, .data$growthForm,.data$year) %>% dplyr::summarise(agb = sum(.data$agb, na.rm = TRUE)) %>% dplyr::ungroup()
  # This is the final biomass value for each individualID by year by plantStatus2 combo, which is subsequently used for both annual biomass summaries and NPP calculations for specified consecutive years

 vst_agb_final  <- merge(vst_agb_final, perplot, by=c("plot_eventID", "eventID", "plotID", "year"), all.x=TRUE) # add total sampled areas
  vst_agb_final$sampledAreaM2 <- ifelse(vst_agb_final$growthForm == "single bole tree" | vst_agb_final$growthForm == "multi-bole tree", vst_agb_final$totalSampledAreaTrees, NA )
  vst_agb_final$sampledAreaM2 <- ifelse(vst_agb_final$growthForm == "single shrub" | vst_agb_final$growthForm == "small shrub" | vst_agb_final$growthForm == "sapling"  | vst_agb_final$growthForm == "small tree", 
                                  vst_agb_final$totalSampledAreaShrubSapling, vst_agb_final$sampledAreaM2 )
  vst_agb_final$sampledAreaM2 <- ifelse(vst_agb_final$growthForm == "liana", vst_agb_final$totalSampledAreaLiana, vst_agb_final$sampledAreaM2 )
  vst_agb_final <- vst_agb_final %>% dplyr::filter(!is.na(.data$sampledAreaM2) ) # remove records that can't be scaled to area basis
    vst_agb_final$agb_Mgha <- round(vst_agb_final$agb * 0.001 * (10000/vst_agb_final$sampledAreaM2), 6)   # multiply by 0.001 to convert from kg to Mg and then convert to ha (10,000m2) based on plot area
  
  if(growthForm == "shrubLiana") {
      vst_agb_final <- vst_agb_final %>% dplyr::filter(.data$growthForm == "single shrub" | .data$growthForm == "small shrub" | .data$growthForm == "liana") }
  
  
  if(growthForm == "tree") {
      vst_agb_final <- vst_agb_final %>% dplyr::filter(.data$growthForm == "single bole tree" | .data$growthForm == "multi-bole tree"  | .data$growthForm == "sapling" | .data$growthForm == "small tree") }
  
  # vst_agb_per_ha <- vst_agb_final %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, .data$individualID, .data$plantStatus2, .data$year) %>% dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE)) %>% dplyr::ungroup()
  #   # add up biomass per unit area for each plot x individualID x year x plantStatus2 x nlcdClass x taxonID combo (growthForm no longer needed since area has been calculated in previous steps)

  vst_agb_per_ha_appInd <- vst_agb_final %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, .data$growthForm, .data$individualID, .data$plantStatus2, .data$year) %>% dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE)) %>% dplyr::ungroup()
   # add up biomass per unit area for each plot x individualID x year x plantStatus2 x nlcdClass x taxonID x growthForm combo  

#################################################################################################
#################################################################################################
# combine vst_non-woody (vst_agb_per_ha_other) and vst_apparentindividual (vst_agb_per_ha_appInd)
vst_agb_per_ha <- rbind(vst_agb_per_ha_appInd, vst_agb_per_ha_other) # combine woody and non-woody tables

if(growthForm == "other") {
      vst_agb_per_ha <- vst_agb_per_ha %>% dplyr::filter(.data$growthForm == "tree fern" | .data$growthForm == "small tree fern" | .data$growthForm == "large tree fern"  | 
                           .data$growthForm == "palm" | .data$growthForm == "small palm" | .data$growthForm == "palm tree" | 
                           .data$growthForm == "ocotillo" | .data$growthForm == "xerophyllum"  | .data$growthForm == "yucca") }

if(growthForm == "shrubLiana") {
      vst_agb_per_ha <- vst_agb_per_ha %>% dplyr::filter(.data$growthForm == "single shrub" | .data$growthForm == "small shrub" | .data$growthForm == "liana") }
  
  
if(growthForm == "tree") {
      vst_agb_per_ha <- vst_agb_per_ha %>% dplyr::filter(.data$growthForm == "single bole tree" | .data$growthForm == "multi-bole tree"  | .data$growthForm == "sapling" | .data$growthForm == "small tree") }

# if growthForm is "all" then do no filtering by growthForm
    
agb_ind_eventID_list <- unique(vst_agb_per_ha$plot_eventID) # build list of plot_eventIDs that had qualifying veg (e.g. large trees) sampled in perplot - later diff against perplot

#################################################################################################
#################################################################################################
# identify plot by eventID combos that don't have biomass values, so that we can assign them values of 0
# create list of plot by eventID combos that have biomass values from vst woody, vst non-herb perennial, or both. 

vst_agb_zeros <- base::setdiff(plot_eventID_list, agb_ind_eventID_list) # events in perplot plot_eventID list that aren't in agb_per_ha list (before filtering the increment)
vst_agb_zeros <- as.data.frame(vst_agb_zeros)
 vst_agb_zeros <- dplyr::rename(vst_agb_zeros, plot_eventID = vst_agb_zeros)
  vst_agb_zeros$plotID <- substr(vst_agb_zeros$plot_eventID,1,8)
  vst_agb_zeros$siteID <- substr(vst_agb_zeros$plot_eventID,1,4)
  vst_agb_zeros$year <- as.numeric(substr(vst_agb_zeros$plot_eventID,19,22) )
  vst_agb_zeros$eventID <- substr(vst_agb_zeros$plot_eventID,10,22)
  vst_agb_zeros <- merge(vst_agb_zeros, plotType_df, by = "plotID", all.x=TRUE)
 
#################################################################################################  
#################################################################################################  



####### PLOT AND SITE-LEVEL BIOMASS SUMMARIES - not concerned here with year to year continuity or transitions from live to dead, etc.
vst_plot_summary <- vst_agb_per_ha %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, .data$growthForm, .data$plantStatus2, .data$year) %>% dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE)) 
        # add up biomass per unit area for each plot x year x plantStatus2 x nlcdClass combo (aggegate individualIDs
vst_plot_wide <- tidyr::pivot_wider(vst_plot_summary, id_cols = c("plot_eventID", "eventID", "siteID", "plotID", "plotType", "nlcdClass", "taxonID", "growthForm", "year"), names_from = "plantStatus2", names_prefix = "agb_Mgha__", values_from = "agb_Mgha")  # transpose live and dead rows into separate columns

vst_plot_wide[is.na(vst_plot_wide)] <- 0 # NAs created during transpose. Assumption that if Live individuals sampled then Dead were too, and vice versa

# ## Aggregate data such that species IDs are retained but tree vs. woody distinction is lost
# if(nrow(vst_plot_wide > 0)) {vst_plot_wide <- vst_plot_wide %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$taxonID, .data$growthForm, .data$year) %>% dplyr::summarise(agb_Mgha__Live = sum(.data$agb_Mgha__Live, na.rm = TRUE),
#              agb_Mgha__Dead_or_Lost = sum(.data$agb_Mgha__Dead_or_Lost, na.rm = TRUE) ) # if same taxonID present in tree df and other df, sum them - loses the treSapShrLia vs nonHerbPer DP source distinction
# } else {
#   vst_plot_wide$agb_Mgha__Live = 0
#   vst_plot_wide$agb_Mgha__Dead_or_Lost= 0
# }

# ADD PLOTS WITH 0 BIOMASS AND HERB BIOMASS - FORK FROM NPP CALCULATIONS   
  vst_agb_zeros_plot <- vst_agb_zeros
  if(nrow(vst_agb_zeros_plot) > 0) {vst_agb_zeros_plot$agb_Mgha__Dead_or_Lost <- vst_agb_zeros_plot$agb_Mgha__Live <- 0 }

# add on plots with biomass of zero, and calculate site averages
vst_plot_w_0s <- rbind(vst_plot_wide, vst_agb_zeros_plot) # toggle on to add zeros for plots that were sampled but focal individuals not found (woody + nonHerbPer, just large trees, etc, depending on upstream filters)

priority_plots_without_plotType <- priority_plots_all %>% dplyr::select(-"plotType")
vst_plot_w_0s <- merge(vst_plot_w_0s, priority_plots_without_plotType, by=c("plotID","eventID","year"), all.x=TRUE)

vst_plot_w_0s <- vst_plot_w_0s %>% dplyr::filter(!is.na(.data$agb_Mgha__Live) & !is.na(.data$agb_Mgha__Dead_or_Lost) ) # remove records that are missing any productivity values
vst_plot_w_0s <- vst_plot_w_0s %>% dplyr::filter(.data$year >= start_from_input) # remove records from before the time of interest
if(plotType == "tower") {vst_plot_w_0s <- vst_plot_w_0s %>% dplyr::filter(.data$plotType == "tower") } # if arg plotType = "tower" then filter to just tower plots, otherwise keep all plots from input data
if(!is.na(plotPriority)) {vst_plot_w_0s <- vst_plot_w_0s %>% dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)} # remove lower priority plots that aren't required to be sampled every year (default is 5 (the 5 highest priority plots))

vst_plot_w_0s_sum_taxa <- vst_plot_w_0s
if(nrow(vst_plot_w_0s_sum_taxa) > 0) {vst_plot_w_0s_sum_taxa <- vst_plot_w_0s %>% dplyr::group_by(.data$plot_eventID, .data$eventID, .data$siteID, .data$plotID, .data$plotType, .data$nlcdClass, .data$year) %>% dplyr::summarise(agb_Mgha__Live = sum(.data$agb_Mgha__Live, na.rm = TRUE),
             agb_Mgha__Dead_or_Lost = sum(.data$agb_Mgha__Dead_or_Lost, na.rm = TRUE) ) # sum the taxonIDs within each plot in preparation for creating site level averages and other metrics
} else {
  vst_plot_w_0s_sum_taxa$agb_Mgha__Live = 0
  vst_plot_w_0s_sum_taxa$agb_Mgha__Dead_or_Lost= 0
}

vst_site <- vst_plot_w_0s_sum_taxa %>% dplyr::group_by(.data$siteID, .data$year) %>% dplyr::summarise(woodPlotNum = dplyr::n(), woodLiveMassMean_Mgha = round(mean(.data$agb_Mgha__Live, na.rm = TRUE),3), 
      woodLiveMassSD_Mgha = round(stats::sd(.data$agb_Mgha__Live, na.rm = TRUE),3))   
  # calculate site - level averages of plots (including plots with 0 biomass), along with plot min, max, standard deviation and plot count
  
priority_plots$year <- NULL
vst_plot_w_0s$plotType <- NULL #  plotType field only partially populated so remove
 vst_plot_w_0s <- merge(vst_plot_w_0s, plotType_df, by="plotID", all.x = TRUE) # add fully populated plotType field
   domainID_df <- unique(appInd %>% dplyr::select("siteID", "domainID") )
  vst_plot_w_0s <- merge(vst_plot_w_0s, domainID_df, by="siteID", all.x=TRUE)

print("Returning woody biomass output data frames as a list object  ..... ")

  output.list <- list(
   vst_agb_per_ha = vst_agb_per_ha,
   vst_plot_w_0s = vst_plot_w_0s,
   vst_agb_zeros = vst_agb_zeros,
   vst_site = vst_site
)
  return(output.list)
}
