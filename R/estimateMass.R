##############################################################################################
#' @title Combine biomass from multiple NEON data products

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Wrapper function to combine biomass summaries of NEON data products. Currently supported data products are "Vegetation structure" (DP1.10098.001),
#' estimated with the estimateWoodMass function, and "Herbaceous clip harvest (DP1.10023.001), estimated with the scaleHerbMass function. Biomass outputs can, if 
#' desired, be used in the follow-up estimateProd productivity function. Data inputs are list objects retrieved using the neonUtilities::loadByProduct() function 
#' for each selected data product (preferred), or data downloaded from the NEON Data Portal.
#' 
#' @details A site-level summary of NEON biomass from the selected NEON data products is provided as a dataframe, in addition to list objects with more detailed 
#' outputs for each of the selected NEON data products. For more documentation details see the documentation for the companion functions (e.g. estimateWoodMass 
#' and scaleHerbMass).
#' 
#' @param dataProducts Specify a character vector with the NEON data products to be combined. Defaults to c("Vst","Hbp") for Vegetation structure and Herbaceous clip harvest, respectively. [character]
#' @param inputDataListVst Specify a loaded R list object (e.g. VstDat) that contains NEON portal VST (Vegetation structure) data. [character]
#' @param inputDataListHbp Specify a loaded R list object (e.g. HbpDat) that contains NEON portal HBP (Herbaceous clip harvest) data. [character]
#' @param site Either NA, meaning all available sites from inputDataListVst and inputHBP data, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all. [list]
#' @param start Either NA, meaning all available years from inputDataListVst and inputHBP data, or a character vector specifying start year in the form YYYY, e.g. 2019. The default and earliest allowable option is 2018. [numeric]
#' @param end Either NA, meaning all available years from inputDataListVst and inputHBP data after the start year, or a character vector specifying start year in the form YYYY, e.g. 2021. Data from the current calendar year are excluded since they would be incomplete. [numeric
#' @param plotType Optional filter based on NEON plot type. Defaults to "tower" plots, which are sampled annually. Otherwise "distributed" plots are examined also. [character]
#' @param plotPriority NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is 5. [numeric]
#' @param growthForm Select which growth forms to analyse within NEON portal data [character]
#' 
#' @return This function returns a site-level summary dataframe "biomass_site" with NEON biomass (units are Megagrams per hectare) from the selected NEON data products. 
#' In addition, list objects (e.g. estimateWoodMassOutputs and scaleHerbMassOutputs) with more granular data summaries are returned for each of the selected NEON data products. 
#' More detailed documentation is provided in the documentation of contributing functions (e.g. estimateWoodMass and scaleHerbMass).
#' 
#' @examples
#' \dontrun{
#' # If list of lists is not in memory, load lists of dataframes from local files:
#' load('VstDat.rds') # load NEON VST portal data from a local file
#' load('HbpDat.rds') # load NEON HBP portal data from a local file
#' 
#' # example with arguments at default values
#' estimateMassOutputs <- estimateMass(dataProducts = c("Vst","Hbp"), inputDataListVst = VstDat, 
#' inputDataListHbp = HbpDat )
#' 
#' # example with inputDataListVst = NA that triggers a fresh NEON portal data download
#' estimateMassOutputs <- estimateMass(inputDataListVst = NA, site = "STEI", 
#'                                            inputDataListHbp = NA)
#' 
#' # example with just a subset of inputDataListVst data being utilized
#' estimateMassOutputs <- estimateMass(inputDataListVst = VstDat, start = 2022, 
#'                                            inputDataListHbp = NA)
#' 
#' # example specifying several non-default arguments
#' estimateMassOutputs <- estimateMass(inputDataListVst = VstDat, inputDataListHbp = NA,
#'                                              growthForm = "all trees", plotPriority = 4)
#' 
#' list2env(estimateMassOutputs ,.GlobalEnv) # unlist all data frames
#' saveRDS(estimateMassOutputs, 'estimateMassOutputs.rds') # save all outputs locally
#' }

# changelog and author contributions / copyrights
# Samuel M Simkin (2025-01-08)  original creation
# Samuel M Simkin (2025-01-15)  revised
##############################################################################################

estimateMass = function(dataProducts = c("Vst","Hbp"),
                       inputDataListVst = NA,
                       inputDataListHbp = NA,
#                       inputDataListBbc,
                       site = NA,
                       start = NA, 
                       end = NA,
                       plotType = "tower",
                       plotPriority = 5,
                       growthForm = "single and multi-bole trees"
                         ) {

# Warning if start date is too early
   if (!is.na(start)) {if(as.numeric(start) < 2018){  start = "2018"
    print("The earliest year that can be used for this function is 2018. The start year has automatically been changed to 2018.")
  }} else {start = as.numeric(start_from_input)}

# Warning if end date is too late
   if (!is.na(end)) {if(as.numeric(end) > as.integer(format(Sys.Date(), "%Y"))-1){  end = as.character(as.integer(format(Sys.Date(), "%Y"))-1)
    print("The end year can not be the current calendar year or a future year. The end year has automatically been changed to the year prior to the current calendar year.")
  }} else {end = as.numeric(end_from_input)}


if("Vst" %in% dataProducts){  
  
# Check whether inputDataListVst is something other than a list or NA, and if NA then do a fresh portal download
  
if(!methods::is(inputDataListVst, class = "list" )){ if(length(inputDataListVst) == 1 ){ if(!is.na(inputDataListVst) ){
  stop("The inputDataListVst argument is expected to be either a list or NA. A character argument is not allowed.")}
 }
}
  
if(!methods::is(inputDataListVst, class = "list" )){ if(length(inputDataListVst) > 1 ){ 
  stop("The inputDataListVst argument is expected to be either a list or NA. Another argument (e.g. a dataframe) is not allowed.")
 }  
}

if(methods::is(inputDataListVst, class = "list" )){   
list2env(inputDataListVst ,.GlobalEnv)  

vst_mappingandtagging <- inputDataListVst$vst_mappingandtagging
vst_perplotperyear <- inputDataListVst$vst_perplotperyear
vst_apparentindividual <- inputDataListVst$vst_apparentindividual
'vst_non-woody' <- inputDataListVst$'vst_non-woody'


#   Check that required tables within list match expected names
listExpNames <- c("vst_apparentindividual", "vst_mappingandtagging", "vst_non-woody", "vst_perplotperyear")
    
if (length(setdiff(listExpNames, names(inputDataListVst))) > 0) {
      stop(glue::glue("Required tables missing from 'inputDataListVst':",
                      '{paste(setdiff(listExpNames, names(inputDataListVst)), collapse = ", ")}',
                      .sep = " "))
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
  
  
perplot <- inputDataListVst$vst_perplotperyear ## read in plot sample area for each growthForm by eventID combo from vst_perplotperyear table
perplot$year <- as.numeric(substr(perplot$eventID,10,13) )
start_from_input <- as.character(min(as.numeric(substr(perplot$year, 1,4)))) # year component of eventID, which on rare occasions may be before the year component of earliest collect date)
end_from_input <- as.character(max(as.numeric(substr(perplot$date, 1,4))))
site_from_input <- unique(perplot$siteID)


# Warning if start date filter is before input data
  if (!is.na(start)) {if(as.numeric(start) < as.numeric(start_from_input)){  start = as.numeric(start_from_input)
    print("The start year is earlier than the input data. The start year in current run of function has automatically been changed to:")
    print(start_from_input)
  }} else {start = as.numeric(start_from_input)}

# Warning if end date filter is after input data
   if (!is.na(end)) {if(as.numeric(end) > as.numeric(end_from_input)){  end = as.numeric(end_from_input)
    print("The end year is later than the input data (re-pull data as late as the preceding calendar year from the portal using separate function). The end year in current run of function has automatically been changed to:")
    print(end_from_input)
  }} else {end = as.numeric(end_from_input)}

# Warning if site is not in input data
if (length(site) >1) {
     sites_not_in_input <- setdiff(site, site_from_input)
     if(length(sites_not_in_input) >= 1){ 
    stop(paste0("One or more sites are not in the input data (re-pull data for desired sites from the portal using separate function). The following site(s) are not in the input data: ", sites_not_in_input) ) 
     }} else {
if (!is.na(site)) {
     sites_not_in_input <- setdiff(site, site_from_input)
     if(length(sites_not_in_input) >= 1){ 
    stop(paste0("One or more sites are not in the input data (re-pull data for desired sites from the portal using separate function). The following site(s) are not in the input data: ", sites_not_in_input) ) 
     }} else site = site_from_input}

} # end of section checking input data list object


# Error if invalid growthForm option selected
  if(growthForm != "single and multi-bole trees" & growthForm != "all trees"){
    stop("Currently the only valid growthForm options are 'single and multi-bole trees' or 'all trees'.")
  }  

# Error if invalid plotType option selected
  if(plotType != "tower" & plotType != "all"){
    stop("The only valid plotType options are 'tower' or 'all'.")
  }  
  
# Error if invalid plotPriority option selected
  if(plotPriority < 1){
    stop("The minimum plotPriority value is 1, and 5 or greater is the recommended default.")
  }  
  
  
print("Calculating above-ground woody biomass  ..... ")
estimateWoodMassOutputs <- estimateWoodMass(inputDataListVst = inputDataListVst, site = site, start = start, end = end, plotType = plotType,
                       plotPriority = plotPriority, growthForm = growthForm)
Vst <- estimateWoodMassOutputs$vst_site
}

######################################################  
  
  
if("Hbp" %in% dataProducts){ 

# Check whether inputDataListHbp is something other than a list or NA, and if NA then do a fresh portal download
  
if(!methods::is(inputDataListHbp, class = "list" )) { if(length(inputDataListHbp) == 1 ){ if(!is.na(inputDataListHbp) ){
  stop("The inputDataListHbp argument is expected to be either a list or NA. A character argument is not allowed.") }
 }
}
  
if(!methods::is(inputDataListHbp, class = "list" )){ if(length(inputDataListHbp) > 1 ){ 
  stop("The inputDataListHbp argument is expected to be either a list or NA. Another argument (e.g. a dataframe) is not allowed.")
 }  
}

if(methods::is(inputDataListHbp, class = "list" )){   
list2env(inputDataListHbp ,.GlobalEnv)  

hbp_perbout <- inputDataListHbp$hbp_perbout
hbp_massdata <- inputDataListHbp$hbp_massdata

#   Check that required tables within list match expected names
listExpNames <- c("hbp_perbout", "hbp_massdata")
    
if (length(setdiff(listExpNames, names(inputDataListHbp))) > 0) {
      stop(glue::glue("Required tables missing from 'inputDataListHbp':",
                      '{paste(setdiff(listExpNames, names(inputDataListHbp)), collapse = ", ")}',
                      .sep = " "))
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

} # end of section checking input data list object

print("Calculating above-ground herbaceous biomass  ..... ")
scaleHerbMassOutputs <- scaleHerbMass(inputDataListHbp = inputDataListHbp, site = site, start = start, end = end)
Hbp <- scaleHerbMassOutputs$hbp_site
}

#############################################  
  
print("Combining biomass from selected data products and returning biomass output objects as a list object ..... ")

biomass_list <- mget(dataProducts) # this works if the final summary dataframes to be joined have the exact names as the values in the dataProducts argument
biomass_site <- biomass_list %>% purrr::reduce(dplyr::full_join, by=c("siteID", "year") )

if("woodLiveMassMean_Mgha" %in% colnames(biomass_site) & "herbPeakMassMean_Mgha" %in% colnames(biomass_site)){
biomass_site$mass_Mgha <- biomass_site$woodLiveMassMean_Mgha + biomass_site$herbPeakMassMean_Mgha
}

if("Vst" %in% dataProducts & !("Hbp" %in% dataProducts)){
 output.list <- list(
    biomass_site = biomass_site,
     estimateWoodMassOutputs = estimateWoodMassOutputs
    )
}

if("Hbp" %in% dataProducts & !("Vst" %in% dataProducts)){
 output.list <- list(
    biomass_site = biomass_site,
     scaleHerbMassOutputs = scaleHerbMassOutputs
    )
}

if("Vst" %in% dataProducts & "Hbp" %in% dataProducts){  # overwrites earlier list if both are present 
 output.list <- list(
    biomass_site = biomass_site,
     estimateWoodMassOutputs = estimateWoodMassOutputs,
     scaleHerbMassOutputs = scaleHerbMassOutputs
    )
}
  return(output.list)

}
