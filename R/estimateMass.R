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
#' @param dataProducts Specify a character vector with the NEON data products to be combined. Defaults to c("Vst","Hbp") for Vegetation structure and Herbaceous 
#' clip harvest, respectively. The other option currently available is "Bbc" for Root biomass and chemistry, periodic [character]

#' @param inputDataListVst A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct function. 
#' If list input is provided, the table input arguments must all be NA; similarly, if list input is missing and "Vst" is included in dataProducts argument, table 
#' inputs must be provided for 'vst_apparentindividual', 'vst_mappingandtagging', and 'vst_perplotperyear' arguments. [list]
#' @param vst_apparentindividual The 'vst_apparentindividual' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataListVst' argument must be missing. [data.frame]
#' @param vst_mappingandtagging The 'vst_mappingandtagging' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataListVst' argument must be missing. [data.frame]
#' @param vst_perplotperyear The 'vst_perplotperyear' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataListVst' argument must be missing. [data.frame]

#' @param inputDataListHbp A list object comprised of "Herbaceous clip harvest" tables (DP1.10023.001) downloaded using the neonUtilities::loadByProduct function. 
#' If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 
#' 'hbp_perbout', and 'hbp_massdata' arguments. [list]
#' @param hbp_perbout The 'hbp_perbout' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataListHbp' argument must be missing. [data.frame]
#' @param hbp_massdata The 'hbp_massdata' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataListHbp' argument must be missing. [data.frame]

#' @param inputDataListBbc A list object comprised of Plant Below Ground Biomass tables (DP1.10067.001) 
#' downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table
#' input arguments must all be NA; similarly, if list input is missing, table inputs must be
#' provided for 'inputCore' and 'inputMass' arguments at a minimum. [list]
#' @param inputCore The 'bbc_percore' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataListBbc' argument must be missing.
#' [data.frame]
#' @param inputMass The 'bbc_rootmass' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataListBbc' argument must be missing.
#' [data.frame]
#' @param inputDilution The 'bbc_dilution' table for the site x month combination(s) of interest
#' (optional, defaults to NA). If table input is provided, the 'inputDataListBbc' argument must be 
#' missing. [data.frame]
#' @param includeDilution Required if dataProducts includes "Bbc". Indicator for whether mass of root fragments < 1 cm length should be
#' calculated, as estimated via the Dilution Sampling method (Defaults to TRUE). If TRUE and
#' 'inputDilution' is NA, the 'bbc_dilution' table will be extracted from the list input. [logical]
#' @param includeFragInTotal Required if dataProducts includes "Bbc". Indicator for whether mass of root fragments < 1 cm length 
#' calculated from dilution sampling should be included when summing across sizeCategory to 
#' calculate the 'totalDryMass'. Defaults to FALSE. If set to TRUE and 'inputDataListBbc' is missing, 
#' the 'bbc_dilution' table must be provided to the 'inputDilution' argument. [logical]

#' @param plotType Applicable if dataProducts includes "Vst" or "Hbp". Optional filter based on NEON plot type. Defaults to "tower" plots, which are sampled annually. Otherwise "distributed" plots are examined also. [character]
#' @param plotPriority Applicable if dataProducts includes "Vst" or "Hbp". NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is 5. [numeric]
#' @param growthForm Required if dataProducts includes "Vst" or "Hbp". Select which growth forms to analyse within NEON portal data [character]
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
                       vst_apparentindividual = NA,
                       vst_mappingandtagging = NA,
                       vst_perplotperyear = NA,
                       inputDataListHbp = NA,
                       hbp_perbout = NA,
                       hbp_massdata = NA,
                       inputDataListBbc = NA,
                       inputCore = NA,
                       inputMass = NA,
                       inputDilution = NA,
                       includeDilution = NA,
                       includeFragInTotal = NA,
                       plotType = "tower",
                       plotPriority = 5,
                       growthForm = "single and multi-bole trees"
                         ) {

if("Vst" %in% dataProducts){  

inputDataList = inputDataListVst
  
print("Calculating above-ground woody biomass  ..... ")
estimateWoodMassOutputs <- estimateWoodMass(inputDataList = inputDataList, 
        vst_apparentindividual = vst_apparentindividual, vst_mappingandtagging = vst_mappingandtagging, vst_perplotperyear = vst_perplotperyear, 
        growthForm = growthForm, plotType = plotType, plotPriority = plotPriority)
Vst <- estimateWoodMassOutputs$vst_site
}

######################################################  
  
  
if("Hbp" %in% dataProducts){ 
  
inputDataList = inputDataListHbp

print("Calculating above-ground herbaceous biomass  ..... ")
scaleHerbMassOutputs <- scaleHerbMass(inputDataList = inputDataList, 
        hbp_perbout = hbp_perbout, hbp_massdata = hbp_massdata,
        plotType = plotType, plotPriority = plotPriority)
Hbp <- scaleHerbMassOutputs$hbp_site
}

#############################################  

if("Bbc" %in% dataProducts){ 
  
inputDataList = inputDataListBbc

print("Scaling below-ground biomass  ..... ")
scaleRootMassOutputs <- scaleRootMass(inputDataList = inputDataList, includeDilution = includeDilution, includeFragInTotal = includeFragInTotal, 
       inputCore = inputCore, inputMass = inputMass, inputDilution = inputDilution)
Bbc <- scaleRootMassOutputs$siteRootMass
}

#############################################  
  
print("Combining biomass from selected data products and returning biomass output objects as a list object ..... ")

biomass_list <- mget(dataProducts) # this works because the final summary dataframes to be joined have the exact names as the values in the dataProducts argument
biomass_site <- biomass_list %>% purrr::reduce(dplyr::full_join, by=c("siteID", "year") )

if("woodLiveMassMean_Mgha" %in% colnames(biomass_site) & "herbPeakMassMean_Mgha" %in% colnames(biomass_site)){
biomass_site$mass_Mgha <- biomass_site$woodLiveMassMean_Mgha + biomass_site$herbPeakMassMean_Mgha
}

if("woodLiveMassMean_Mgha" %in% colnames(biomass_site) & "herbPeakMassMean_Mgha" %in% colnames(biomass_site) & "rootMassMean_Mgha" %in% colnames(biomass_site)){
biomass_site$mass_Mgha <- biomass_site$woodLiveMassMean_Mgha + biomass_site$rootMassMean_Mgha
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
 
if("Vst" %in% dataProducts & "Hbp" %in% dataProducts & "Bbc" %in% dataProducts){  # overwrites earlier list if all three are present 
 output.list <- list(
    biomass_site = biomass_site,
     estimateWoodMassOutputs = estimateWoodMassOutputs,
     scaleHerbMassOutputs = scaleHerbMassOutputs,
     scaleRootMassOutputs = scaleRootMassOutputs
    )

}
  return(output.list)

}
