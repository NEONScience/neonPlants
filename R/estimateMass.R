#' @title Generate summed site-level biomass estimates from NEON data products
#'
#' @author Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Combine above- and/or below-ground biomass summaries from multiple NEON data products. Currently supported data products are "Herbaceous clip harvest" (DP1.10023.001), "Root biomass and chemistry, periodic" (DP1.10067.001), and "Vegetation structure" (DP1.10098.001). Biomass outputs can subsequently be used with the estimateProd() function. Data inputs are list objects retrieved using the neonUtilities::loadByProduct() function for each selected data product (preferred), or data downloaded from the NEON Data Portal.
#' 
#' @details A site-level summary of NEON biomass from the selected NEON data products is provided as a dataframe, in addition to more detailed outputs for each of the selected NEON data products used to generate the site-level summary. For more documentation details see the documentation for the companion functions - i.e., estimateWoodMass(), scaleHerbMass(), and scaleRootMass().
 
#' @param dataProducts Character vector specifying the NEON data products to be summed. Valid options are "Bbc" (fine root biomass), "Hbp" (herbaceous clip harvest biomass), and "Vst" (above-ground allometrically estimated woody biomass, including palms and tree ferns); Required argument with no default. [character]
#'
#' @param inputDataListVst A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. If "Vst" list input is provided, the "Vst" table input arguments must all be NA; similarly, if list input is missing and "Vst" is included in the dataProducts argument, table inputs must be provided for 'wood_apparentindividual', 'wood_mappingandtagging', 'wood_nonWoody', and 'wood_perplotperyear' arguments. [list]
#' 
#' @param wood_apparentindividual The 'vst_apparentindividual' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListVst' argument must be missing. [data.frame]
#' 
#' @param wood_mappingandtagging The 'vst_mappingandtagging' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListVst' argument must be missing. [data.frame]
#' 
#' @param wood_nonWoody The 'vst_non-woody' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListVst' argument must be missing. [data.frame]
#' 
#' @param wood_perplotperyear The 'vst_perplotperyear' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListVst' argument must be missing. [data.frame]
#' 
#' @param inputDataListHbp A list object comprised of "Herbaceous clip harvest" tables (DP1.10023.001) downloaded using the neonUtilities::loadByProduct() function. If "Hbp" list input is provided, the "Hbp" table input arguments must all be NA; similarly, if list input is missing and "Hbp" is included in the dataProducts argument, table inputs must be provided for 'herb_perbout' and 'herb_massdata' arguments. [list]
#' 
#' @param herb_perbout The 'hbp_perbout' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListHbp' argument must be missing. [data.frame]
#' 
#' @param herb_massdata The 'hbp_massdata' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListHbp' argument must be missing. [data.frame]
#'
#' @param inputDataListBbc A list object comprised of "Root biomass and chemistry" tables (DP1.10067.001) downloaded using the neonUtilities::loadByProduct() function. If "Bbc" list input is provided, the "Bbc" table input arguments must all be NA; similarly, if list input is missing and "Bbc" is included in the dataProducts argument, table inputs must be provided for 'inputCore' and 'inputMass' arguments. [list]
#' 
#' @param inputCore The 'bbc_percore' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListBbc' argument must be missing. [data.frame]
#' 
#' @param inputMass The 'bbc_rootmass' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataListBbc' argument must be missing. [data.frame]
#' 
#' @param inputDilution The 'bbc_dilution' table for the site x month combination(s) of interest (optional, defaults to NA). If table input is provided, the 'inputDataListBbc' argument must be missing. [data.frame]
#' 
#' @param includeDilution Required if dataProducts includes "Bbc". Indicator for whether mass of root fragments < 1 cm length should be  calculated, as estimated via the Dilution Sampling method (Defaults to TRUE). If TRUE and 'inputDilution' is NA, the 'bbc_dilution' table will be extracted from the list input. [logical]
#' 
#' @param includeFragInTotal Required if dataProducts includes "Bbc". Indicator for whether mass of root fragments < 1 cm length calculated from dilution sampling should be included when summing across sizeCategory to calculate root 'totalDryMass'. Defaults to FALSE. If set to TRUE and 'inputDataListBbc' is missing, the 'bbc_dilution' table must be provided to the 'inputDilution' argument. [logical]
#' 
#' @param plotSubset Applicable if dataProducts includes "Vst" or "Hbp". The options are "all" (all tower and distributed plots), the default of "towerAll" (all plots in the tower airshed but no distributed plots), "towerAnnualSubset" (only the subset of tower plots that are sampled annually), and "distributed" (all distributed plots, which are sampled in 5-yr bouts and are spatially representative of the NLCD classes at at site). [character]
#' 
#' @param growthForm Applicable if dataProducts includes "Vst". Select which growth forms to analyse. The options are "tree" (sbt, mbt) for trees with a dbh > 10 cm and the default of "all", which includes the large trees, and also small trees (smt, sap), shrubs (sis, sms), liana (lia), palms (plm, ptr, spm), tree ferns (ltf, stf, tfn), ocotillo (oco), yucca (yuc), and xerophyllum (xer).  Consult the Vegetation Structure Quick Start Guide and/or the Data Product User Guide for more growth form information. [character]
#' 
#' @return A site-level summary dataframe named "biomass_site" with biomass from the selected NEON data products (megagrams per hectare). In addition, more granular data summaries are returned for each of the selected input data products. More detail is provided for product-specific data frames in the documentation associated wiht contributing functions - i.e., estimateWoodMass(), scaleHerbMass(), and scaleRootMass().
#' 
#' @examples
#' \dontrun{
#' 
#' #  Example with arguments at default values
#' estimateMassOutputs <- estimateMass(
#' dataProducts = c("Vst","Hbp","Bbc"), 
#' inputDataListVst = VstDat, 
#' inputDataListHbp = HbpDat
#' )
#' 
#' #  Example specifying several non-default arguments
#' estimateMassOutputs <- estimateMass(
#' dataProducts = c("Vst","Hbp"), 
#' inputDataListVst = VstDat, 
#' inputDataListHbp = NA,
#' growthForm = "tree"
#' )
#' }
#' 
#' @export estimateMass


estimateMass = function(dataProducts = NA,
                        inputDataListVst = NA,
                        wood_apparentindividual = NA,
                        wood_mappingandtagging = NA,
                        wood_nonWoody = NA,
                        wood_perplotperyear = NA,
                        inputDataListHbp = NA,
                        herb_perbout = NA,
                        herb_massdata = NA,
                        inputDataListBbc = NA,
                        inputCore = NA,
                        inputMass = NA,
                        inputDilution = NA,
                        includeDilution = TRUE,
                        includeFragInTotal = FALSE,
                        plotSubset = "towerAll",
                        growthForm = "all") {


  if (!all(dataProducts %in% c("Bbc", "Hbp", "Vst") )) {
    stop("The dataProducts argument must be one of: 'Bbc', 'Hbp', 'Vst', or a comma-separated combination of two or more of these.")
  }  

  
  ### VST data: Conditionally obtain site-level data with estimateWoodMass() function
  if("Vst" %in% dataProducts){  
    
    inputDataList = inputDataListVst
    
    message("Calculating above-ground woody biomass  ..... ")
    
    estimateWoodMassOutputs <- estimateWoodMass(inputDataList = inputDataListVst, 
                                                inputIndividual = wood_apparentindividual, 
                                                inputMapTag = wood_mappingandtagging, 
                                                inputNonWoody = wood_nonWoody, 
                                                inputPerPlot = wood_perplotperyear, 
                                                growthForm = growthForm, 
                                                plotSubset = plotSubset)
    
    Vst <- estimateWoodMassOutputs$vst_site %>% 
      dplyr::select("siteID","year","woodPlotNum","woodLiveMassMean_Mgha","woodLiveMassSD_Mgha","woodDeadMassMean_Mgha","woodDeadMassSD_Mgha")
  }
  
  
  
  ### HBP data: Conditionally obtain site-level data with scaleHerbMass() function
  if ("Hbp" %in% dataProducts) { 
    
    message("Calculating above-ground herbaceous biomass  ..... ")
    
    scaleHerbMassOutputs <- scaleHerbMass(inputDataList = inputDataListHbp, 
                                          inputBout = herb_perbout, 
                                          inputMass = herb_massdata,
                                          plotSubset = plotSubset)
    
    Hbp <- scaleHerbMassOutputs$hbp_site %>% 
      dplyr::select("siteID","year","herbPlotNum","herbPeakMassMean_Mgha","herbPeakMassSD_Mgha")
  }
  
  
  
  ### BBC data: Conditionally obtain site-level data with scaleRootMass() function
  if ("Bbc" %in% dataProducts) { 
    
    message("Scaling below-ground biomass  ..... ")
    
    scaleRootMassOutputs <- scaleRootMass(inputDataList = inputDataListBbc, 
                                          includeDilution = includeDilution, 
                                          includeFragInTotal = includeFragInTotal, 
                                          inputCore = inputCore, 
                                          inputMass = inputMass, 
                                          inputDilution = inputDilution)
    
    Bbc <- scaleRootMassOutputs$siteRootMass %>% 
      dplyr::select("siteID","year","rootPlotNum","rootMassMean_Mgha","rootMassSD_Mgha")
  }
  
  
  
  ### Combine biomass estimates from BBC, HBP, and VST data products
  message("Combining biomass from selected data products and returning biomass output objects as a list object ..... ")
  
  #   Create list of output data frames for user-selected products; works because final summary dataframes to be joined have the exact names as the values in the dataProducts argument.
  biomass_list <- mget(dataProducts)
  
  #   Create single data frame from elements in 'biomass_list'
  biomass_site <- biomass_list %>% 
    purrr::reduce(dplyr::full_join, 
                  by = c("siteID", "year"))
  
  # #   Conditionally sum mean above- and below-ground biomass
  # if("woodLiveMassMean_Mgha" %in% colnames(biomass_site) & "herbPeakMassMean_Mgha" %in% colnames(biomass_site) & "rootMassMean_Mgha" %in% colnames(biomass_site)){
  #   biomass_site$mass_Mgha <- round(biomass_site$woodLiveMassMean_Mgha + biomass_site$herbPeakMassMean_Mgha + biomass_site$rootMassMean_Mgha, 3)
  # }
  # 
  # if("woodLiveMassMean_Mgha" %in% colnames(biomass_site) & "rootMassMean_Mgha" %in% colnames(biomass_site)){
  #   biomass_site$mass_Mgha <- round(biomass_site$woodLiveMassMean_Mgha + biomass_site$rootMassMean_Mgha, 3)
  # }
  # 
  # if ("woodLiveMassMean_Mgha" %in% colnames(biomass_site) & "herbPeakMassMean_Mgha" %in% colnames(biomass_site)) {
  #   biomass_site$mass_Mgha <- round(biomass_site$woodLiveMassMean_Mgha + biomass_site$herbPeakMassMean_Mgha, 3)
  # }
  # 
  # if("herbPeakMassMean_Mgha" %in% colnames(biomass_site) & "rootMassMean_Mgha" %in% colnames(biomass_site)){
  #   biomass_site$mass_Mgha <- round(biomass_site$herbPeakMassMean_Mgha + biomass_site$rootMassMean_Mgha, 3)
  # }
  # 
  # if("woodLiveMassMean_Mgha" %in% colnames(biomass_site) & "herbPeakMassMean_Mgha" %in% colnames(biomass_site) & "rootMassMean_Mgha" %in% colnames(biomass_site)){
  #   biomass_site$mass_Mgha <- round(biomass_site$woodLiveMassMean_Mgha + biomass_site$herbPeakMassMean_Mgha + biomass_site$rootMassMean_Mgha, 3)
  # }
  
  
  #   Create output.list object
  output.list <- list(biomass_site = biomass_site)
  
  #   Conditionally append BBC output
  if ("Bbc" %in% dataProducts) {
    output.list <- append(output.list, 
                          list(scaleRootMassOutputs = scaleRootMassOutputs), 
                          after = length(output.list))
  }
  
  #   Conditionally append HBP output
  if ("Hbp" %in% dataProducts) {
    output.list <- append(output.list,
                          list(scaleHerbMassOutputs = scaleHerbMassOutputs),
                          after = length(output.list))
  }
  
  #   Conditionally append VST output
  if ("Vst" %in% dataProducts) {
    output.list <- append(output.list,
                          list(estimateWoodMassOutputs = estimateWoodMassOutputs),
                          after = length(output.list))
  }
  
  return(output.list)
  
}
