##############################################################################################
#' @title Generate summed site-level productivity estimates from NEON data products
#'
#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Combine above-ground productivity summaries from multiple NEON data products and generate summed site-level productivity estimates. Currently supported data products are "Herbaceous clip harvest" (DP1.10023.001) and "Vegetation structure" (DP1.10098.001). 
#' Data inputs are list objects created by the estimateMass(), scaleHerbMass(), or estimateWoodMass() neonPlants functions.
#'
#' @details A site-level summary of NEON productivity from the selected NEON data products is provided as a dataframe, in addition to more detailed outputs for each of the selected NEON data products used to generate the site-level summary. For more documentation details see the documentation for the companion functions - i.e., estimateWoodProd() and estimateeHerbProd().
#' 
#' @param dataProducts Specify a character vector with the NEON data products to be combined. Defaults to c("Vst","Hbp") for Vegetation structure and Herbaceous clip harvest, respectively. [character]
#' @param inputDataListVstMass Specify a loaded R list object (e.g. estimateWoodMassOutputs) that was produced by companion estimateWoodMass or estimateMass function. [character]
#' @param inputDataListHbpMass Specify a loaded R list object (e.g. scaleHerbMassOutputs) that was produced by companion scaleHerbMass or estimateMass function. [character]
#' @param plotType Optional filter for NEON plot type. Options are "tower" (default) or "all". A subset of 5 Tower plots are sampled annually (those plots with the highest plot priority), and remaining Tower plots are scheduled every 5 years. If "all" is selected, results include data from Distributed plots that are also sampled every 5 years. [character]
#' @param plotPriority NEON plots have a priority number to spatially balance plots by NLCD class, etc. in the event that not all scheduled plots can be sampled. The lower the number the higher the priority. Options are "all" or "5". The default is "5" which retains just the 5 highest priority Tower plots that are sampled annually. [character]
#' @param calcMethod Select plot-level (approach 2) or individual-level (approach 1) productivity calculations (for woody data). The default is "approach_1" [character]
#' @param outlier Specify how much (if any) outlier removal should be performed (for woody data). The default is 1.5. [numeric]
#' @param outlierType Specify the type of outlier (for woody data), either SD (standard deviations) or IQR (interquartile range). The default is "IQR". [character]
#' 
#' @return A site-level summary dataframe named "productivity_site" with summed productivity ("Mg/ha/yr") from the selected NEON data products. In addition, more granular data summaries are returned for each of the selected input data products. 
#'
#' @examples
#' \dontrun{
#' 
#' #  Example with arguments at default values
#' estimateProdOutputs <- estimateProd(inputDataListVstMass = estimateWoodMassOutputs, 
#' inputDataListHbpMass = scaleHerbMassOutputs)
#'                       
#' #  Example specifying many non-default arguments
#' estimateWoodProdOutputs <- estimateWoodProd(inputDataListVstMass = estimateWoodMassOutputs, 
#' plotType = "all", 
#' plotPriority = "all", 
#' calcMethod = "approach_2", 
#' outlier = 2, 
#' outlierType = "SD")
#' 
#' }
#' 
#' @export estimateProd

estimateProd = function(dataProducts = c("Vst","Hbp"),
                        inputDataListVstMass = NA,
                        inputDataListHbpMass = NA,
                        plotType = "tower",
                        plotPriority = "5",
                        calcMethod = "approach_1",
                        outlier = 1.5,
                        outlierType = "IQR"
                         ) {
  

  ######################################################  

  if("Vst" %in% dataProducts){  
  print("Calculating above-ground woody productivity  ..... ")

  estimateWoodProdOutputs <- estimateWoodProd(inputDataList = inputDataListVstMass, 
                                              plotType = plotType, 
                                              plotPriority = plotPriority, 
                                              calcMethod = calcMethod, 
                                              outlier = outlier, 
                                              outlierType = outlierType)
  
  if(calcMethod == "approach_2"){                
  Vst <- estimateWoodProdOutputs$vst_ANPP_site_2
  } else {
  Vst <- estimateWoodProdOutputs$vst_ANPP_site
  }

  }

  ######################################################  
  
  if ("Hbp" %in% dataProducts) { 
  print("Calculating above-ground herbaceous productivity  ..... ")
  }
  
  estimateHerbProdOutputs <- estimateHerbProd(inputDataList = inputDataListHbpMass, 
                                              plotType = plotType, 
                                              plotPriority = plotPriority)
  
  Hbp <- estimateHerbProdOutputs$herb_ANPP_site
  
  ######################################################  
  
  print("Combining productivity from selected data products and returning productivity output objects as a list object ..... ")
  
  productivity_list <- mget(dataProducts) # this works if the final summary dataframes to be joined have the exact names as the values in the dataProducts argument
  productivity_site <- productivity_list %>% purrr::reduce(dplyr::full_join, by=c("siteID", "year") )
  
  
  if("woodANPPMean_Mghayr" %in% colnames(productivity_site) & "herbANPPMean_Mghayr" %in% colnames(productivity_site)){
  productivity_site$ANPP_Mghayr <- productivity_site$woodANPPMean_Mghayr + productivity_site$herbANPPMean_Mghayr
  productivity_site$herb_percent_of_ANPP <- round((100 * productivity_site$herbANPPMean_Mghayr / productivity_site$ANPP_Mghayr ), 1)
  
  productivity_site <- productivity_site %>% dplyr::select("siteID", "year", "outlier_threshold", "woodPlotNum", "woodANPPSD_Mghayr", "woodANPPMean_Mghayr", 
                                                           "herbPlotNum", "herbANPPSD_Mghayr", "herbANPPMean_Mghayr", "ANPP_Mghayr", "herb_percent_of_ANPP")
  }
  
  
  if("Vst" %in% dataProducts & !("Hbp" %in% dataProducts)){
    output.list <- list(
    productivity_site = productivity_site,
    estimateWoodProdOutputs = estimateWoodProdOutputs
    )
  }

  if("Hbp" %in% dataProducts & !("Vst" %in% dataProducts)){
  output.list <- list(
    productivity_site = productivity_site,
    estimateHerbProdOutputs = estimateHerbProdOutputs
    )
  }
  
  if("Vst" %in% dataProducts & "Hbp" %in% dataProducts){  # overwrites earlier list if both are present 
  output.list <- list(
    productivity_site = productivity_site,
    estimateWoodProdOutputs = estimateWoodProdOutputs,
    estimateHerbProdOutputs = estimateHerbProdOutputs
    )
  }
  return(output.list)

}
