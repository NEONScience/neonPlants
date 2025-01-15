##############################################################################################
#' @title Combine productivity from multiple NEON data products

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Wrapper function to combine productivity summaries of NEON data products. Currently supported data products are "Vegetation structure" 
#' (DP1.10098.001), estimated with the estimateWoodProd function, and "Herbaceous clip harvest (DP1.10023.001), estimated with the estimateHerbProd function. 
#' The data input is a list of biomass dataframes created by each of the contributing functions. For example, estimateWoodMassOutputs from the estimateWoodMass 
#' function and scaleHerbMassOutputs from the scaleHerbMass function.

#' @details A site-level summary of NEON productivity from the selected NEON data products is provided as a dataframe, in addition to list objects with more detailed 
#' outputs for each of the selected NEON data products. For more documentation details see the documentation for the companion functions (e.g. estimateWoodProd 
#' and estimateHerbProd).
#' 
#' @param dataProducts Specify a character vector with the NEON data products to be combined. Defaults to c("Vst","Hbp") for Vegetation structure and Herbaceous clip harvest, respectively. [character]
#' @param inputWoodMass Specify a loaded R list object (e.g. estimateWoodMassOutputs) that was produced by companion estimateWoodMass.R function. [character]
#' @param inputHerbMass Specify a loaded R list object (e.g. scaleHerbMassOutputs) that was produced by companion scaleHerbMass.R function. [character]
#' @param plotType Optional filter based on NEON plot type. Defaults to "tower" plots, which are sampled annually. Otherwise "distributed" plots are examined also, if included in the input .rds. [character]
#' @param plotPriority NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. The default is 5. [numeric]
#' @param calcMethod Select plot-level (approach 2) or individual-level (approach 1) productivity calculations (for woody data). The default is "approach_1" [character]
#' @param outlier Specify how much (if any) outlier removal should be performed (for woody data). The default is 1.5. [numeric]
#' @param outlierType Specify the type of outlier (for woody data), either SD (standard deviations) or IQR (interquartile range). The default is "IQR". [character]
#' 
#' @return This function returns a site-level summary dataframe "productivity_site" with NEON productivity (units are Megagrams per hectare per year) from the selected NEON data products. 
#' In addition, list objects (e.g. estimateWoodProdOutputs and estimateHerbProdOutputs) with more granular data summaries are returned for each of the selected NEON data products.
#' More detailed documentation is provided in the documentation of contributing functions (e.g. estimateWoodProd and estimateHerbProd).

#' @examples
#' \dontrun{
#' # example with arguments at default values
#' estimateProdOutputs <- estimateProd(inputWoodMass = estimateWoodMassOutputs, 
#'                                     inputHerbMass = scaleHerbMassOutputs)
#'                       
#' # example specifying many non-default arguments
#' estimateWoodProdOutputs <- estimateWoodProd(input = estimateWoodMassOutputs, 
#'                plotType = "all", plotPriority = 50, 
#'                calcMethod = "approach_2", outlier = 2, outlierType = "SD")
#' 
#' list2env(estimateWoodProdOutputs ,.GlobalEnv) # unlist all data frames for easier viewing
#' saveRDS(estimateWoodProdOutputs, 'estimateWoodProdOutputs.rds') # save output locally
#' }

# changelog and author contributions / copyrights
#   Samuel M Simkin (2024-01-09)  original creation
#   Samuel M Simkin (2024-01-15)  revised
#################################################################################  

estimateProd = function(
                         dataProducts = c("Vst","Hbp"),
                         inputWoodMass,
                         inputHerbMass = NA,
                         plotType = "tower",
                         plotPriority = 5,
                         calcMethod = "approach_1",
                         outlier = 1.5,
                         outlierType = "IQR"
                         ) {
  
options(dplyr.summarise.inform = FALSE)

if(is.na(inputWoodMass[1]) & is.na(inputHerbMass[1])){
    stop("An input list object from at least one companion mass function is required.")
  }    
  
if(!is.na(inputWoodMass[1])){  

if(!methods::is(inputWoodMass, class = "list" )){
  stop("The inputWoodMass argument is expected to be a list. A character or data.frame argument is not allowed.")
  }

list2env(inputWoodMass ,.GlobalEnv)
vst_agb_per_ha <- inputWoodMass$vst_agb_per_ha
vst_agb_zeros <- inputWoodMass$vst_agb_zeros
vst_plot_w_0s <- inputWoodMass$vst_plot_w_0s
vst_site <- inputWoodMass$vst_site

#   Check that required tables within list match expected names
listExpNames <- c("vst_agb_per_ha", "vst_plot_w_0s", "vst_agb_zeros", "vst_site")
if (length(setdiff(listExpNames, names(inputWoodMass))) > 0) {
      stop(glue::glue("Required tables missing from inputWoodMass list:",
                      '{paste(setdiff(listExpNames, names(inputWoodMass)), collapse = ", ")}',
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
  
# Error if invalid calcMethod selected
  if(calcMethod != "approach_1" & calcMethod != "approach_2"){
    stop("The only valid outlierType options are 'approach_1' or 'approach_2'.")
  }

# Error if invalid outlierType selected
  if(outlierType != "SD" & outlierType != "IQR"){
    stop("The only valid calcMethod options are 'SD' or 'IQR'.")
  }

### Verify input tables contain required columns and data ####

### Verify user-supplied vst_agb_per_ha table contains required data
#   Check for required columns
  vst_agb_per_ha_ExpCols <- c("siteID", "plotID", "eventID", "year", "plot_eventID", "nlcdClass", "taxonID", "individualID", "plantStatus2", "agb_Mgha")
  
  if (length(setdiff(vst_agb_per_ha_ExpCols, colnames(vst_agb_per_ha))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_agb_per_ha':", '{paste(setdiff(vst_agb_per_ha_ExpCols, colnames(vst_agb_per_ha)), collapse = ", ")}',
                    .sep = " "))
  }
  
#   Check for data
  if (nrow(vst_agb_per_ha) == 0) {
    stop(glue::glue("Table 'vst_agb_per_ha' has no data."))
}
  
### Verify user-supplied vst_agb_zeros table contains required data
#   Check for required columns
  vst_agb_zeros_ExpCols <- c("siteID", "plotID", "eventID", "year", "plot_eventID", "plotType")
  
  if (length(setdiff(vst_agb_zeros_ExpCols, colnames(vst_agb_zeros))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_agb_zeros':", '{paste(setdiff(vst_agb_zeros_ExpCols, colnames(vst_agb_zeros)), collapse = ", ")}',
                    .sep = " "))
  }

  
### Verify user-supplied vst_plot_w_0s table contains required data
#   Check for required columns
  vst_plot_w_0s_ExpCols <- c("domainID", "siteID", "plotID", "eventID", "year", "plot_eventID", "nlcdClass", "taxonID", "agb_Mgha__Live", "agb_Mgha__Dead_or_Lost",
                   "specificModuleSamplingPriority","plotType")
  
  if (length(setdiff(vst_plot_w_0s_ExpCols, colnames(vst_plot_w_0s))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_plot_w_0s':", '{paste(setdiff(vst_plot_w_0s_ExpCols, colnames(vst_plot_w_0s)), collapse = ", ")}',
                    .sep = " "))
  }
  
#   Check for data
  if (nrow(vst_plot_w_0s) == 0) {
    stop(glue::glue("Table 'vst_plot_w_0s' has no data."))
}
  
### Verify user-supplied vst_site contains required data
#   Check for required columns
  vst_site_ExpCols <- c("siteID","year","woodPlotNum","woodLiveMassMean_Mgha","woodLiveMassSD_Mgha")
  
  if (length(setdiff(vst_site_ExpCols, colnames(vst_site))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_site':", '{paste(setdiff(vst_site_ExpCols, colnames(vst_site)), collapse = ", ")}',
                    .sep = " "))
  }
  
#   Check for data
  if (nrow(vst_site) == 0) {
    stop(glue::glue("Table 'vst_site' has no data."))
}


start <- min(vst_plot_w_0s$year)
end  <- max(vst_plot_w_0s$year)


# Error if not at least 2 years of data
  if(as.numeric(end) - as.numeric(start) < 1){
    print("At least 2 years of data are needed to calculate productivity (more for plots sampled less frequently than annually). Current dataset only has woody biomass data from: ")
    print(unique(vst_agb_per_ha$year))
    stop( )
  }


print("Calculating above-ground woody productivity  ..... ")

estimateWoodProdOutputs <- estimateWoodProd(input = inputWoodMass, plotType = plotType, plotPriority = plotPriority, calcMethod = calcMethod, 
                                                          outlier = outlier, outlierType = outlierType)

if(calcMethod == "approach_2"){                
Vst <- estimateWoodProdOutputs$vst_ANPP_site_2
} else {
Vst <- estimateWoodProdOutputs$vst_ANPP_site
}

}

######################################################  
  
  
if(!is.na(inputHerbMass[1])){  
  
if(!methods::is(inputHerbMass, class = "list" )){
  stop("The inputHerbMass argument is expected to be a list. A character, data.frame, or NA argument is not allowed.")
  }

list2env(inputHerbMass ,.GlobalEnv)
hbp_agb <- inputHerbMass$hbp_agb
hbp_plot <- inputHerbMass$hbp_plot


listExpNames_Hbp <- c("hbp_agb", "hbp_plot", "hbp_site")
if (length(setdiff(listExpNames_Hbp, names(inputHerbMass))) > 0) {
      stop(glue::glue("Required tables missing from inputHerbMass list:",
                      '{paste(setdiff(listExpNames_Hbp, names(inputHerbMass)), collapse = ", ")}',
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


# Error if not at least 2 years of data
  if(as.numeric(end) - as.numeric(start) < 1){
    print("At least 2 years of data are needed to calculate productivity (more for plots sampled less frequently than annually). Current dataset only has biomass data from: ")
    print(unique(hbp_plot$year))
    stop( )
  }

print("Calculating above-ground herbaceous productivity  ..... ")
}

estimateHerbProdOutputs <- estimateHerbProd(input = inputHerbMass, plotType = plotType, plotPriority = plotPriority)

Hbp <- estimateHerbProdOutputs$herb_ANPP_site
  
#############################################  
  
print("Combining productivity from selected data products and returning productivity output objects as a list object ..... ")

productivity_list <- mget(dataProducts) # this works if the final summary dataframes to be joined have the exact names as the values in the dataProducts argument
productivity_site <- productivity_list %>% purrr::reduce(dplyr::full_join, by=c("siteID", "year") )


if("wood_ANPP__Mg_ha_yr" %in% colnames(productivity_site) & "herb_ANPP__Mg_ha_yr" %in% colnames(productivity_site)){
productivity_site$ANPP__Mg_ha_yr <- productivity_site$wood_ANPP__Mg_ha_yr + productivity_site$herb_ANPP__Mg_ha_yr
productivity_site$ANPP__Mg_ha_yr_se <- productivity_site$wood_ANPP__Mg_ha_yr_se + productivity_site$herb_ANPP__Mg_ha_yr_se
productivity_site$herb_percent_of_ANPP <- round((100 * productivity_site$herb_ANPP__Mg_ha_yr / productivity_site$ANPP__Mg_ha_yr ), 1)
productivity_site$herb_percent_of_ANPP_se <- round((abs(productivity_site$herb_percent_of_ANPP) * sqrt((productivity_site$herb_ANPP__Mg_ha_yr_se / productivity_site$herb_ANPP__Mg_ha_yr)^2 + 
                                                    (productivity_site$ANPP__Mg_ha_yr_se / productivity_site$ANPP__Mg_ha_yr)^2) ), 1)

productivity_site <- productivity_site %>% dplyr::select("siteID", "year", "wood_N", "wood_count_type", "wood_ANPP__Mg_ha_yr_se", "wood_ANPP__Mg_ha_yr", "herb_N", "herb_ANPP__Mg_ha_yr_se", "herb_ANPP__Mg_ha_yr", "ANPP__Mg_ha_yr", 
                                          "herb_percent_of_ANPP_se", "herb_percent_of_ANPP")
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
