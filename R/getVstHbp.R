##############################################################################################
#' @title getVstHbp.R

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Download NEON vegetation structure (VST) data and optionally aboveground herbaceous biomass (HBP) data for usage in NEONbiomass function.
#' 
#' changelog and author contributions / copyrights
#' Samuel M Simkin (2023-09-14)  original creation

#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all. [list]
#' @param start Either NA, meaning all available years starting with 2018, or a character vector specifying start year in the form YYYY, e.g. 2019. The default and earliest allowable option is 2018. [character]
#' @param end Either NA, meaning all available years after the start year, or a character vector specifying start year in the form YYYY, e.g. 2021. Data from the current calendar year are excluded since they would be incomplete. [character]
#' @param dataProducts Select whether to analyse only woody biomass "Vst" or woody plus herbaceous "VstHbp" [character]
#

#' @details All available data from the NEON "Vegetation structure" data product (dpID "DP1.10098.001") meeting the site and year query criteria 
#' will be downloaded using the neonUtilities::loadByProduct function. If dataProducts option "VstHbp" is selected then "Herbaceous clip harvest (dpID "DP1.10023.001") 
#' data are also downloaded using the neonUtilities::loadByProduct function.

#' @return This function returns NEON "Vegetation structure" (VST) data from the NEON data portal, and optionally "Herbaceous clip harvest" (HBP) data as well.
#' 'VstDat' Vegetation structure data from the NEON portal
#' 'HbpDat' Herbaceous clip harvest data from the NEON portal (optional)

#' @examples
#' \dontrun{
#' # example with arguments at default values
#' VstHbpData <- getVstHbp(site="all")
#' 
#' # example specifying non-default arguments
#' VstHbpData <- getVstHbp(siteID = c("HARV","JERC"), 
#'          start = "2019", end = "2022", dataProducts = "Vst")
#' 
#' list2env(VstHbpData ,.GlobalEnv) # unlist all data frames for easier viewing or additional analysis
#' saveRDS(VstHbpData$VstDat, 'VstDat.rds') # save vst portal data locally for use in NEONbiomass function
#' saveRDS(VstHbpData$HbpDat, 'HbpDat.rds') # save hbp portal data locally for use in NEONbiomass function
#' 
#' }

##############################################################################################

getVstHbp = function(site = "all",
                     start = "2018", 
                     end = as.character(as.integer(format(Sys.Date(), "%Y"))-1),
                     dataProducts = "VstHbp"
                         ) {

library(neonUtilities)
library(tidyverse)

# Error if invalid dataProducts option selected
  if(dataProducts != "VstHbp" & dataProducts != "Vst"){
    stop("Currently the only valid dataProducts options are 'VstHbp' or 'Vst'.")
  }

# Warning if start date is too early
  if(as.numeric(start) < 2018){  start = "2018"
    print("The earliest year that can be used for this function is 2018. The start year has automatically been changed to 2018.")
  }

# Warning if end date is too late
  if(as.numeric(end) > as.integer(format(Sys.Date(), "%Y"))-1){  end = as.character(as.integer(format(Sys.Date(), "%Y"))-1)
    print("The end year can not be the current calendar year or a future year. The end year has automatically been changed to the year prior to the current calendar year.")
  }
  
print("Downloading NEON 'Vegetation structure' data (dpID DP1.10098.001)  ..... ")
#### ingest tree, sapling, shrub, and liana data (plus non-herbaceous perennial other data) from portal   

VstDat <- loadByProduct(dpID="DP1.10098.001", 
                             site = site,
                             startdate = paste0(start,"-01"),
                             enddate = paste0(end, "-12"),
                             package = "basic", check.size = FALSE, token = Sys.getenv('NEON_TOKEN')) # pulled from portal with loadByProduct on 2023 Jul 11


##### If option to include herbaceous data was selected then download the herbaceous data #############################
if(grepl("Hbp", dataProducts) )    {
# Original hbp scripts by Eric Sokol (esokol@battelleecology) in May 2020. Merged and modified by Sam Simkin (ssimkin@battelleecology.org) in Jul 2023
  

print("Downloading NEON 'Herbaceous clip harvest' data (dpID DP1.10023.00)  ..... ")
  
HbpDat <- neonUtilities::loadByProduct(dpID = "DP1.10023.001", site = site, startdate = paste0(start,"-01"), enddate = paste0(as.character(as.integer(format(Sys.Date(), "%Y"))-1), "-12"), 
                                           package = "basic", check.size = FALSE, token = Sys.getenv('NEON_TOKEN'))


}


if(grepl("Hbp", dataProducts) )    {
output.list <- list(
   VstDat = VstDat,
   HbpDat = HbpDat
   )
 return(output.list)
  } else {
  output.list <- list(
   VstDat = VstDat
)
  }

}

#VstHbpData <- getVstHbp(site="all")

#list2env(VstHbpData ,.GlobalEnv) # unlist raw data for easier viewing or additional analysis
#saveRDS(VstHbpData$VstDat, 'VstDat.rds') # save raw data locally for further examination and if desired use in follow-up biomass and productivity functions.
#saveRDS(VstHbpData$HbpDat, 'HbpDat.rds') # save raw data locally for further examination and if desired use in follow-up biomass and productivity functions.


