##############################################################################################
#' @title Download vegetation structure (VST) data and optionally aboveground herbaceous biomass (HBP) data from NEON portal

#' @author
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr

#' @description Download NEON vegetation structure (VST) data and optionally aboveground herbaceous biomass (HBP) data for usage in calculateBiomass function.

#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all. [list]
#' @param start Either NA, meaning all available years starting with 2018, or a character vector specifying start year in the form YYYY, e.g. 2019. The default and earliest allowable option is 2018. [character]
#' @param end Either NA, meaning all available years after the start year, or a character vector specifying start year in the form YYYY, e.g. 2021. Data from the current calendar year are excluded since they would be incomplete. [character]
#' @param dataProducts Select whether to analyse only woody biomass "Vst" or woody plus herbaceous "VstHbp" [character]
#

#' @details All available data from the NEON "Vegetation structure" data product (dpID "DP1.10098.001") meeting the site and year query criteria 
#' will be downloaded using the neonUtilities::loadByProduct function. If dataProducts option "VstHbp" is selected then "Herbaceous clip harvest (dpID "DP1.10023.001") 
#' data are also downloaded using the neonUtilities::loadByProduct function.

#' @return This function returns a list of lists. The returned parent list includes at least one list, named 'VstDat', with "Vegetation structure" (VST) data from the NEON data portal.
#' If the dataProducts parameter is "VstHbp" then the returned parent list also includes a second list, named 'HbpDat', with "Herbaceous clip harvest" (HBP) data from the NEON portal.

#' @examples
#' \dontrun{
#' 
#' # example with arguments at default values
#' VstHbpData <- getBiomassInputs(site="all")
#' 
#' # examples specifying multiple non-default arguments
#' VstHbpData <- getBiomassInputs(site="HARV", start = 2021, end = 2021, dataProducts = "Vst")
#' 
#' VstHbpData <- getBiomassInputs(siteID = c("HARV","JERC"), 
#'          start = "2019", end = "2022", dataProducts = "Vst")
#' 
#' list2env(VstHbpData ,.GlobalEnv) # unlist VstDat list (and optionally HbpDat list)
#' saveRDS(VstHbpData$VstDat, 'VstDat.rds') # save vst data locally
#' saveRDS(VstHbpData$HbpDat, 'HbpDat.rds') # save hbp data locally
#' 
#' }

# changelog and author contributions / copyrights
# Samuel M Simkin (2023-09-14)  original creation
# Samuel M Simkin (2024-09-30)  revised
##############################################################################################

getBiomassInputs = function(site = "all",
                     start = 2018, 
                     end = as.integer(format(Sys.Date(), "%Y"))-1,
                     dataProducts = "VstHbp"
                         ) {

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


start_mod <- paste0(start,"-01")
end_mod <- paste0((end+1),"-07")

VstDat <- neonUtilities::loadByProduct(dpID="DP1.10098.001", 
                             site = site,
                             startdate = start_mod,
                             enddate = end_mod,
                             package = "basic", check.size = FALSE, token = Sys.getenv('NEON_PAT'))

### filter to sampling events that started in specified date range
list2env(VstDat, envir=.GlobalEnv)
vstList <- names(VstDat)
vst_perplotperyear <- vst_perplotperyear %>% filter(as.numeric(substr("eventID", 10,13)) >= start & as.numeric(substr("eventID", 10,13)) <= end)
vst_apparentindividual <- vst_apparentindividual %>% filter(as.numeric(substr("eventID", 10,13)) >= start & as.numeric(substr("eventID", 10,13)) <= end)
`vst_non-woody` <- `vst_non-woody` %>% filter(as.numeric(substr("eventID", 10,13)) >= start & as.numeric(substr("eventID", 10,13)) <= end)
VstDat <-  mget(vstList, envir = sys.frame())

##### If option to include herbaceous data was selected then download the herbaceous data #############################
if(grepl("Hbp", dataProducts) )    {
# Original hbp scripts by Eric Sokol (esokol@battelleecology) in May 2020. Merged and modified by Sam Simkin (ssimkin@battelleecology.org) in Jul 2023
  

print("Downloading NEON 'Herbaceous clip harvest' data (dpID DP1.10023.00)  ..... ")
  
HbpDat <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                             site = site,
                             startdate = paste0(start,"-01"),
                             enddate = paste0(as.character(as.integer(format(Sys.Date(), "%Y"))-1), "-12"),
                             package = "basic", check.size = FALSE, token = Sys.getenv('NEON_PAT'))


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
