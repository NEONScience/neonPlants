#' @title Estimate Phenophase Duration by Tag
#'
#' @author
#' Katie Jones \email{kjones@battelleecology.org} \cr
#'
#' @description This function uses observation data from the NEON Plant Phenology Observation (DP1.10055.001) to calculate phenophase duration and descriptive statistics.
#'
#'
#' @param pheData data list from NEON Plant Phenology Observation (DP1.10055.001) as returned from neonUtilities::loadByProduct(), or individual dataframes for observations and tags as formatted from NEON Plant Phenology Observation (DP1.10055.001) downloaded from neonUtilities::loadByProduct().
#
#' @details
#' This function uses the time series created by the neonPlants::estimatePheTransByTag function to calculate phenophase durations  for the time frame provided in the input data frame.
#' Calculated values inclued:
#'  * trans_date_start - calendar date of the estimated transition onset
#'  * trans_doy_start - ordinal day of year of the estimated transition onset
#'  * trans_date_end - calendar date of the estimated transition end
#'  * trans_doy_end - ordinal day of year of the estimated transition end
#'  * duration - difference in days from the onset day of year to the transition end 
#'  * transition_type - indicator that output is for phenophase duration
#'  * precision_duration - sum of precision_days for estimated oneset and end
#'  * nth transition - a count of onset events per individualID, phenophase name, within a given calendar year
#'
#'
#' @return This function returns a data frame
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # load additional packages for these examples
#' library(neonUtilities)
#' library(dplyr)
#' library(data.table)
#'
#' # get data
#' pheDat <- loadByProduct(
#'   dpID = "DP1.10055.001",
#'   site = "UKFS",
#'   startdate = "2022-01",
#'   enddate = "2022-12",
#'   package = "basic",
#'   check.size = FALSE)
#'
#'out <- estimatePheDurationByTag(inputDataList = pheDat)
#'
#'WORKS
#'
#'out2 <- estimatePheDurationByTag(inputStatus = pheDat$phe_statusintensity, 
#'                              inputTags = pheDat$phe_perindividual) 
#'
#'FLAGGING NON EXISTANT DUPLICATES                              
#'                              
#' }

##############################################################################################


estimatePheDurationByTag <- function(
    inputDataList = NULL,
    inputStatus = NULL,
    inputTags = NULL
){
  
  trans <- estimatePheTransByTag(inputDataList=inputDataList,
                                    inputStatus = inputStatus,
                                    inputTags = inputStatus)
  
  out <- trans%>%
    dplyr::group_by(year, siteID, individualID, taxonID, scientificName, phenophaseName, nth_transition)%>%
    #filter(n()>1)%>%
    dplyr::summarise(trans_date_start=min(date_transition), trans_doy_start=min(doy_transition), 
              trans_date_end=max(date_transition), trans_doy_end=max(doy_transition),
              duration = doy_transition[transitionType=='end']-doy_transition[transitionType=='onset'], 
              precision_duration=sum(precision_days), ## does sum of precision_days make sense for duration metrics?
              transitionType = 'duration')
  
  return(out)
}


phaseDuration <- estimatePheDurationByTag(
    inputDataList = pheDat)

phaseDuration2 <- estimatePheDurationByTag(
  inputStatus = pheDat$phe_statusintensity,
  inputTags = pheDat$phe_perindividual)
