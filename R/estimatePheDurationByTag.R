#' @title Estimate Phenophase Duration by Tag
#'
#' @author
#' Katie Jones \email{kjones@battelleecology.org} \cr
#'
#' @description This function uses observation data from the NEON Plant Phenology Observation (DP1.10055.001) to calculate phenophase duration for each phenophase transition identified by the neonPlants::estimatePheTransByTag function for the time frame provided in the input data frame. Each  duration includes additional fields describing number of transitions reported for the given individual x phenophase combination, the start and end date and day of year, and the precision around the duration estimate. Required inputs are either a list of data frames (inputDataList) as returned from neonUtilities::loadByProduct() that must include a data frame titled "phe_statusintensity" and one titled "phe_perindividual". Alternatively, the function will accept two individual data frames, inputStatus, corresponding to the phe_statusintensity table and inputTags, corresponding to the phe_perindividual table. However, if both list and table inputs are provided at the same time the function will error out
#'
#' @param inputDataList a list of data frames returned from neonUtilities::loadByProduct() [list]
#' @param inputStauts a data frame with phenological observation data [data.frame]
#' @param inputTag a data frame with taxon data for individuals present in inputStatus dataframe [data.frame]
#' 
#' 
#' @details
#'
#' @return This function uses the time series created by the neonPlants::estimatePheTransByTag function to calculate phenophase durations for the time frame provided in the input data frame. Calculated values include: 
#'  * dateTransitionStart - calendar date of the estimated transition onset
#'  * doyTransitionStart - ordinal day of year of the estimated transition onset
#'  * dateTransitionEnd - calendar date of the estimated transition end
#'  * doyTransitionEnd - ordinal day of year of the estimated transition end
#'  * duration - difference in days from the onset day of year to the transition end
#'  * transitionType - indicator that output is for phenophase duration
#'  * precisionDuration - sum of precisionDays for estimated oneset and end
#'  * nth transition - a count of onset events per individualID, phenophase name, within a given calendar year
#'
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
#'out2 <- estimatePheDurationByTag(inputStatus = pheDat$phe_statusintensity,
#'                                 inputTags = pheDat$phe_perindividual)
#' }

##############################################################################################


estimatePheDurationByTag <- function(
    inputDataList = NULL,
    inputStatus = NULL,
    inputTags = NULL) {
  
  trans <- estimatePheTransByTag(inputDataList=inputDataList,
                                 inputStatus = inputStatus,
                                 inputTags = inputTags)

  out <- trans %>%
    dplyr::group_by(.data$year, 
                    .data$siteID, 
                    .data$individualID, 
                    .data$taxonID, 
                    .data$scientificName, 
                    .data$phenophaseName, 
                    .data$nthTransition) %>%

    dplyr::reframe(dateTransitionStart = min(.data$dateTransition),
                   doyTransitionStart = min(.data$doyTransition),
                   dateTransitionEnd = max(.data$dateTransition), 
                   doyTransitionEnd = max(.data$doyTransition),
                   duration = .data$doyTransition[.data$transitionType=='end']-.data$doyTransition[.data$transitionType=='onset'],
                   precisionDuration = sum(.data$precisionDays), ## does sum of precision_days make sense for duration metrics?
                   transitionType = 'duration')

  return(out)
}

