#' @title Estimate Phenophase Duration by Tag
#'
#' @author Katie Jones \email{kjones@battelleecology.org} \cr
#'
#' @description Data from the NEON Plant Phenology Observation (DP1.10055.001) product are used to calculate phenophase duration for each phenophase transition identified by the neonPlants::estimatePheTransByTag() function for the time frame provided in the input data frame. Each  duration includes additional fields describing number of transitions reported for the given individual x phenophase combination, the start and end date and day of year, and the precision around the duration estimate. Required inputs are either a list of data frames as returned from neonUtilities::loadByProduct() that must include a data frame titled "phe_statusintensity" and one titled "phe_perindividual", or two individual data frames corresponding to the "phe_statusintensity" table and the "phe_perindividual" table. 
#' 
#' @details Input data may be provided either as a list generated from the neonUtilities::laodByProduct() function or as individual tables. However, if both list and table inputs are provided at the same time the function will error.
#' 
#' For table joining to be successful, inputs must contain data from the same sites for all tables. When individualID duplicates exist in the "phe_perindividual" table, the function will error. If this occurs when providing data via the inputDataList argument, extract individual data frames from the list, resolve duplicates, and re-run with separate inputStatus and inputTags inputs.  
#' 
#' @param inputDataList A list of data frames returned from the neonUtilities::loadByProduct() function. [list]
#' 
#' @param inputStatus A data frame with phenological observation data, either the "phe_statusintensity" table or equivalent. [data.frame]
#' 
#' @param inputTags A data frame with taxon data for individuals present in the inputStatus dataframe, either the "phe_perindividual" table or equivalent. [data.frame]
#'
#' @return The time series created by the neonPlants::estimatePheTransByTag() function is used to calculate phenophase durations for the time frame provided in the input data frame. Calculated values include: 
#'  * dateTransitionStart - calendar date of the estimated transition onset
#'  * doyTransitionStart - ordinal day of year of the estimated transition onset
#'  * dateTransitionEnd - calendar date of the estimated transition end
#'  * doyTransitionEnd - ordinal day of year of the estimated transition end
#'  * duration - difference in days from the onset day of year to the transition end
#'  * transitionType - indicator that output is for phenophase duration
#'  * precisionDuration - sum of precisionDays for estimated oneset and end
#'  * nth transition - a count of onset events per individualID, phenophase name, within a given calendar year
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @examples
#' \dontrun{
#'
#' # load additional packages for these examples
#' library(neonUtilities) 
#'
#' # get data
#' pheDat <- neonUtilities::loadByProduct(
#'   dpID = "DP1.10055.001",
#'   site = "UKFS",
#'   startdate = "2022-01",
#'   enddate = "2022-12",
#'   package = "basic",
#'   check.size = FALSE
#'   )
#'
#'out <- estimatePheDurationByTag(inputDataList = pheDat)
#'
#'out2 <- estimatePheDurationByTag(inputStatus = pheDat$phe_statusintensity,
#'                                 inputTags = pheDat$phe_perindividual)
#' }
#' 
#' @export estimatePheDurationByTag


estimatePheDurationByTag <- function(inputDataList = NULL,
                                     inputStatus = NULL,
                                     inputTags = NULL) {
  
  trans <- estimatePheTransByTag(inputDataList = inputDataList,
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
                   duration = .data$doyTransition[.data$transitionType == 'end'] - .data$doyTransition[.data$transitionType == 'onset'],
                   precisionDuration = sum(.data$precisionDays), 
                   transitionType = 'duration')

  return(out)
}
