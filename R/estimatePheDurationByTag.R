#' @title Estimate Phenophase Duration by Tag
#'
#' @author Katie Jones \email{kjones@battelleecology.org} \cr
#'
#' @description Data from the NEON Plant Phenology Observation (DP1.10055.001) product are used to calculate phenophase duration for each phenophase identified by the neonPlants::estimatePheTransByTag() function for the time frame provided in the input data frame. Each duration includes additional fields including an annual count of transitions reported for the given individual x phenophase combination, the start and end date and day of year for each phenophase, and the precision around the duration estimate. Required inputs are either a list of data frames as returned from neonUtilities::loadByProduct() that must include a data frame titled "phe_statusintensity" and one titled "phe_perindividual", or two individual data frames corresponding to the "phe_statusintensity" table and the "phe_perindividual" table. 
#' 
#' @details Input data may be provided either as a list generated from the neonUtilities::laodByProduct() function or as individual tables. However, if both list and table inputs are provided at the same time the function will error.
#' 
#' For table joining to be successful, inputs must contain data from the same sites for all tables. When individualID duplicates exist in the "phe_perindividual" table, the function will attempt to resolve them based on the editedDate field.
#' 
#' Phenophases may begin in one year and end in another. For the most part this happens in southern sites, but users should be alert for it at any site. estimatePheDurationByTag() calls estimatePheTransByTag() to find the dates of phenophase transitions, and uses yearPhenophaseBegan for those calculations.
#' 
#' @param inputDataList A list of data frames returned from the neonUtilities::loadByProduct() function. [list]
#' @param inputStatus A data frame with phenological observation data, either the "phe_statusintensity" table or equivalent. [data.frame]
#' @param inputTags A data frame with taxon data for individuals present in the inputStatus dataframe, either the "phe_perindividual" table or equivalent. [data.frame]
#'
#' @return The time series of phenophase transitions created by the neonPlants::estimatePheTransByTag() function is used to calculate the duration of each phenophase for each individual for the time frame provided in the input data frame. Calculated values include: 
#'  * yearPhenophaseBegan - calendar year of the estimated onset of the phenophase
#'  * dateTransitionStart - calendar date of the estimated phenophase onset
#'  * doyTransitionStart - ordinal day of year of the estimated phenophase onset
#'  * dateTransitionEnd - calendar date of the estimated phenophase end
#'  * doyTransitionEnd - ordinal day of year of the estimated phenophase end
#'  * duration - difference in days from the onset to the end of the phenophase
#'  * transitionType - indicator that output is for phenophase duration
#'  * precisionDuration - sum of precisionDays for estimated onset and end
#'  * nthTransition - count of onset events per individualID and phenophase name for each year (yearPhenophaseBegan)
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
                                 inputTags = inputTags,
                                 began = TRUE)
  
  out <- trans %>%
    dplyr::group_by(.data$yearPhenophaseBegan, 
                    .data$siteID, 
                    .data$individualID, 
                    .data$taxonID, 
                    .data$scientificName, 
                    .data$phenophaseName, 
                    .data$nthTransition) %>%

    dplyr::reframe(dateTransitionStart = .data$dateTransition[.data$transitionType == 'onset'],
                   doyTransitionStart = lubridate::yday(.data$dateTransition[.data$transitionType == 'onset']),
                   dateTransitionEnd = .data$dateTransition[.data$transitionType == 'end'], 
                   doyTransitionEnd = lubridate::yday(.data$dateTransition[.data$transitionType == 'end']),
                   duration = as.numeric(lubridate::date(.data$dateTransition[.data$transitionType == 'end']) - 
                                         lubridate::date(.data$dateTransition[.data$transitionType == 'onset'])),
                   precisionDuration = sum(.data$precisionDays), 
                   transitionType = 'duration')
  
  return(out)
}
