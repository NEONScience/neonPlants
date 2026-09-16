#' @title Estimate NEON Phenophase Duration by Tag
#'
#' @author
#' Katie Jones \email{joneseyk@gmail.com} \cr
#' Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Data from the NEON Plant Phenology Observation (DP1.10055.001) product are used to calculate phenophase duration for each phenophase identified by the neonPlants::estimatePheTransByTag() function for the time frame provided in the input data frame. Each duration is accompanied by additional fields, including an annual count of transitions reported for the given individual x phenophase combination, the start and end date and day of year for each phenophase, and the precision around the duration estimate. Required inputs are either a list of data frames as returned from neonUtilities::loadByProduct() that must include a data frame titled "phe_statusintensity" and one titled "phe_perindividual", or two individual data frames corresponding to the "phe_statusintensity" table and the "phe_perindividual" table.
#'
#' @details Input data may be provided either as a list generated from the neonUtilities::laodByProduct() function or as individual tables. However, only list or table inputs are allowed (not a mix of both).
#'
#' For table joining to be successful, inputs must contain data from the same sites for all tables. When individualID duplicates exist in the "phe_perindividual" table, the function will attempt to resolve them based on the 'editedDate' field.
#'
#' Phenophases may begin in one year and end in another; or the most part this happens in southern sites, but users should be alert for this possibility at any site. The estimatePheDurationByTag() function calls estimatePheTransByTag() to find the dates of phenophase transitions, and uses yearPhenophaseBegan for those calculations.
#'
#' @param inputDataList A list of data frames returned from the neonUtilities::loadByProduct() function. [list]
#'
#' @param inputStatus A data frame with phenological observation data, either the "phe_statusintensity" table or equivalent. [data.frame]
#'
#' @param inputTags A data frame with taxon data for individuals present in the inputStatus dataframe, either the "phe_perindividual" table or equivalent. [data.frame]
#'
#' @return The following objects are returned as a list:
#'   * phe_duration_tag - Table containing the duration of each phenophase for each individual for the timeframe provided in hte input data. Phenophase transition dates required for the calculation are derived via the neonPlants::estimatePheTransByTag() function.
#'   * variables - Units and definitions of novel variables created by the function that are not already defined in the Plant Phenology data product.
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @examples
#' \dontrun{
#' #   Obtain NEON Plant Phenology data; note that a token is required and may be obtained after creating a NEON user account
#' phe <- neonUtilities::loadByProduct(
#'   dpID = "DP1.10055.001",
#'   site = "UKFS",
#'   startdate = "2022-01",
#'   enddate = "2022-12",
#'   package = "basic",
#'   check.size = FALSE,
#'   token = "my_NEON_token"
#'   )
#'
#' out <- estimatePheDurationByTag(inputDataList = phe)
#' }
#'
#' @export estimatePheDurationByTag


estimatePheDurationByTag <- function(inputDataList = NULL,
                                     inputStatus = NULL,
                                     inputTags = NULL) {


  ###  Generate transition data ####
  trans <- estimatePheTransByTag(inputDataList = inputDataList,
                                 inputStatus = inputStatus,
                                 inputTags = inputTags,
                                 began = TRUE)

  trans <- trans$phe_transition_tag



  ### Calculate duration data ####
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



  ### Variables: Process function-specific variables for output ####
  data("variables", envir = environment())

  variables <- variables %>%
    dplyr::filter(.data$functionName == "estimatePheDurationByTag")



  ### Return output ####
  output <- list(phe_duration_tag = out,
                 variables = variables)

  return(output)
}
