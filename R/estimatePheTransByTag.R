#' @title Estimate Phenological Transition Dates by Tag
#'
#' @author Katie Jones \email{kjones@battelleecology.org} \cr
#'
#' @description Data from the NEON Plant Phenology Observation data product (DP1.10055.001) are used to calculate phenophase transition dates for each phenophase transition (status = no -> yes or yes -> no) for each tagged plant or patch observed along a NEON phenology transect or within a phenocam plot in the input data set. Additionally, each estimated transition includes fields describing the number of transitions observed for the given individual x phenophase combination and the sampling interval around the estimate. Required inputs are either a list of data frames (inputDataList) as returned from neonUtilities::loadByProduct() that must include a data frame titled "phe_statusintensity" and one titled "phe_perindividual". Alternatively, the function will accept two individual data frames corresponding to the "phe_statusintensity" table and the "phe_perindividual" table.
#' 
#' @details Input data may be provided either as a list generated from the neonUtilities::laodByProduct() function or as individual tables. However, if both list and table inputs are provided at the same time the function will error.
#' 
#' For table joining to be successful, inputs must contain data from the same sites for all tables. When individualID duplicates exist in the "phe_perindividual" table, the function will produce an error. If this occurs when providing data via the inputDataList argument, extract individual data frames from the list, resolve duplicates, and re-run with separate inputStatus and inputTags inputs.
#'
#' @param inputDataList A list of data frames returned from the neonUtilities::loadByProduct() function. [list]
#' 
#' @param inputStatus A data frame with phenological observation data, either the "phe_statusintensity" table or equivalent. [data.frame]
#' 
#' @param inputTags A data frame with taxon data for individuals present in the inputStatus dataframe, either the "phe_perindividual" table or equivalent. [data.frame]
#
#' @return A data frame containing a time series for each phenophase reported for each individual in the data set, including identified  transition dates for beginning and end of a given phenophase, as well as explanatory metrics about that estimate for the time frame provided in the input data frame. Calculated values include:
#'  * transition date - the mid-point between two consecutive dates with different phenophase status values.
#'  * transitionType - the phenophase status values of the transition.
#'  * sampling interval - the number of days between observations.
#'  * uncertainty - sampling interval/2
#'  * nth transition - a count of onset events per individualID, phenophase name, within a given calendar year.
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
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
#'out <- estimatePheTrans(inputDataList = pheDat)
#'
#'out2 <- estimatePheTrans(inputStatus = pheDat$phe_statusintensity,
#'                         inputTags = pheDat$phe_perindividual)
#' }
#' 
#' @export estimatePheTransByTag


estimatePheTransByTag <- function(inputDataList = NULL,
                                  inputStatus = NULL,
                                  inputTags = NULL) {
  
  # Verify that only one input type is provided
  if(!is.null(inputDataList) && !is.null(inputStatus) |!is.null(inputDataList) && !is.null(inputTags)) {
    stop("Please provide either a list of data frames, inputDataList, OR individual data frames for inputStatus and inputTags, but not both.")
  }
  
  # Verify that if inputStatus is provided, inputTags is also provided
  if(!is.null(inputStatus) && is.null(inputTags)) {
    stop("inputStatus provided without inputTags, both data frames are required.")
  }
  
  # Verify that if inputStatus is provided, inputTags is also provided
  if(!is.null(inputTags) && is.null(inputStatus)) {
    stop("inputTags provided without inputStatus, both data frames are required.")
  }
  
  # Verify that list is provided if inputDataList is non-null
  if(!is.null(inputDataList) && !inherits(inputDataList, 'list')) {
    stop(paste("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct(). Supplied input object is class", {class(inputDataList)}))
  }
  
  # Verify that list contains expected data frames
  req_tables <- c("phe_statusintensity", "phe_perindividual")
  
  if(!is.null(inputDataList) && length(setdiff(req_tables, names(inputDataList))) > 0) {
    stop(paste0("Required data frames missing from inputDataList: ",
                paste0(setdiff(req_tables, names(inputDataList)),
                       collapse = ", "),
                sep = " "))
  }
  
  # Verify that df is provided if inputStatus is non-null
  if(!is.null(inputStatus) && !inherits(inputStatus, 'data.frame')) {
    stop(paste("Argument 'inputStatus' must be a data frame object from neonUtilities::loadByProduct(). 
               Supplied input object is class", {class(inputStatus)}))
  }
  
  # Verify that df is provided if inputTags is non-null
  if(!is.null(inputTags) && !inherits(inputTags, 'data.frame')) {
    stop(paste("Argument 'inputTags' must be a data frame object from neonUtilities::loadByProduct(). 
               Supplied input object is class", {class(inputTags)}))
  }
  
  # Verify expected tables are present in inputDataList 
  if(!is.null(inputDataList) && !"phe_statusintensity" %in% ls(inputDataList)) {
    stop("'phe_statusintensity' tables missing from inputDataList")
  }
  
  if(!is.null(inputDataList) && !"phe_perindividual" %in% ls(inputDataList)) {
    stop("'phe_perindividual' tables missing from inputDataList")
  }  
  
  # Verify inputDataList tables have expected columns
  obs_fields <- c("date", "individualID", "phenophaseName", "phenophaseStatus")
  
  tag_fields <- c("individualID", "taxonID", "scientificName", "growthForm")
  
  if(!is.null(inputDataList) && length(setdiff(obs_fields, names(inputDataList$phe_statusintensity))) > 0) {
    
    stop(paste0("Required columns missing from inputDataList$phe_statusintensity:", 
                paste0(setdiff(obs_fields, names(inputDataList$phe_statusintensity)), 
                       collapse = ", "),
                sep = " "))
  }
  
  if(!is.null(inputDataList) && length(setdiff(tag_fields, names(inputDataList$phe_perindividual))) > 0) {
    
    stop(paste0("Required columns missing from inputDataList$phe_perindividual:", 
                paste0(setdiff(tag_fields, names(inputDataList$phe_perindividual)), 
                       collapse = ", "),
                sep = " "))
  }  
  
  # Verify inputStatus has expected columns
  if (!is.null(inputStatus) && length(setdiff(obs_fields, names(inputStatus))) > 0) {
    
    stop(paste0("Required columns missing from inputStatus:", 
                paste0(setdiff(obs_fields, names(inputStatus)), 
                       collapse = ", "),
                sep = " "))
  }
  
  # Verify inputTags has expected columns
  if(!is.null(inputTags) && length(setdiff(tag_fields, names(inputTags))) > 0){
    stop(paste0("Required columns missing from inputStatus:", 
                paste0(setdiff(tag_fields, names(inputTags)), 
                       collapse = ", "),
                sep = " "))
  }
  
  # Verify data are present in tables and dfs  
  if(!is.null(inputDataList) && nrow(inputDataList$phe_statusintensity) < 1) {
    stop("inputDataList$phe_statusintensity data frame does not contain data.")
  }
  
  if(!is.null(inputDataList) && nrow(inputDataList$phe_perindividual) < 1) {
    stop("inputDataList$phe_perindividual data frame does not contain data.")
  }
  
  if(!is.null(inputTags) && nrow(inputTags) < 1) {
    stop("inputTags data frame does not contain data.")
  }
  
  if(!is.null(inputStatus) && nrow(inputStatus) < 1) {
    stop("inputStatus data frame does not contain data.")
  }
  
  # Assign working data frame
  if(exists("inputDataList") ==TRUE & is.list(inputDataList)) {
    
    obs <- inputDataList$phe_statusintensity
    tags <- inputDataList$phe_perindividual
    
  } else {
    obs <- inputStatus
    tags <- inputTags
  }

  #check for duplicate tags
  if(any(duplicated(tags$individualID))) {
    stop(paste("duplicate records present for", unique(tags$individualID[duplicated(tags$individualID)]),
               "please resolve before running estimatePheTrans"))
  }

  # Print data range for input data set
  print(paste("Observation date range:", min(obs$date), "to", max(obs$date)))


  # Format transition output dataframe
  step_one <- obs %>%
    
    # extract year from date
    dplyr::mutate(year = substr(.data$date, 1,4)) %>%
    dplyr::group_by(.data$individualID, 
                    .data$phenophaseName) %>%
    
    # remove uninformative phenophaseStatuses
    dplyr::filter(.data$phenophaseStatus != "uncertain") %>%
    
    # get status, doy previous observation, create transition type for current obs
    dplyr::mutate(statusLag = dplyr::lag(.data$phenophaseStatus),
                  dateIntervalStart = dplyr::lag(date), 
                  doyIntervalStart = dplyr::lag(.data$dayOfYear),
                  transitionType = paste0(.data$statusLag, "-", .data$phenophaseStatus))
  
  # verify input data set contains transitions
  if(any(step_one$transitionType %in% c('no-yes', 'yes-no')) == FALSE) {
    stop("Input dataset does not contain any phenophase transitions")
  }
  
  
  ##  Remove first observation with no preceding observation & steps with no transition  
  step_two <- step_one %>%  
    dplyr::filter(!is.na(.data$statusLag), 
                  .data$phenophaseStatus != .data$statusLag) %>%
    
    # Calculate values for each time step
    dplyr::mutate(dateTransition = as.Date(.data$dateIntervalStart + (.data$date - .data$dateIntervalStart) / 2),
                  doyTransition = lubridate::yday(.data$dateTransition),
                  precisionDays = as.numeric(difftime(.data$date, .data$dateIntervalStart, units = "days"))/2,
                  samplingInterval = as.numeric(difftime(.data$date, .data$dateIntervalStart, units = "days"))) %>%  
    
    dplyr::group_by(.data$year,
                    .data$individualID, 
                    .data$phenophaseName) %>%  
    
    # Count number of onsets (per year, individual, phenophase)
    dplyr::mutate(nthTransition = cumsum(.data$statusLag == "no" & .data$phenophaseStatus == "yes")) %>%
    
    # Clean up outputs
    dplyr::select("year", 
                  "siteID", 
                  "individualID", 
                  "phenophaseName", 
                  "transitionType",
                  "nthTransition", 
                  "date", 
                  "dateIntervalStart", 
                  "dayOfYear",
                  "doyIntervalStart", 
                  "samplingInterval", 
                  "dateTransition", 
                  "doyTransition", 
                  "precisionDays") %>%
    
    dplyr::arrange(.data$year, 
                   .data$phenophaseName, 
                   .data$individualID)
  
  
  # Rename transition type to onset/offset
  step_two$transitionType <- ifelse(step_two$transitionType == 'no-yes',
                                    'onset',
                                    ifelse(step_two$transitionType == 'yes-no', 
                                           'end',
                                           step_two$transitionType))
  
  # Prep tags df
  out <- tags %>%
    dplyr::select("individualID", 
                  "taxonID",
                  "scientificName",
                  "growthForm") %>%
    
    # Join with Obs
    dplyr::right_join(step_two, 
                      by = "individualID") %>%
    
    # Reorder fields
    dplyr::select("year", 
                  "siteID", 
                  "individualID", 
                  "taxonID", 
                  "scientificName", 
                  "phenophaseName", 
                  "transitionType", 
                  "nthTransition", 
                  "dateIntervalStart", 
                  "doyIntervalStart", 
                  "dateIntervalEnd" = "date",
                  "doyIntervalEnd" = "dayOfYear",
                  "dateTransition", 
                  "doyTransition", 
                  "samplingInterval",
                  "precisionDays")
  
  return(out)
}
