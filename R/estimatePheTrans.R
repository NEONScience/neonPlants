#' @title Estimate Phenological Transition Dates
#'
#' @author
#' Katie Jones \email{kjones@battelleecology.org} \cr
#'
#' @description This function uses observation data from the NEON Plant Phenology Observation (DP1.10055.001) to calculate phenophase transition dates and descriptive statistics.
#'
#'
#' @param pheData data list from NEON Plant Phenology Observation (DP1.10055.001) as returned from neonUtilities::loadByProduct().
#
#' @details
#' This function creates a time series for each phenophase reported for each individual in the data set and identifies transition dates for beginning and end of a given phenophase as well as explanatory metrics about that estimate for the time frame provided in the input data frame.
#' Calculated values inclued:
#'  * transition date - the mid-point between two consecutive dates with different phenophase status values
#'  * transition_type - indicating the phenophase status values of the transition
#'  * sampling interval - the number of days between observations
#'  * uncertainty - sampling interval/2
#'  * nth tranition - a count of onset events per individualID, phenophase name, within a given calendar year
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
#'out <- estimatePheTrans(inputDataList = pheDat)
#'out2 <- estimatePheTrans(inputStatus = pheDat$phe_statusintensity)
#' }

##############################################################################################


estimatePheTrans <- function(
    inputDataList = NULL,
    inputStatus = NULL,

    ){

  # Verify that only one input is provided
  if(!is.null(inputDataList) && !is.null(inputStatus)){
    stop("Please provide either a list of data frames (inputDataList) or a single data frame (inputStatus), but not both.")
  }

  if(!is.null(inputDataList) && !"phe_statusintensity"%in%ls(inputDataList)){
    stop("'phe_statusintensity' table missing from inputDataList")
  }

  if(!is.null(inputDataList) && !"phe_perindividual"%in%ls(inputDataList)){
    stop("'phe_perindividual' table missing from inputDataList")
  }

  # Assign working data frame
  if(is.list(inputDataList)){
  df <- inputDataList$phe_statusintensity
  }else{
    df <- inputStatus
  }


  # Print data range for input data set
  print(paste("Observation date range:", min(df$date), "to", max(df$date)))

  # Format output dataframe
  step_one <-df%>%
    # extract year from date
    dplyr::mutate(year = substr(date, 1,4))%>%
    dplyr::group_by(individualID, phenophaseName) %>%
    # remove uninformative phenophaseStatuses
    dplyr::filter(phenophaseStatus!="uncertain") %>%
    # get status, doy previous observation, create transition type for current obs
    dplyr::mutate(status_lag = dplyr::lag(phenophaseStatus),
           date_lag = dplyr::lag(date), doy_lag = dplyr::lag(dayOfYear),
           transitionType = paste0(status_lag, "-", phenophaseStatus))

  # verify input data set contains transitions
  if(any(step_one$transitionType%in%c('no-yes', 'yes-no'))==FALSE){
    stop("input dataset does not contain any phenophase transitions")
  }

  step_two <- step_one%>%  # remove first observation with no preceding observation & steps with no transition
    dplyr::filter(!is.na(status_lag), phenophaseStatus != status_lag) %>%
    #calculate values for each time step
    dplyr::mutate(date_transition = as.Date(date_lag + (date - date_lag) / 2),
           doy_transition = lubridate::yday(date_transition),
           precision_days = as.numeric(difftime(date, date_lag, units = "days"))/2,
           samplingInterval = as.numeric(difftime(date, date_lag, units = "days"))) %>%
    dplyr::group_by(year,individualID, phenophaseName)%>%
    #count number of onsets (per year, individual, phenophase)
    dplyr::mutate(nth_transition = cumsum(status_lag == "no" & phenophaseStatus == "yes"))%>%
    #clean up outputs
    dplyr::select(year, siteID, individualID, phenophaseName, transitionType, nth_transition, date, date_lag,
           doy_lag, samplingInterval, date_transition, doy_transition, precision_days)%>%
    dplyr::arrange(year, phenophaseName, individualID)

  return(step_two)


}


