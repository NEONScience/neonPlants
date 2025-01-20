#' @title Estimate Phenological Transition Dates by Tag
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
#' Calculated values include:
#'  * transition date - the mid-point between two consecutive dates with different phenophase status values
#'  * transitionType - indicating the phenophase status values of the transition
#'  * sampling interval - the number of days between observations
#'  * uncertainty - sampling interval/2
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
#'out <- estimatePheTrans(inputDataList = pheDat)
#'out2 <- estimatePheTrans(inputStatus = pheDat$phe_statusintensity)
#' }

##############################################################################################


estimatePheTransByTag <- function(
    inputDataList = NULL,
    inputStatus = NULL,
    inputTags = NULL
    ){
  
  require(dplyr)

  # Verify that only one input type is provided
  if(!is.null(inputDataList) && !is.null(inputStatus) |!is.null(inputDataList) && !is.null(inputTags)){
    stop("Please provide either a list of data frames, inputDataList, OR individual data frames for inputStatus and inputTags, but not both.")
  }
  
  # Verify that if inputStatus is provided, inputTags is also provided
  if(!is.null(inputStatus) && is.null(inputTags)){
    stop("inputStatus provided without inputTags, both data frames are required.")
  }
  
  # Verify that if inputStatus is provided, inputTags is also provided
  if(!is.null(inputTags) && is.null(inputStatus)){
    stop("inputTags provided without inputStatus, both data frames are required.")
  }
  
  # Verify that list is provided if inputDataList is non-null
  if(!is.null(inputDataList) && !class(inputDataList)=='list'){
    stop(paste("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct(). 
               Supplied input object is class", {class(inputDataList)}))
  }
  
  # Verify that list contains expected data frames
  req_tables <- c("phe_statusintensity", "phe_perindividual")
  
  if(!is.null(inputDataList) && length(setdiff(req_tables, names(inputDataList))) > 0){
    stop(paste0("Required data frames missing from inputDataList: ",
                paste0(setdiff(req_tables, names(inputDataList)),
                collapse = ", "),
                sep = " "))
  }
  
  # Verify that df is provided if inputStatus is non-null
  if(!is.null(inputStatus) && !class(inputStatus)=='data.frame'){
    stop(paste("Argument 'inputStatus' must be a data frame object from neonUtilities::loadByProduct(). 
               Supplied input object is class", {class(inputStatus)}))
  }
  
  # Verify that df is provided if inputTags is non-null
  if(!is.null(inputTags) && !class(inputTags)=='data.frame'){
    stop(paste("Argument 'inputTags' must be a data frame object from neonUtilities::loadByProduct(). 
               Supplied input object is class", {class(inputTags)}))
  }
  
  # Verify expected tables are present in inputDataList 
  if(!is.null(inputDataList) && !"phe_statusintensity"%in%ls(inputDataList)){
    stop("'phe_statusintensity' tables missing from inputDataList")
  }
  if(!is.null(inputDataList) && !"phe_perindividual"%in%ls(inputDataList)){
    stop("'phe_perindividual' tables missing from inputDataList")
  }  

  # Verify inputDataList tables have expected columns
    obs_fields <- c("date", "individualID", "phenophaseName", "phenophaseStatus")
    
    tag_fields <- c("individualID", "taxonID", "scientificName", "growthForm")
   
    if(!is.null(inputDataList) && length(setdiff(obs_fields, 
                                                 names(inputDataList$phe_statusintensity))) > 0){
      stop(paste0("Required columns missing from inputDataList$phe_statusintensity:", 
                  paste0(setdiff(tag_fields, names(inputTags)), 
                         collapse = ", "),
                  sep = " "))
    }
     
    if(!is.null(inputDataList) && length(setdiff(tag_fields, names(inputDataList$phe_perindividual))) > 0){
      stop(paste0("Required columns missing from inputDataList$phe_perindividual:", 
                  paste0(setdiff(tag_fields, names(inputTags)), 
                         collapse = ", "),
                  sep = " "))
    }  
  
  # Verify inputStatus has expected columns
  if (!is.null(inputStatus) && length(setdiff(obs_fields, names(inputStatus))) > 0){
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
    if(!is.null(inputDataList) && nrow(inputDataList$phe_statusintensity)< 1){
      stop("inputDataList$phe_statusintensity data frame does not contain data.")
    }
    if(!is.null(inputDataList) && nrow(inputDataList$phe_perindividual)< 1){
      stop("inputDataList$phe_perindividual data frame does not contain data.")
    }
    if(!is.null(inputTags) && nrow(inputTags)< 1){
      stop("inputTags data frame does not contain data.")
    }
    if(!is.null(inputStatus) && nrow(inputStatus)< 1){
      stop("inputTags data frame does not contain data.")
    }

  # Assign working data frame
  if(exists("inputDataList")==TRUE & is.list(inputDataList)){
  obs <- inputDataList$phe_statusintensity
  tags <- inputDataList$phe_perindividual
  }else{
    obs <- inputStatus
    tags <- inputTags
  }

  #check for duplicate tags
  if(any(duplicated(tags$individualID))){
    stop(paste("duplicate records present for", unique(tags$individualID[duplicated(tags$individualID)]),
               "please resolve before running estimatePheTrans"))
  }

  # Print data range for input data set
  print(paste("Observation date range:", min(obs$date), "to", max(obs$date)))

  # Define the categorical values
  intensities <- unique(obs$phenophaseIntensity)
  
  # Define the order of the categories 
  order_int_levels <- c(NA, "< 5%", "< 25%", "5-24%", "25-49%", "50-74%", "75-94%", ">= 95%", 
                        "<= 3", "3 to 10", "11 to 100", "101 to 1000", "1001 to 10000", "> 10000")
  
  # Create an ordered factor 
  ordered_intensities <- factor(intensities, levels = order_int_levels, ordered = TRUE)
  
  # Format transition output dataframe
  step_one <-obs%>%
    # extract year from date
    dplyr::mutate(year = substr(.data$date, 1,4))%>%
    dplyr::group_by(.data$individualID, 
                    .data$phenophaseName) %>%
    # remove uninformative phenophaseStatuses
    dplyr::filter(.data$phenophaseStatus!="uncertain") %>%
    # get status, doy previous observation, create transition type for current obs
    dplyr::mutate(statusLag = dplyr::lag(.data$phenophaseStatus),
                  dateIntervalStart = dplyr::lag(date), doyIntervalStart = dplyr::lag(.data$dayOfYear),
           transitionType = paste0(.data$statusLag, "-", .data$phenophaseStatus))

  # verify input data set contains transitions
  if(any(step_one$transitionType%in%c('no-yes', 'yes-no'))==FALSE){
    stop("input dataset does not contain any phenophase transitions")
  }

    dplyr::mutate(maxIntensity = .data$phenophaseIntensity[which.max(as.numeric(ordered_intensities))])%>%
    
  step_two <- step_one%>%  # remove first observation with no preceding observation & steps with no transition
    dplyr::filter(!is.na(.data$statusLag), .data$phenophaseStatus != .data$statusLag) %>%
    #calculate values for each time step
    dplyr::mutate(dateTransition = as.Date(.data$dateIntervalStart + (.data$date - .data$dateIntervalStart) / 2),
                  doyTransition = lubridate::yday(.data$dateTransition),
                  precisionDays = as.numeric(difftime(.data$date, .data$dateIntervalStart, units = "days"))/2,
                  samplingInterval = as.numeric(difftime(.data$date, .data$dateIntervalStart, units = "days"))) %>%  
    dplyr::group_by(.data$year,
                    .data$individualID, 
                    .data$phenophaseName)%>%  
    #count number of onsets (per year, individual, phenophase)
    dplyr::mutate(nthTransition = cumsum(.data$statusLag == "no" & .data$phenophaseStatus == "yes"))%>%
    #clean up outputs
    dplyr::select("year", "siteID", "individualID", "phenophaseName", "transitionType",
                  "nthTransition", "date", "dateIntervalStart", "dayOfYear",
                  "doyIntervalStart", "samplingInterval", "dateTransition", "doyTransition", 
                  "precisionDays")%>%
    dplyr::arrange(.data$year, .data$phenophaseName, .data$individualID)

    
# rename transition type to onset/offset
  step_two$transitionType <- ifelse(step_two$transitionType=='no-yes', 'onset',
                                    ifelse(step_two$transitionType=='yes-no', 'end',
                                    step_two$transitionType))

# prep tags df
  out <- tags%>%
    dplyr::select("individualID", "taxonID", "scientificName", "growthForm")%>%
# Join with Obs
    dplyr::right_join(., step_two)%>%
# reorder fields
    dplyr::select("year", "siteID", "individualID", "taxonID", "scientificName", 
                  "phenophaseName", "transitionType", "nthTransition", 
                  "dateIntervalStart", "doyIntervalStart", "dateIntervalEnd"="date",
                  "doy_intervalEnd" = "dayOfYear", "dateTransition", 
                  "doyTransition", "samplingInterval", "precisionDays")
  return(out)
}


