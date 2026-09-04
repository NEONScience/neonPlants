#' @title Find NEON Vegetation Structure Sampling Bouts
#'
#' @author Claire K Lunch \email{clunch@battelleecology.org} \cr
#'
#' @description Vegetation structure (DP1.10098.001) data are collected annually at NEON terrestrial sites with qualifying vegetation, but different plots are sampled in different years. This function returns a table of which types of sampling events occurred in each year.
#'
#' @details Vegetation structure event types are:
#'    * towerSubset: The subset of plots (n=5) in the Tower airshed that are sampled annually.
#'    * allTowerPlots: All plots in the Tower airshed, including the annual subset. Tower plots not in the annual subset are sampled every 5-6 years, and always in a different year from the Distributed plots.
#'    * distributedAndTowerSubset: The annual plots in the Tower airshed plus the Distributed plots. Distributed plots are sampled every 5-6 years.
#'
#'  Trees are typically measured outside of the growing season. It is not unusual for sampling bouts to begin in one year and end in the next, so check the start and end date when choosing the date range of data for your analysis.
#'
#' @param site 4-letter code for a single NEON site. [character]
#' @param includePlots Include plotIDs for the plots sampled in each event? Defaults to FALSE. [logical]
#' @param token NEON API token [character]
#'
#' @return A table listing the sampling event type for each year and eventID, and the start and end date of each sampling event. If includePlots = TRUE, the table also includes the list of plots sampled in each event. Start and end date are the start and end date of the entire event, not specific to a given plot. For the most recent year of data, it is possible not all data collection has been completed; in this case the end date reflects the end date of data in the database, but publication of more data may be pending.
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @examples
#' \dontrun{
#'
#' rmnpevents <- getVegStructureEvents(site="RMNP", token=token)
#'
#' }
#'
#' @export getVegStructureEvents


getVegStructureEvents <- function(site = NA_character_,
                                  includePlots = FALSE,
                                  token = NA_character_) {

  if(is.na(site)) {
    stop("Site is a required input for this function.")
  }

  vegpppy <- suppressMessages(neonUtilities::loadByProduct(dpID="DP1.10098.001", site=site,
                                          tabl="vst_perplotperyear",
                                          include.provisional=TRUE,
                                          check.size=FALSE, token=token,
                                          progress=FALSE))

  pppy <- vegpppy$vst_perplotperyear

  if(isFALSE(includePlots)) {
    vevents <- unique(pppy[which(pppy$samplingImpractical=="OK" | is.na(pppy$samplingImpractical)),
                           c("eventID","eventType")])
  } else {
    vevents <- unique(pppy[which(pppy$samplingImpractical=="OK" | is.na(pppy$samplingImpractical)),
                           c("eventID","eventType","plotID")])
  }

  # get year from eventID
  vevents$eventYear <- base::regmatches(vevents$eventID,
                                        base::regexpr("20[0-9]{2}", vevents$eventID))

  # get date range for apparent individual table
  ds <- suppressMessages(neonUtilities::datasetQuery(dpID="DP1.10098.001", site=site,
                                    tabl="vst_apparentindividual",
                                    include.provisional=TRUE,
                                    token=token))

  evdategp <- dplyr::group_by(.data=ds, .data$eventID)
  evdatesum <- dplyr::summarise(.data=evdategp,
                                startDate=min(.data$date),
                                endDate=max(.data$date))
  evdates <- dplyr::collect(evdatesum)

  # check for non-woody and shrub group tables. if present, get date range
  dshrub <- try(suppressMessages(neonUtilities::datasetQuery(dpID="DP1.10098.001",
                                                             site=site,
                                    tabl="vst_shrubgroup",
                                    include.provisional=TRUE,
                                    token=token)), silent=TRUE)

  if(inherits(dshrub, "try-error") | is.null(dshrub)) {
    shrubdates <- NULL
  } else {
    shrubdategp <- dplyr::group_by(.data=dshrub, .data$eventID)
    shrubdatesum <- dplyr::summarise(.data=shrubdategp,
                                  startDate=min(.data$date),
                                  endDate=max(.data$date))
    shrubdates <- dplyr::collect(shrubdatesum)
  }

  dnw <- try(suppressMessages(neonUtilities::datasetQuery(dpID="DP1.10098.001",
                                                          site=site,
                                                          tabl="vst_non-woody",
                                                          include.provisional=TRUE,
                                                          token=token)), silent=TRUE)

  if(inherits(dnw, "try-error") | is.null(dnw)) {
    nwdates <- NULL
  } else {
    nwdategp <- dplyr::group_by(.data=dnw, .data$eventID)
    nwdatesum <- dplyr::summarise(.data=nwdategp,
                                     startDate=min(.data$date),
                                     endDate=max(.data$date))
    nwdates <- dplyr::collect(nwdatesum)
  }

  # if shrub or non-woody tables are present, calculate overall event date range
  if(all(is.null(shrubdates), is.null(nwdates))) {
    alldates <- evdates
  } else {
    events <- unique(c(evdates$eventID, shrubdates$eventID, nwdates$eventID))
    alldates <- data.frame(eventID=sort(events),
                           startDate=as.Date(x=integer(length(events)), origin="2012-01-01"),
                           endDate=as.Date(x=integer(length(events)), origin="2012-01-01"))
    for(i in 1:nrow(alldates)) {
      evi <- alldates$eventID[i]
      datesi <- c(evdates$startDate[which(evdates$eventID==evi)],
                  evdates$endDate[which(evdates$eventID==evi)],
                  shrubdates$startDate[which(shrubdates$eventID==evi)],
                  shrubdates$endDate[which(shrubdates$eventID==evi)],
                  nwdates$startDate[which(nwdates$eventID==evi)],
                  nwdates$endDate[which(nwdates$eventID==evi)])
      alldates$startDate[i] <- min(datesi, na.rm=TRUE)
      alldates$endDate[i] <- max(datesi, na.rm=TRUE)
    }
    if(any(alldates$startDate==as.Date("2012-01-01"))) {
      alldates$startDate[which(alldates$startDate==as.Date("2012-01-01"))] <- NA
    }
    if(any(alldates$endDate==as.Date("2012-01-01"))) {
      alldates$endDate[which(alldates$endDate==as.Date("2012-01-01"))] <- NA
    }
  }

  # join date range to event types
  vevents <- merge(vevents, alldates, by="eventID", all=TRUE)

  return(vevents)

}
