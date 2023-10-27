#' @title Stack NEON plant diversity data
#'
#' @author
#' Dave T Barnett \email{dbarnettl@battelleecology.org} \cr
#'
#' @description Use this function to aggregate NEON plant presence data to reflect plant species present at each plot scale.
#'
#' @import httr XML dplyr
#'
#' @param div_1m2Data div_1m2Data table in NEON data produdct (Plant presence and percent cover, DP1.10058.001)
#' @param div_10m2Data100m2Data div_10m2Data100m2Data table in NEON data produdct (Plant presence and percent cover, DP1.10058.001)
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
#' # load neonUtilities
#' library (neonUtilities)
#'
#' # get data
#' allDiv <- loadByProduct(
#'   dpID = "DP1.10058.001",
#'   site = "SRER",
#'   startdate = "2010-01",
#'   package = "basic",
#'   check.size = FALSE)
#'
#' ##extract 1m2 data from list of lists and some processing
#' data_1m2 <- allDiv[["div_1m2Data"]]
#' ###get 10_100
#' data_10_100m2 <- allDiv[["div_10m2Data100m2Data"]]
#'
#' data_stacked <- stackPlantDiv(
#'   div_1m2Data = data_1m2,
#'   div_10m2Data100m2Data = data_10_100m2)
#' }


# changelog and author contributions / copyrights
#   Dave T Barnett (2022-08-30)
#     original creation
#   Eric R Sokol (2023-03-08)
#     add to neonPlants package
##############################################################################################

stackPlantDiv <- function(
    div_1m2Data = NA,
    div_10m2Data100m2Data = NA){

  # library(dplyr)
  # library(stringr)

  ###deal with eventID and year - 1m2 data
  #create year column
  div_1m2Data$year <- substr(div_1m2Data$endDate, start = 1, stop = 4)
  div_1m2Data$eventID <- ifelse(
    is.na(div_1m2Data$eventID) | stringr::str_length(div_1m2Data$eventID) > 11,
    paste(div_1m2Data$siteID, div_1m2Data$boutNumber, div_1m2Data$year, sep="."),
    div_1m2Data$eventID)
  div_1m2Data <- div_1m2Data %>% dplyr::select(-year)

  ###deal with eventID and year - larger subplot data table
  #create year column
  div_10m2Data100m2Data$year <- substr(div_10m2Data100m2Data$endDate, start = 1, stop = 4)
  div_10m2Data100m2Data$eventID <- ifelse(
    is.na(div_10m2Data100m2Data$eventID) | stringr::str_length(div_10m2Data100m2Data$eventID) > 11,
    paste(div_10m2Data100m2Data$siteID,
          div_10m2Data100m2Data$boutNumber,
          div_10m2Data100m2Data$year, sep="."),
    div_10m2Data100m2Data$eventID)
  div_10m2Data100m2Data <- div_10m2Data100m2Data %>% dplyr::select(-year)

  ###limit 1m2 data to plant species records
  div_1m2Data <- dplyr::filter(div_1m2Data, divDataType == "plantSpecies")

  # columns to keep
  cols2keep <- c("namedLocation", "domainID",	"siteID",
                 "decimalLatitude",	"decimalLongitude",	"geodeticDatum",
                 "coordinateUncertainty",	"elevation",
                 "elevationUncertainty", "nlcdClass", "eventID",
                 "plotType", "plotID",	"subplotID",	"boutNumber",
                 "targetTaxaPresent",	"taxonID",	"scientificName",
                 "taxonRank",	"family",	"nativeStatusCode",
                 "identificationQualifier",	"morphospeciesID",
                 "samplingImpractical", "samplingImpracticalRemarks",
                 "biophysicalCriteria", "publicationDate", "release")

  ###get rid of extra fields that could be hard for generating unique records
  #div_1m2Data
  div_1m2Data <- div_1m2Data %>% dplyr::select(
    tidyr::any_of(c("div_1m2Data",cols2keep)))

  #data 10_100
  div_10m2Data100m2Data <- div_10m2Data100m2Data %>% dplyr::select(
    tidyr::any_of(c("div_10m2Data100m2Data",cols2keep)))


  ##############identify those years where sample 1m2 subplots only and those
  # years where sample both (alternating years)##############
  # don't want to process 1m2 only year with larger subplot data that don't exist

  # 1m2 data data frame of eventIDs
  smallEventID <- dplyr::select(div_1m2Data, eventID) %>% unique()

  # 10_100m2 data data frame of eventIDs
  bigEventID <- dplyr::select(div_10m2Data100m2Data, eventID) %>% unique()

  # make df of those eventIDs sampled the 1m2 and not the larger subplots
  smallOut <- dplyr::anti_join(smallEventID, bigEventID)

  # pull out the corresponding data into unique data frame that will not be
  # incorporated in the larger scale data
  div_1m2DataOut <- div_1m2Data %>%
    dplyr::filter(eventID %in% smallOut$eventID)

  #make df of eventID common to both
  smallMerge <- dplyr::inner_join(smallEventID, bigEventID)
  smallMergeEventID <- smallMerge$eventID

  #subset to data that corresponds to evenID in both the small and large data
  div_1m2Data <- div_1m2Data %>%
    dplyr::filter(eventID %in% smallMergeEventID)

  ###############set up data aggregation across scales, will need to update
  # to new naming convention##############

  ###preparation

  #make sure subplotID is character (seems like it comes down that way now)
  div_10m2Data100m2Data$subplotID <- as.character(div_10m2Data100m2Data$subplotID)

  #separate the 10m2 data from the 100m2 data:
  data_100m2 = div_10m2Data100m2Data[which(nchar(div_10m2Data100m2Data$subplotID)<3), ]
  data_10m2 = div_10m2Data100m2Data[which(nchar(div_10m2Data100m2Data$subplotID)>2), ]

  ###build 10m2 data by aggregating observations from 1m2
  # (years sampled both 1 and 10_100) and 10m2
  #rename 1m2 to combine with 10
  data_10m2Build <- div_1m2Data

  #rename 1m2 subplots so associated observations will combine with 10m2 observations
  data_10m2Build$subplotID[data_10m2Build$subplotID == "31.1.1"] <- "31.1.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "31.4.1"] <- "31.4.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "32.2.1"] <- "32.2.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "32.4.1"] <- "32.4.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "40.1.1"] <- "40.1.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "40.3.1"] <- "40.3.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "41.1.1"] <- "41.1.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "41.4.1"] <- "41.4.10"

  #combine what was the nested 1m2 subplot data with the 10m2 data to get the
  # complete list of species in each 10m2 subplot
  data_10m2 <- rbind(data_10m2, data_10m2Build)

  ###aggregate 100m2 data by combining 10m2 observations with 100m2 observations
  #rename 10m2 to combine with 100m2
  data_100m2Build <- data_10m2

  # rename 10m2 subplots so associated observations will combine with 100m2 observations
  data_100m2Build$subplotID[data_100m2Build$subplotID == "31.1.10"] <- 31
  data_100m2Build$subplotID[data_100m2Build$subplotID == "31.4.10"] <- 31
  data_100m2Build$subplotID[data_100m2Build$subplotID == "32.2.10"] <- 32
  data_100m2Build$subplotID[data_100m2Build$subplotID == "32.4.10"] <- 32
  data_100m2Build$subplotID[data_100m2Build$subplotID == "40.1.10"] <- 40
  data_100m2Build$subplotID[data_100m2Build$subplotID == "40.3.10"] <- 40
  data_100m2Build$subplotID[data_100m2Build$subplotID == "41.1.10"] <- 41
  data_100m2Build$subplotID[data_100m2Build$subplotID == "41.4.10"] <- 41

  # combine what was the nested 10m2 subplot data with the 100m2 data to get
  # the complete list of species in each 100m2 subplot
  data_100m2 <- rbind(data_100m2, data_100m2Build)

  #################recombine the 1m2 data#####################
  div_1m2Data <- rbind(div_1m2Data, div_1m2DataOut)

  #################remove duplicates and combine to one data frame#####################
  ###make sure unique at each scale after combinations Need to figure out how to do unique on specific columns or get rid Different people might have measured the 1 and 10m subplots which could result in otherwise duplicate entries, for example. Maybe have to get rid of the stuff like date above?
  div_1m2Data <- unique(div_1m2Data)
  data_10m2 <- unique(data_10m2)
  data_100m2 <- unique(data_100m2)

  ###create the 400m2 plot species lists
  data_400m2 <- data_100m2
  data_400m2$subplotID <- "400"
  data_400m2 <- unique(data_400m2)

  ###this chunk combines the data from all scales of measurement into one table

  #data10_100_400 <- rbind(data_10m2, data_100m2)
  data <- rbind(div_1m2Data, data_10m2, data_100m2, data_400m2)

  #remove duplicates
  divPlantPresenceData <- unique(data)

  #don't pass target taxa present to larger-scale subplots if not true
  divplantPresenceData <- divPlantPresenceData %>%
    dplyr::group_by(eventID, plotID, subplotID) %>%
    dplyr::mutate(tot = n()) %>%
    dplyr::filter(
      (tot > 1 & targetTaxaPresent != "N") |
        (tot <= 1 & targetTaxaPresent == targetTaxaPresent)) %>%
    dplyr::select(-tot) %>%
    dplyr::ungroup()

  return(divplantPresenceData)

}





