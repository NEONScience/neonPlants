#' @title Reformat old subplotIDs
#'
#' @author
#' Eric Sokol \email{esokol@battelleecology.org} \cr
#'
#' @description Helper function to reformat old subplotIDs
#'
#' @param x old subplotID (text string)
#'
#' @return This function returns a data frame
reformatSubplotID <- function(x){
  x_split <- strsplit(x,split = "\\.") %>% unlist()
  if((length(x_split)==3) & x_split[3]%in%c(1,10,100,400)){
    return(
      paste(x_split[c(1,3,2)], collapse = "_"))
  }else{return(x)}
}



#' @title Stack NEON plant occurrence data
#'
#' @author
#' Dave T Barnett \email{dbarnettl@battelleecology.org} \cr
#' Eric Sokol \email{esokol@battelleecology.org} \cr

#' @description Use this function to aggregate the occurrence data from the NEON Plant presence and percent cover (DP1.10058.001) data product to list the plant species present at each plot scale.
#'
#'
#' @param divDataList A list of data.frames from the NEON Plant presence and percent cover (DP1.10058.001) data product as returned from neonUtilities::loadByProduct(). This list must include data.frames with the names 'div_1m2Data' and 'div_10m2Data100m2Data'.
#' @param totalSampledAreaFilter (integer, options are NA, 1, 10, 100, 400) The plot size for which data are returned. Default (NA) will return data for all plot sizes in the dataset. If you select a plot size, the function will filter the data returned to the desired plot size.
#'
#' @details
#' This function properly stacks occurrence records from the NEON Plant presence and percent cover, (DP1.10058.001) data product. Provide a list that includes data.frames named 'div_1m2Data' and 'div_10m2Data100m2Data' and this function will properly stack the occurrence data for each plot scale. If you only want to return a species list for one plot scale, use the totalSampledAreaFilter parameter to select the scale (1, 10, 100, or 400m). If totalSampledAreaFilter is NA (default), then the function will return a data.frame with the occurrence records for all plot scales, and you will need to filter the output to get species lists for each plot scale.
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
#'
#' # get data
#' allDiv <- loadByProduct(
#'   dpID = "DP1.10058.001",
#'   site = "SRER",
#'   startdate = "2010-01",
#'   package = "basic",
#'   check.size = FALSE)
#'
#'
#' # stack the data by sending the list returned by neonUtilities::loadByProduct
#' data_stacked <- stackPlantPresence(
#'   divDataList = allDiv)
#'
#'
#' # send list of data using pipe
#' data_stacked <- allDiv %>%
#'   stackPlantPresence()
#'
#'
#' # filter to 10m plots
#' data_stacked_10 <- allDiv %>%
#'   stackPlantPresence(totalSampledAreaFilter = 10)
#'
#'
#' # filter to 10m plots outside of the stackPlantPresence function
#' data_stacked_10 <- allDiv %>%
#'   stackPlantPresence() %>%
#'   filter(totalSampledArea == 10)
#'
#'
#' # make your own list and stack the data
#' my_1m_data <- allDiv$div_1m2Data
#' my_10_100m_data <- allDiv$div_10m2Data100m2Data
#'
#' data_stacked <- stackPlantPresence(
#'   divDataList = list(
#'     div_1m2Data = my_1m_data,
#'     div_10m2Data100m2Data = my_10_100m_data))
#'
#'
#' # filter tables to a single plot, then stack the data
#' my_1m_data_SRER_43 <- my_1m_data %>%
#'   filter(namedLocation == "SRER_043.basePlot.div")
#'
#' my_10_100m_data_SRER_43 <- my_10_100m_data %>%
#'   filter(namedLocation == "SRER_043.basePlot.div")
#'
#' data_stacked_SRER_43 <- stackPlantPresence(
#'   divDataList = list(
#'     div_1m2Data = my_1m_data_SRER_43,
#'     div_10m2Data100m2Data = my_10_100m_data_SRER_43))
#' }


# changelog and author contributions / copyrights
#   Dave T Barnett (2022-08-30)
#     original creation
#   Eric R Sokol (2023-03-08)
#     add to neonPlants package
##############################################################################################

stackPlantPresence <- function(
    divDataList = NA,
    totalSampledAreaFilter = NA_integer_){

  # error handling
  # check if divDataList is a list
  if(methods::is(divDataList,"list")){

    # check that the div 1m and 10_100m data.frames are in the list
    if(length(
      dplyr::setdiff(
        c("div_1m2Data","div_10m2Data100m2Data"),
        names(divDataList))) == 0){

      # extract data.frames from divDataList list
      div_1m2Data <- divDataList$div_1m2Data
      div_10m2Data100m2Data <- divDataList$div_10m2Data100m2Data

    }else{
      stop("please provide a list containing data.frames named 'div_1m2Data' and 'div_10m2Data100m2Data'")
    }
  }

  # reformat subplotID
  div_10m2Data100m2Data <- div_10m2Data100m2Data %>%
    dplyr::mutate(
      subplotID_old = subplotID,
      subplotID = dplyr::case_when(
        nchar(subplotID) == 2 ~
          paste0(subplotID,"_100"),
        grepl("\\.", subplotID) ~
          sapply(subplotID, reformatSubplotID, USE.NAMES = FALSE),
        .default = subplotID
      ))

  div_1m2Data <- div_1m2Data %>%
    dplyr::mutate(
      subplotID_old = subplotID,
      subplotID = dplyr::case_when(
        nchar(subplotID) == 2 ~
          paste0(subplotID,"_100"),
        grepl("\\.", subplotID) ~
          sapply(subplotID, reformatSubplotID, USE.NAMES = FALSE),
        .default = subplotID
      ))



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

  # #make sure subplotID is character (seems like it comes down that way now)
  # div_10m2Data100m2Data$subplotID <- as.character(div_10m2Data100m2Data$subplotID)



  #separate the 10m2 data from the 100m2 data:

  # data_100m2 = div_10m2Data100m2Data[which(nchar(div_10m2Data100m2Data$subplotID)<3), ]
  # data_10m2 = div_10m2Data100m2Data[which(nchar(div_10m2Data100m2Data$subplotID)>2), ]

  data_100m2 <- div_10m2Data100m2Data %>%
    dplyr::filter(grepl("_100$", subplotID))
  data_10m2 <- div_10m2Data100m2Data %>%
    dplyr::filter(grepl("_10_",subplotID))

  ###build 10m2 data by aggregating observations from 1m2
  # (years sampled both 1 and 10_100) and 10m2
  #rename 1m2 to combine with 10
  data_10m2Build <- div_1m2Data

  #rename 1m2 subplots so associated observations will combine with 10m2 observations
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "31.1.1"] <- "31.1.10"
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "31.4.1"] <- "31.4.10"
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "32.2.1"] <- "32.2.10"
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "32.4.1"] <- "32.4.10"
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "40.1.1"] <- "40.1.10"
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "40.3.1"] <- "40.3.10"
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "41.1.1"] <- "41.1.10"
  # data_10m2Build$subplotID[data_10m2Build$subplotID == "41.4.1"] <- "41.4.10"

  data_10m2Build <- data_10m2Build %>%
    dplyr::mutate(subplotID = gsub("_1_", "_10_",subplotID))

  #combine what was the nested 1m2 subplot data with the 10m2 data to get the
  # complete list of species in each 10m2 subplot
  data_10m2 <- dplyr::bind_rows(data_10m2, data_10m2Build)

  ###aggregate 100m2 data by combining 10m2 observations with 100m2 observations
  #rename 10m2 to combine with 100m2
  data_100m2Build <- data_10m2

  # rename 10m2 subplots so associated observations will combine with 100m2 observations
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "31.1.10"] <- 31
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "31.4.10"] <- 31
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "32.2.10"] <- 32
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "32.4.10"] <- 32
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "40.1.10"] <- 40
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "40.3.10"] <- 40
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "41.1.10"] <- 41
  # data_100m2Build$subplotID[data_100m2Build$subplotID == "41.4.10"] <- 41

  data_100m2Build <- data_100m2Build %>%
    dplyr::mutate(subplotID = gsub("_10_[0-9]","_100", subplotID))

  # combine what was the nested 10m2 subplot data with the 100m2 data to get
  # the complete list of species in each 100m2 subplot
  data_100m2 <- dplyr::bind_rows(data_100m2, data_100m2Build)

  #################recombine the 1m2 data#####################
  div_1m2Data <- dplyr::bind_rows(div_1m2Data, div_1m2DataOut)

  #################remove duplicates and combine to one data frame#####################
  ###make sure unique at each scale after combinations Need to figure out how to do unique on specific columns or get rid Different people might have measured the 1 and 10m subplots which could result in otherwise duplicate entries, for example. Maybe have to get rid of the stuff like date above?
  div_1m2Data <- div_1m2Data %>% dplyr::distinct()
  data_10m2 <- data_10m2 %>% dplyr::distinct()
  data_100m2 <- data_100m2 %>% dplyr::distinct()


  ###create the 400m2 plot species lists
  data_400m2 <- data_100m2
  data_400m2$subplotID <- "400"
  data_400m2 <- data_400m2 %>% dplyr::distinct()

  ###this chunk combines the data from all scales of measurement into one table

  #data10_100_400 <- rbind(data_10m2, data_100m2)
  div_1m2Data$totalSampledArea <- 1
  data_10m2$totalSampledArea <- 10
  data_100m2$totalSampledArea <- 100
  data_400m2$totalSampledArea <- 400
  data <- dplyr::bind_rows(div_1m2Data, data_10m2, data_100m2, data_400m2)

  #remove duplicates by primary key fields (in variables table)
  divPlantPresenceData <- data %>%
    dplyr::distinct(namedLocation,
                    plotID,
                    subplotID,
                    boutNumber,
                    eventID,
                    taxonID,
                    identificationQualifier,
                    morphospeciesID,
                    targetTaxaPresent,
                    .keep_all = TRUE)


  #don't pass target taxa present to larger-scale subplots if not true
  divPlantPresenceData <- divPlantPresenceData %>%
    dplyr::group_by(eventID, plotID, subplotID) %>%
    dplyr::filter(
      !(targetTaxaPresent == "Y" &
        is.na(scientificName) &
        is.na(taxonID))) %>%
    dplyr::mutate(tot = dplyr::n()) %>%
    dplyr::filter(
      (tot > 1 & targetTaxaPresent != "N")
      | (tot <= 1)) %>%
    dplyr::select(-tot) %>%
    dplyr::ungroup()


  # filter to totalSampledAreaFilter if necessary
  if(!is.na(totalSampledAreaFilter)){
    if(totalSampledAreaFilter %in% c(1,10,100,400)){
      divPlantPresenceData <- divPlantPresenceData %>%
        dplyr::filter(
          totalSampledArea == totalSampledAreaFilter)
    }else{
      message(totalSampledAreaFilter, " is not a valid option for 'totalSampledAreaFilter', returning the full dataset.")
    }
  }

  return(divPlantPresenceData)
}
