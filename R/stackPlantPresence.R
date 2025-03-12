#' @title Stack NEON Plant Presence and Percent Cover data
#' 
#' @author
#' Dave T Barnett \email{dbarnettl@battelleecology.org} \cr
#' Eric Sokol \email{esokol@battelleecology.org} \cr
#' 
#' @description Data inputs are NEON Plant Presence and Percent Cover data (DP1.10058.001) retrieved with the neonUtilities::loadByProduct() function (preferred), data downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations. The stackPlantPresence() function aggregates the occurrence data from the Plant Presence and Percent Cover data product to return all plant species present at the scale of each subplot sampled (1m2, 10m2, and 100m2) as well as the plot scale (400m2). In contrast, data downloaded from the NEON data portal report species in 10m2 and 100m2 subplots only if those species are not encountered in smaller-scale subplots, and a list of species present within the entire 400m2 scale plot is not provided.
#' 
#' @details Input data may be provided either as a list generated from the neonUtilities::laodByProduct() function or as individual tables. However, if both list and table inputs are provided at the same time the function will error. 
#'
#' @param inputDataList A list object comprised of NEON Plant Presence and Percent Cover tables (DP1.10058.001) downloaded with the neonUtilities::loadByProduct() function (defaults to required). If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for the 'input_1m2Data' and 'input_10m2Data100m2Data' arguments.[list]
#' 
#' @param totalSampledAreaFilter The subplot or plot size for which data are returned (in meters squared). Default (NA) will return data for all subplot and plot sizes. If a valid filter integer is provided, the returned data are filtered to the desired subplot or plot size. Input options are NA, 1, 10, 100, 400. [integer] 
#'
#' @param input_1m2Data The 'div_1m2Data' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param input_10m2Data100m2Data The 'div_10m2Data100m2Data' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @return A data frame that aggregates plant species lists across all subplots (or those subplots defined in 'totalSampledAreaFilter' argument) and for the entire plot where subplotID is '400' (see Data Product documentation for subplot description). The column 'totalSampledArea' is also created to reflect the size of the subplot or plot in square meters in which a given taxon was detected. Description of other columns can be found in the Data Product User Guide.
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
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
#'   inputDataList = allDiv)
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
#'   inputDataList = list(
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
#'   inputDataList = list(
#'     div_1m2Data = my_1m_data_SRER_43,
#'     div_10m2Data100m2Data = my_10_100m_data_SRER_43))
#' }
#'
#' @export stackPlantPresence


stackPlantPresence <- function(inputDataList,
                               input_1m2Data = NA,
                               input_10m2Data100m2Data = NA,
                               totalSampledAreaFilter = NA_integer_) {
  
  
  
  ### Test that user has supplied arguments as required by function ####
  
  
  
  ### Verify user-supplied inputDataList object contains correct data if not NA
  if (!missing(inputDataList)) {
    
    #   Check that input is a list
    if (!inherits(inputDataList, "list")) {
      stop(glue::glue("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"))
    }                                                                                                                                      
    
    #   Check that required tables within list match expected names
    listExpNames <- c("div_1m2Data", "div_10m2Data100m2Data")
    
    #   Identify missing tables
    missing_tables <- setdiff(listExpNames, names(inputDataList))
    
    #   If there are missing tables, construct an informative error message
    if (length(missing_tables) > 0) {
      
      #   Base error message listing the missing tables
      error_msg <- glue::glue("Required tables missing from 'inputDataList': {paste(missing_tables, collapse = ', ')}.")
      
      #   Add extra explanation if "div_10m2Data100m2Data" is missing
      if ("div_10m2Data100m2Data" %in% missing_tables) {
        
        error_msg <- glue::glue("{error_msg} If missing table 'div_10m2Data100m2Data', be sure the 10m2 and 100m2 subplots ",
                                "were observed at the site x year combination, as these data are collected every other year.")
      }
      
      #   Stop execution with the final message
      stop(error_msg)
    }
    
  } else {
    
    inputDataList <- NULL
    
  } # end missing conditional
  
  
  
  ### Verify table inputs are NA if inputDataList is supplied
  if (inherits(inputDataList, "list") & (!is.logical(input_1m2Data) | !is.logical(input_10m2Data100m2Data))) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }
  
  
  
  ### Verify all table inputs are data frames if inputDataList is NA
  if (is.null(inputDataList) & 
      (!inherits(input_1m2Data, "data.frame") | !inherits(input_10m2Data100m2Data, "data.frame"))) {
    
    stop("Data frames must be supplied for all table inputs if 'inputDataList' is missing. If missing table 'div_10m2Data100m2Data', be sure the 10m2 and 100m2 subplots were sampled the site x year as these data are collected every other year.")
    
  }
  
  
  
  ### Conditionally define input tables ####
  if (inherits(inputDataList, "list")) {
    
    div_1m2Data <- inputDataList$div_1m2Data
    div_10m2Data100m2Data <- inputDataList$div_10m2Data100m2Data
    
  } else {
    
    div_1m2Data <- input_1m2Data
    div_10m2Data100m2Data <- input_10m2Data100m2Data
    
  }
  
  
  
  ### Verify input tables contain required columns and data ####
  
  ### Verify 'div_1m2Data' table contains required data
  #   Check for required columns
  massExpCols <- c("domainID", "siteID", "plotID", "endDate", "divDataType", 
                   "subplotID", "boutNumber", "eventID", "taxonID", "identificationQualifier", "morphospeciesID", "targetTaxaPresent",
                   "namedLocation", "decimalLatitude", "decimalLongitude", "geodeticDatum", "coordinateUncertainty", "elevation", "elevationUncertainty", "nlcdClass", 
                   "plotType", "scientificName", "taxonRank", "family", "nativeStatusCode", "identificationQualifier", "morphospeciesID",
                   "samplingImpractical", "samplingImpracticalRemarks", "biophysicalCriteria", "publicationDate", "release")
  
  if (length(setdiff(massExpCols, colnames(div_1m2Data))) > 0) {
    stop(glue::glue("Required columns missing from 'input_1m2Data':", '{paste(setdiff(massExpCols, colnames(div_1m2Data)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(div_1m2Data) == 0) {
    stop(glue::glue("Table 'input_1m2Data' has no data."))
  }
  
  
  
  ### Verify 'div_10m2Data100m2Data' table contains required data
  #   Check for required columns
  massExpCols <- c("domainID", "siteID", "plotID", "endDate",
                   "subplotID", "boutNumber", "eventID", "taxonID", "identificationQualifier", "morphospeciesID", "targetTaxaPresent",
                   "namedLocation", "decimalLatitude", "decimalLongitude", "geodeticDatum", "coordinateUncertainty", "elevation", "elevationUncertainty", "nlcdClass", 
                   "plotType", "scientificName", "taxonRank", "family", "nativeStatusCode", "identificationQualifier", "morphospeciesID",
                   "samplingImpractical", "samplingImpracticalRemarks", "biophysicalCriteria", "publicationDate", "release") 
  
  if (length(setdiff(massExpCols, colnames(div_10m2Data100m2Data))) > 0) {
    stop(glue::glue("Required columns missing from 'input_10m2Data100m2Data':", '{paste(setdiff(massExpCols, colnames(div_10m2Data100m2Data)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(div_10m2Data100m2Data) == 0) {
    stop(glue::glue("Table 'input_10m2Data100m2Data' has no data."))
  }
  
  
  
  ### Verify totalSampledAreaFilter input is of expected value
  areaFilterExpVals <- c(1, 10, 100, 400, NA)
  
  if (!all(totalSampledAreaFilter %in% areaFilterExpVals)) {
    stop("Error: 'totalSampledAreaFilter' must be one of 1, 10, 100, 400, or NA.")
  }
  
  
  
  ### Handling outdated subplotID format of Plant Presence and Percent Cover data that appear in 2023 and earlier NEON data releases ####
  
  #   Function to facilitate the accommodation of old subplotID formats
  reformatSubplotID <- function(x) {
    
    x_split <- strsplit(x, split = "\\.") %>% 
      unlist()
    
    if ((length(x_split) == 3) & x_split[3] %in% c(1, 10, 100, 400)) {
      
      return(paste(x_split[c(1,3,2)], 
                   collapse = "_"))
      
    } else {return(x)}
  }
  
  #   Reformat subplotID
  div_10m2Data100m2Data <- div_10m2Data100m2Data %>%
    dplyr::mutate(subplotID_old = .data$subplotID,
                  subplotID = dplyr::case_when(nchar(subplotID) == 2 ~ paste0(subplotID, "_100"),
                                               grepl("\\.", subplotID) ~ sapply(subplotID, reformatSubplotID, USE.NAMES = FALSE),
                                               .default = .data$subplotID))
  
  div_1m2Data <- div_1m2Data %>%
    dplyr::mutate(subplotID_old = .data$subplotID,
                  subplotID = dplyr::case_when(nchar(subplotID) == 2 ~ paste0(subplotID, "_100"),
                                               grepl("\\.", subplotID) ~ sapply(subplotID, reformatSubplotID, USE.NAMES = FALSE),
                                               .default = .data$subplotID))
  
  
  
  ### Data manipulation and organization ####
  
  ##  Deal with eventID and year - 1m2 data
  #   Create 'year' column
  div_1m2Data$year <- substr(div_1m2Data$endDate, 
                             start = 1, 
                             stop = 4)
  
  #   Create 'eventID' if missing or if string is longer than expected
  div_1m2Data$eventID <- ifelse(is.na(div_1m2Data$eventID) | stringr::str_length(div_1m2Data$eventID) > 11,
                                paste(div_1m2Data$siteID, 
                                      div_1m2Data$boutNumber, 
                                      div_1m2Data$year, 
                                      sep = "."),
                                div_1m2Data$eventID)
  
  div_1m2Data <- div_1m2Data %>% 
    dplyr::select(-"year")
  
  
  ##  Deal with eventID and year - larger subplot data table
  #   Create 'year' column
  div_10m2Data100m2Data$year <- substr(div_10m2Data100m2Data$endDate, 
                                       start = 1, 
                                       stop = 4)
  
  div_10m2Data100m2Data$eventID <- ifelse(is.na(div_10m2Data100m2Data$eventID) | stringr::str_length(div_10m2Data100m2Data$eventID) > 11,
                                          paste(div_10m2Data100m2Data$siteID,
                                                div_10m2Data100m2Data$boutNumber,
                                                div_10m2Data100m2Data$year, sep = "."),
                                          div_10m2Data100m2Data$eventID)
  
  div_10m2Data100m2Data <- div_10m2Data100m2Data %>% 
    dplyr::select(-"year")
  
  
  ##  Limit 1m2 data to plant species records
  div_1m2Data <- div_1m2Data %>%
    dplyr::filter(.data$divDataType == "plantSpecies")
  
  #   Columns to keep
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
  
  
  ##  Remove fields/columns that prevent generation of unique records
  #   div_1m2Data
  div_1m2Data <- div_1m2Data %>% 
    dplyr::select(tidyr::any_of(c("div_1m2Data", cols2keep)))
  
  #   data 10_100
  div_10m2Data100m2Data <- div_10m2Data100m2Data %>% 
    dplyr::select(tidyr::any_of(c("div_10m2Data100m2Data", cols2keep)))
  
  
  
  ### Remove irrelevant 1m2 only site-year combinations ####
  
  ##  Site-year combinations where only the 1m2 subplots were scheduled and intended to be sampled are not relevant to function output and should not be including in stack function routine; don't process 1m2 only year with larger subplot data that don't exist
  
  #   1m2 data data frame of eventIDs
  smallEventID <- dplyr::select(.data = div_1m2Data, 
                                "eventID") %>% 
    unique()
  
  #   10_100m2 data data frame of eventIDs
  bigEventID <- dplyr::select(.data = div_10m2Data100m2Data, 
                              "eventID") %>% 
    unique()
  
  #   Make df of those eventIDs sampled only at 1m2 and not the larger subplots
  smallOut <- dplyr::anti_join(smallEventID, 
                               bigEventID, 
                               by = "eventID")
  
  #   Pull out the corresponding data into unique data frame that will not be incorporated in the larger scale data
  div_1m2DataOut <- div_1m2Data %>%
    dplyr::filter(.data$eventID %in% smallOut$eventID)
  
  #   Make df of eventIDs common to both
  smallMerge <- dplyr::inner_join(smallEventID, 
                                  bigEventID, 
                                  by = "eventID")
  
  smallMergeEventID <- smallMerge$eventID
  
  #   subset to data that corresponds to evenID in both the small and large data
  div_1m2Data <- div_1m2Data %>%
    dplyr::filter(.data$eventID %in% smallMergeEventID)
  
  
  
  ### Set up data aggregation across scales ####
  
  #   Identify records collected at 10m2 and 100m2 scales using subplotID string information
  data_100m2 <- div_10m2Data100m2Data %>%
    dplyr::filter(grepl("_100$", .data$subplotID))
  
  data_10m2 <- div_10m2Data100m2Data %>%
    dplyr::filter(grepl("_10_", .data$subplotID))
  
  
  ##  Build 10m2 data by aggregating observations from 1m2 (years sampled both 1 and 10_100) and 10m2
  #   Rename 1m2 to combine with 10
  data_10m2Build <- div_1m2Data
  
  data_10m2Build <- data_10m2Build %>%
    dplyr::mutate(subplotID = gsub("_1_", "_10_", .data$subplotID))
  
  #   Combine what was the nested 1m2 subplot data with the 10m2 data to get the complete list of species in each 10m2 subplot
  data_10m2 <- dplyr::bind_rows(data_10m2, 
                                data_10m2Build)
  
  
  ##  Aggregate 100m2 data by combining 10m2 observations with 100m2 observations
  #   Rename 10m2 to combine with 100m2
  data_100m2Build <- data_10m2
  
  data_100m2Build <- data_100m2Build %>%
    dplyr::mutate(subplotID = gsub("_10_[0-9]","_100", .data$subplotID))
  
  
  ## Combine what was the nested 10m2 subplot data with the 100m2 data to get the complete list of species in each 100m2 subplot
  data_100m2 <- dplyr::bind_rows(data_100m2, 
                                 data_100m2Build)
  
  
  
  ### Recombine the 1m2 data ####
  
  div_1m2Data <- dplyr::bind_rows(div_1m2Data, 
                                  div_1m2DataOut)
  
  
  
  ### Remove duplicates and combine to one data frame ####
  
  ##  Make sure observations are unique at each scale after combinations
  div_1m2Data <- div_1m2Data %>% 
    dplyr::distinct()
  
  data_10m2 <- data_10m2 %>% 
    dplyr::distinct()
  
  data_100m2 <- data_100m2 %>% 
    dplyr::distinct()
  
  
  ##  Create the 400m2 plot species lists
  data_400m2 <- data_100m2
  data_400m2$subplotID <- "400"
  
  data_400m2 <- data_400m2 %>% 
    dplyr::distinct()
  
  
  ##  Combines the data from all scales of measurement into one table
  div_1m2Data$totalSampledArea <- 1
  data_10m2$totalSampledArea <- 10
  data_100m2$totalSampledArea <- 100
  data_400m2$totalSampledArea <- 400
  
  data <- dplyr::bind_rows(div_1m2Data, 
                           data_10m2, 
                           data_100m2, 
                           data_400m2)
  
  #   Remove duplicates by primary key fields (in variables table)
  divPlantPresenceData <- dplyr::distinct(.data = data,
                                          .data$namedLocation,
                                          .data$plotID,
                                          .data$subplotID,
                                          .data$boutNumber,
                                          .data$eventID,
                                          .data$taxonID,
                                          .data$identificationQualifier,
                                          .data$morphospeciesID,
                                          .data$targetTaxaPresent,
                                          .keep_all = TRUE)
  
  
  ##  Don't pass target taxa present to larger-scale subplots if not true
  divPlantPresenceData <- divPlantPresenceData %>%
    dplyr::group_by(.data$eventID,
                    .data$plotID,
                    .data$subplotID) %>%
    dplyr::filter(!(.data$targetTaxaPresent == "Y" & is.na(.data$scientificName) & is.na(.data$taxonID))) %>%
    dplyr::mutate(tot = dplyr::n()) %>%
    dplyr::filter((.data$tot > 1 & .data$targetTaxaPresent != "N") | (.data$tot <= 1)) %>%
    dplyr::select(-"tot") %>%
    dplyr::ungroup()
  
  
  ##  Filter to totalSampledAreaFilter if necessary
  if (!is.na(totalSampledAreaFilter)) {
    
    if (totalSampledAreaFilter %in% c(1, 10, 100, 400)) {
      
      divPlantPresenceData <- divPlantPresenceData %>%
        dplyr::filter(.data$totalSampledArea == totalSampledAreaFilter)
      
    } else {
      message(totalSampledAreaFilter, " is not a valid option for 'totalSampledAreaFilter', returning the full dataset.")
    }
  }
  
  
  ##  Return output
  return(divPlantPresenceData)
}
