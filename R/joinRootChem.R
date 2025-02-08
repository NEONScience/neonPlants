#' @title Join NEON plant below ground biomass root mass and root chemistry data into a single table

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description Join the 'bbc_rootmass', 'bbc_chemistryPooling', and 'bbc_rootChemistry' tables to generate a single table that contains both mass and chemistry data for each sampleID and subsampleID (i.e., sizeCategory). Data inputs are NEON Plant Belowground Biomass data (DP1.10067.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations. 
#'
#' @details Input data may be provided either as a list or as individual tables. However, if both list and table inputs are provided at the same time the function will error out. For table joining to be successful, inputs must contain data from the same site x month combination(s) for all tables. When analytical replicates exist in the 'bbc_rootChemistry' table, the function returns the mean and concatenates analyte-specific QF values, dataQF values, and remarks into a single string for all analytical replicates. 
#' 
#' In the joined output table, 'rootChemistryDataQF' and 'rootChemistryRemarks' are new fields that result from the concatenation described above, and there is a new 'analyticalRepCount' field to document the number of analytical replicates associated with each chemistry data point.
#' 
#' @param inputDataList A list object comprised of Plant Below Ground Biomass tables (DP1.10067.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided. [list]
#'
#' @param inputMass The 'bbc_rootmass' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputPool The 'bbc_chemistryPooling' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputChem The 'bbc_rootChemistry' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @return A table containing both root mass and root chemistry data in the same row for different root sizeCategories (i.e., subsampleIDs) within a sampleID. The subsampleIDs in older data with rootStatus == "dead" do not have root chemistry data, and the function returns these rows with no chemistry data.
#' 
#' @examples
#' \dontrun{
#' #   Obtain NEON Plant Belowground Biomass data
#' bbc <- neonUtilities::loadByProduct(
#' dpID = "DP1.10067.001",
#' site = "LENO",
#' startdate = "2018-07",
#' enddate = "2018-08",
#' tabl = "all",
#' check.size = FALSE
#' )
#' 
#' #   Join downloaded root data
#' df <- neonPlants::joinRootChem(
#' inputDataList = bbc,
#' inputMass = NA,
#' inputPool = NA,
#' inputChem = NA
#' )
#'
#' }
#' 
#' @export joinRootChem



joinRootChem <- function(inputDataList,
                         inputMass = NA,
                         inputPool = NA,
                         inputChem = NA) {
  
  ### Test that user has supplied arguments as required by function ####
  
  ### Verify user-supplied inputDataList object contains correct data if not NA
  if (!missing(inputDataList)) {
    
    #   Check that input is a list
    if (!inherits(inputDataList, "list")) {
      stop(glue::glue("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"))
    }
    
    #   Check that required tables within list match expected names
    listExpNames <- c("bbc_rootmass", "bbc_chemistryPooling", "bbc_rootChemistry")
    
    if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from 'inputDataList':",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
    }
  } else {
    
    inputDataList <- NULL
    
  } # end missing conditional
  
  
  
  ### Verify table inputs are NA if inputDataList is supplied
  if (inherits(inputDataList, "list") & (!is.logical(inputMass) | !is.logical(inputPool) | !is.logical(inputChem))) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }
  
  
  
  ### Verify all table inputs are data frames if inputDataList is NA
  if (is.null(inputDataList) & 
      (!inherits(inputMass, "data.frame") | !inherits(inputPool, "data.frame") | !inherits(inputChem, "data.frame"))) {
    
    stop("Data frames must be supplied for all table inputs if 'inputDataList' is missing")
    
  }
  
  
  
  ### Conditionally define input tables ####
  if (inherits(inputDataList, "list")) {
    
    rootMass <- inputDataList$bbc_rootmass
    rootPool <- inputDataList$bbc_chemistryPooling
    rootChem <- inputDataList$bbc_rootChemistry
    
  } else {
    
    rootMass <- inputMass
    rootPool <- inputPool
    rootChem <- inputChem
    
  }
  
  
  
  ### Verify input tables contain required columns and data ####
  
  ### Verify 'rootMass' table contains required data
  #   Check for required columns
  massExpCols <- c("domainID", "siteID", "plotID", "sampleID", "subsampleID", "rootStatus", "dryMass")
  
  if (length(setdiff(massExpCols, colnames(rootMass))) > 0) {
    stop(glue::glue("Required columns missing from 'inputMass':", '{paste(setdiff(massExpCols, colnames(rootMass)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(rootMass) == 0) {
    stop(glue::glue("Table 'inputMass' has no data."))
  }



  ### Verify 'inputPool' table contains required data
  #   Check for required columns
  poolExpCols <- c("domainID", "siteID", "plotID", "subsampleIDList", "cnSampleID")

  if (length(setdiff(poolExpCols, colnames(rootPool))) > 0) {
    stop(glue::glue("Required columns missing from 'inputPool':", '{paste(setdiff(poolExpCols, colnames(rootPool)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(rootPool) == 0) {
    stop(glue::glue("Table 'inputPool' has no data."))
  }



  ### Verify 'inputChem' table contains required data
  #   Check for required columns
  chemExpCols <- c("cnSampleID", "d15N", "d13C", "nitrogenPercent", "carbonPercent", "CNratio",
                   "cnIsotopeQF", "cnPercentQF", "isotopeAccuracyQF", "percentAccuracyQF", "dataQF", "remarks")
  
  if (length(setdiff(chemExpCols, colnames(rootChem))) > 0) {
    stop(glue::glue("Required columns missing from 'inputChem':", '{paste(setdiff(chemExpCols, colnames(rootChem)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(rootChem) == 0) {
    stop(glue::glue("Table 'inputChem' has no data."))
  }
  
  
  
  ### Table joining ####

  ### Join rootChem and rootPool tables to associate the cnSampleID with the rootMass subsampleID
  #   Select needed columns from rootPool
  rootPool <- rootPool %>%
    dplyr::select("domainID",
                  "siteID",
                  "plotID",
                  "subsampleIDList",
                  "cnSampleID")


  ##  Expand subsampleIDList if "|" exists in string; pivot_longer() approach preserves all columns in input df
  rootPool <- rootPool %>%
    #   tempSub1: If subsampleIDList contains pipe, extract everything before pipe
    #   tempSub2: If subsamleIDList contains pipe, extract everything after pipe
    dplyr::mutate(tempSub1 = dplyr::case_when(grepl("\\|", .data$subsampleIDList) ~ stringr::str_extract(.data$subsampleIDList,
                                                                                                         pattern = "^.*?(?=\\|)"),
                                              TRUE ~ .data$subsampleIDList),
                  tempSub2 = dplyr::case_when(grepl("\\|", .data$subsampleIDList) ~ stringr::str_extract(.data$subsampleIDList,
                                                                                                         pattern = "[^\\|]*$"),
                                              TRUE ~ NA)) %>%
    tidyr::pivot_longer(cols = c("tempSub1", "tempSub2"),
                        names_to = NULL,
                        values_to = "subsampleID") %>%
    dplyr::relocate("subsampleID",
                    .before = "cnSampleID") %>%
    dplyr::filter(!is.na(.data$subsampleID)) %>%
    dplyr::select(-"subsampleIDList")


  ##  Summarise rootChem table: Calculate means for analytical replicates and preserve QF values
  rootChem <- rootChem %>%
    dplyr::group_by(.data$cnSampleID) %>%
    dplyr::summarise(analyticalRepCount = dplyr::n(),
                     d15N = dplyr::case_when(all(is.na(d15N)) ~ NA,
                                             TRUE ~ round(mean(d15N, na.rm = TRUE), digits = 1)),
                     d13C = dplyr::case_when(all(is.na(d13C)) ~ NA,
                                             TRUE ~ round(mean(d13C, na.rm = TRUE), digits = 1)),
                     nitrogenPercent = dplyr::case_when(all(is.na(nitrogenPercent)) ~ NA,
                                                        TRUE ~ round(mean(nitrogenPercent, na.rm = TRUE), digits = 2)),
                     carbonPercent = dplyr::case_when(all(is.na(carbonPercent)) ~ NA,
                                                      TRUE ~ round(mean(carbonPercent, na.rm = TRUE), digits = 2)),
                     CNratio = dplyr::case_when(all(is.na(CNratio)) ~ NA,
                                                TRUE ~ round(mean(CNratio, na.rm = TRUE), digits = 1)),
                     cnIsotopeQF = dplyr::case_when(all(cnIsotopeQF == "OK") ~ "OK",
                                                    all(is.na(cnIsotopeQF)) ~ NA,
                                                    TRUE ~ paste(cnIsotopeQF, collapse = ", ")),
                     cnPercentQF = dplyr::case_when(all(cnPercentQF == "OK") ~ "OK",
                                                    all(is.na(cnPercentQF)) ~ NA,
                                                    TRUE ~ paste(cnPercentQF, collapse = ", ")),
                     isotopeAccuracyQF = dplyr::case_when(all(isotopeAccuracyQF == "OK") ~ "OK",
                                                          all(is.na(isotopeAccuracyQF)) ~ NA,
                                                          TRUE ~ paste(isotopeAccuracyQF, collapse = ", ")),
                     percentAccuracyQF = dplyr::case_when(all(percentAccuracyQF == "OK") ~ "OK",
                                                          all(is.na(percentAccuracyQF)) ~ NA,
                                                          TRUE ~ paste(percentAccuracyQF, collapse = ", ")),
                     rootChemistryDataQF = dplyr::case_when(all(is.na(dataQF)) ~ NA,
                                                            dplyr::n_distinct(dataQF, na.rm = TRUE) == 1 ~ 
                                                              paste(unique(dataQF[!is.na(dataQF)]), collapse = ", "),
                                                            TRUE ~ paste(dataQF, collapse = ", ")),
                     rootChemistryRemarks = dplyr::case_when(all(is.na(remarks)) ~ NA,
                                                             dplyr::n_distinct(remarks, na.rm = TRUE) == 1 ~ 
                                                               paste(unique(remarks[!is.na(remarks)]), collapse = ", "),
                                                             TRUE ~ paste(remarks, collapse = ", ")),
                     .groups = "drop")
  
  
  ##  Join rootPool and rootChem tables
  rootChem <- rootPool %>%
    dplyr::left_join(rootChem,
                     by = "cnSampleID")
  
  
  
  ### Associate root mass data with root chemistry data
  #   Note: qaDryMass == TRUE samples are not filtered out.
  #   Note: for older data, rootStatus == "dead" are filtered out since these have no chem data but
  #         share the same subsampleID as the "live" samples that do have chem data. The "dead" samples
  #         are then added back to the joined data frame via bind.
  
  #   Identify rootStatus == "dead" samples
  tempDead <- rootMass %>%
    dplyr::filter(.data$rootStatus == "dead")
  
  #   Join rootStatus == "live" and NA rootMass data with rootChem data
  rootMass <- rootMass %>%
    dplyr::filter(is.na(.data$rootStatus) | .data$rootStatus == "live") %>%
    dplyr::left_join(rootChem,
                     by = c("domainID", "siteID", "plotID", "subsampleID")) %>%
    dplyr::bind_rows(tempDead) %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$plotID,
                   .data$sampleID,
                   .data$subsampleID)
  
  
  
  ### Return function output
  return(rootMass)

} # end function
