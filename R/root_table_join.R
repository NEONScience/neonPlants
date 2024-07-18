###################################################################################################
#' @title Join NEON Plant Belowground Biomass Root Mass and Root Chemistry Data Into a Single Table

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description This function uses NEON Plant Belowground Biomass data (DP1.10067.001) retrieved 
#' using the neonUtilities::loadByProduct() function (preferred), downloaded from the NEON Data 
#' Portal, or input data tables with an equivalent structure and representing the same site x month
#' combinations. The function joins the bbc_rootmass, bbc_chemistryPooling, and bbc_rootChemistry
#' tables to generate a single table that contains both mass and chemistry data for each sampleID
#' and subsampleID (i.e., sizeCategory).
#'
#' @details For table joining to be successful, all input data frames must contain data from the
#' same site x month combination(s). When analytical replicates exist in the bbc_rootChemistry
#' table, this function returns the mean and concatenates analyte-specific QF values, dataQF
#' values, and remarks into a single string for all analytical replicates. 
#' 
#' In the joined output table, 'rootChemistryDataQF' and 'rootChemistryRemarks' are new fields 
#' that result from the concatenation described above, and there is a new 'analyticalRepCount'
#' field to document the number of analytical replicates associated with each chemistry data 
#' point.
#'
#' @param input_mass The 'bbc_rootmass' table for the site x month combination(s) of interest.
#' [data.frame]
#' @param input_pool The 'bbc_chemistryPooling' table for the site x month combination(s) of
#' interest. [data.frame]
#' @param input_chem The 'bbc_rootChemistry' table for the site x month combination(s) of
#' interest. [data.frame]
#' 
#' @return A table containing both root mass and root chemistry data in the same row for
#' different root sizeCategories (i.e., subsampleIDs) within a sampleID. The subsampleIDs in 
#' older data with rootStatus == "dead" do not have root chemistry data, and the function returns
#' these rows with no chemistry data.
#' 
#' @examples
#' \dontrun{
#' #   Obtain NEON Plant Belowground Biomass data
#' bbc <- neonUtilities::loadByProduct(
#' dpID = "DP1.10067.001",
#' site = "all",
#' startdate = "2022-07",
#' enddate = "2022-08",
#' tabl = "all",
#' check.size = FALSE
#' )
#' 
#' #   Join downloaded root data
#' df <- neonPlants::root_table_join(
#' input_mass = bbc$bbc_rootmass,
#' input_pool = bbc$bbc_chemistryPooling,
#' input_chem = bbc$bbc_rootChemistry
#' )
#'
#' }
#' 
#' @export root_table_join

###################################################################################################

root_table_join <- function(input_mass,
                            input_pool,
                            input_chem) {

  ### Verify user-supplied input_mass table contains correct data
  rootMass <- input_mass
  
  #   Check for required columns
  massExpCols <- c("domainID", "siteID", "plotID", "sampleID", "subsampleID", "rootStatus", "dryMass")
  
  if (length(setdiff(massExpCols, colnames(rootMass))) > 0) {
    #stop(glue::glue("Expected columns missing from input_mass: {setdiff(massExpCols, colnames(rootMass))}"))
    stop(glue::glue('Required columns missing from input_mass: {paste(setdiff(massExpCols, colnames(rootMass)), collapse = ", ")}'))
  }
  
  #   Check for data
  if (nrow(rootMass) == 0) {
    stop(glue::glue("Table input_mass has no data."))
  }



  ### Verify user-supplied input_pool table contains correct data
  rootPool <- input_pool

  #   Check for required columns
  poolExpCols <- c("domainID", "siteID", "plotID", "subsampleIDList", "cnSampleID")

  if (length(setdiff(poolExpCols, colnames(rootPool))) > 0) {
    stop(glue::glue('Required columns missing from input_pool: {paste(setdiff(poolExpCols, colnames(rootPool)), collapse = ", ")}'))
  }

  #   Check for data
  if (nrow(rootPool) == 0) {
    stop(glue::glue("Table input_pool has no data."))
  }




  ### Verify user-supplied input_chem table contains correct data
  rootChem <- input_chem

  #   Check for required columns
  chemExpCols <- c("cnSampleID", "d15N", "d13C", "nitrogenPercent", "carbonPercent", "CNratio",
                   "cnIsotopeQF", "cnPercentQF", "isotopeAccuracyQF", "percentAccuracyQF", "dataQF", "remarks")
  
  if (length(setdiff(chemExpCols, colnames(rootChem))) > 0) {
    stop(glue::glue('Required columns missing from input_chem: {paste(setdiff(chemExpCols, colnames(rootChem)), collapse = ", ")}'))
  }
  
  #   Check for data
  if (nrow(rootChem) == 0) {
    stop(glue::glue("Table input_chem has no data."))
  }




  ### Join rootChem and rootPool tables to associate the cnSampleID with the rootMass subsampleID
  #   Select needed columns from rootPool
  rootPool <- rootPool %>%
    dplyr::select(domainID,
                  siteID,
                  plotID,
                  subsampleIDList,
                  cnSampleID)


  ##  Expand subsampleIDList if "|" exists in string; pivot_longer() approach preserves all columns in input df
  rootPool <- rootPool %>%
    #   tempSub1: If subsampleIDList contains pipe, extract everything before pipe
    #   tempSub2: If subsamleIDList contains pipe, extract everything after pipe
    dplyr::mutate(tempSub1 = dplyr::case_when(grepl("\\|", subsampleIDList) ~ stringr::str_extract(subsampleIDList,
                                                                                                   pattern = "^.*?(?=\\|)"),
                                              TRUE ~ subsampleIDList),
                  tempSub2 = dplyr::case_when(grepl("\\|", subsampleIDList) ~ stringr::str_extract(subsampleIDList,
                                                                                                   pattern = "[^\\|]*$"),
                                              TRUE ~ NA)) %>%
    tidyr::pivot_longer(cols = c(tempSub1, tempSub2),
                        names_to = NULL,
                        values_to = "subsampleID") %>%
    dplyr::relocate(subsampleID,
                    .before = cnSampleID) %>%
    dplyr::filter(!is.na(subsampleID)) %>%
    dplyr::select(-subsampleIDList)


  ##  Summarise rootChem table: Calculate means for analytical replicates and preserve QF values
  rootChem <- rootChem %>%
    dplyr::group_by(cnSampleID) %>%
    dplyr::summarise(analyticalRepCount = n(),
                     d15N = dplyr::case_when(all(is.na(d15N)) ~ NA,
                                             TRUE ~ round(mean(d15N, na.rm = TRUE), digits = 1)),
                     d13C = dplyr::case_when(all(is.na(d13C)) ~ NA,
                                             TRUE ~ round(mean(d13C, na.rm = TRUE), digits = 1)),
                     nitrogenPercent = dplyr::case_when(all(is.na(nitrogenPercent)) ~ NA,
                                                        TRUE ~ round(mean(nitrogenPercent, na.rm = TRUE), digits = 2)),
                     carbonPercent = dplyr::case_when(all(is.na(carbonPercent)) ~ NA,
                                                      TRUE ~ round(mean(carbonPercent, na.rm = TRUE), digits =1)),
                     CNratio = dplyr::case_when(all(is.na(CNratio)) ~ NA,
                                                TRUE ~ round(mean(CNratio, na.rm = FALSE), digits = 1)),
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
                                                            dplyr::n_distinct(dataQF, na.rm = TRUE) == 1 ~ unique(dataQF),
                                                            TRUE ~ paste(dataQF, collapse = ", ")),
                     rootChemistryRemarks = dplyr::case_when(all(is.na(remarks)) ~ NA,
                                                             dplyr::n_distinct(remarks, na.rm = TRUE) == 1 ~ unique(remarks),
                                                             TRUE ~ paste(remarks, collapse = ", ")))
  
  
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
    dplyr::filter(rootStatus == "dead")
  
  #   Join rootStatus == "live" and NA rootMass data with rootChem data
  rootMass <- rootMass %>%
    dplyr::filter(is.na(rootStatus) | rootStatus == "live") %>%
    dplyr::left_join(rootChem,
                     by = c("domainID", "siteID", "plotID", "subsampleID")) %>%
    dplyr::bind_rows(tempDead) %>%
    dplyr::arrange(domainID,
                   siteID,
                   plotID,
                   sampleID,
                   subsampleID)
  
  
  
  ### Return function output
  return(rootMass)


} # end function
