###################################################################################################
#' @title Join NEON Plant Belowground Biomass Root Mass and Root Chemistry Data Into a Single Table

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description Using NEON Plant Belowground Biomass data (DP1.10067.001) retrieved using the
#' neonUtilities::loadByProduct() function (preferred), downloaded from the NEON Data Portal,
#' or input data tables with an equivalent structure and representing the same site x month
#' combinations, this function joins the bbc_rootmass, bbc_chemistryPooling, and bbc_rootChemistry
#' tables to generate a single table that contains both mass and chemistry data for each sampleID.
#'
#' @details For table joining to be successful, all input data frames must contain data from the
#' same site x month combination(s). When analytical replicates exist in the bbc_rootChemistry
#' table, this function returns the mean and concatenates analyte-specific QF values, dataQF
#' values, and bbc_rootChemistry remarks into a single string for all analytical replicates.
#'
#' @param input_mass The 'bbc_rootmass' table for the site x month combination(s) of interest.
#' [data.frame]
#' @param input_pool The 'bbc_chemistryPooling' table for the site x month combination(s) of
#' interest. [data.frame]
#' @param input_chem The 'bbc_rootChemistry' table for the site x month combination(s) of
#' interest. [data.frame]

#' @return A table containing both root mass and root chemistry data in the same row for
#' different root sizeCategories within a sampleID.

#' @examples
#' \dontrun{
#'
#' }

#' @export root_table_join

###################################################################################################

root_table_join <- function(input_mass,
                            input_pool,
                            input_chem) {

  ### Verify user-supplied input_mass table contains correct data
  rootMass <- input_mass





  ### Verify user-supplied input_pool table contains correct data
  rootPool <- input_pool

  #   Check for required columns
  poolExpCols <- c("domainID", "siteID", "plotID", "subsampleIDList", "cnSampleID")

  if (length(setdiff(poolExpCols, colnames(rootPool))) > 0) {
    stop(glue::glue("Expected columns missing from input_pool: {setdiff(poolExpCols, colnames(rootPool))}"))
  }

  #   Check for data
  if (nrow(rootPool) == 0) {
    stop(glue::glue("Table input_pool has no data."))
  }




  ### Verify user-supplied input_chem table contains correct data
  rootChem <- input_chem

  #   Check for required columns
  chemExpCols <- c("cnSampleID", "d15N", "d13C", "")




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


  ##  Summarise rootChem table: Calculate means for analytical replicates;
  ##  data with QF values other than "OK" in any QF column are removed first.
  temp1 <- rootChem %>%
    dplyr::group_by(cnSampleID) %>%
    dplyr::reframe(d15N = round(mean(d15N, na.rm = TRUE), digits = 1),
                   d13C = round(mean(d13C, na.rm = TRUE), digits = 1),
                   nitrogenPercent = round(mean(nitrogenPercent, na.rm = TRUE), digits = 2),
                   carbonPercent = round(mean(carbonPercent, na.rm = TRUE), digits =1),
                   CNratio = round(mean(CNratio, na.rm = TRUE), digits = 1),
                   cnIsotopeQF = paste(cnIsotopeQF, collapse = ", "),
                   cnPercentQF = paste(cnPercentQF, collapse = ", "),
                   isotopeAccuracyQF = paste(isotopeAccuracyQF, collapse = ", "),
                   percentAccuracyQF = paste(percentAccuracyQF, collapse = ", "),
                   chemDataQF = ifelse(is.na(dataQF), NA, paste(dataQF, collapse = ", ")),
                   chemRemarks = ifelse(is.na(remarks), NA, paste(remarks, collapse = ", "))) %>%
    dplyr::mutate(across(everything(), ~replace(., . == "NA", NA)),
                  across(everything(), ~replace(., . == "NaN", NA)))
  
  temp2 <- rootChem %>%
    dplyr::group_by(cnSampleID) %>%
    dplyr::summarise(d15N = round(mean(d15N, na.rm = TRUE), digits = 1),
                     d13C = round(mean(d13C, na.rm = TRUE), digits = 1),
                     nitrogenPercent = round(mean(nitrogenPercent, na.rm = TRUE), digits = 2),
                     carbonPercent = round(mean(carbonPercent, na.rm = TRUE), digits =1),
                     CNratio = round(mean(CNratio, na.rm = TRUE), digits = 1))
  
  temp3 <- rootChem %>%
    dplyr::group_by(cnSampleID) %>%
    dplyr::summarise(chemRepCount = n(),
                     #d15N = round(mean(d15N, na.rm = TRUE), digits = 1),
                     d15N = dplyr::case_when(all(is.na(d15N)) ~ NA,
                                             TRUE ~ round(mean(d15N, na.rm = TRUE), digits = 1)),
                     d13C = round(mean(d13C, na.rm = TRUE), digits = 1),
                     nitrogenPercent = round(mean(nitrogenPercent, na.rm = TRUE), digits = 2),
                     carbonPercent = round(mean(carbonPercent, na.rm = TRUE), digits =1),
                     CNratio = round(mean(CNratio, na.rm = FALSE), digits = 1),
                     cnIsotopeQF = dplyr::case_when(all(cnIsotopeQF == "OK") ~ "OK",
                                                    all(is.na(cnIsotopeQF)) ~ NA,
                                                    TRUE ~ paste(cnIsotopeQF, collapse = ", ")),
                     cnPercentQF = dplyr::case_when(all(cnPercentQF == "OK") ~ "OK",
                                                    TRUE ~ paste(cnPercentQF, collapse = ", "))
                     )







}
