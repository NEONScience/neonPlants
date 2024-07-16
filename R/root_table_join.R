###################################################################################################
#' @title Join NEON Plant Belowground Biomass Root Mass and Root Chemistry Data Into a Single Table

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description Using NEON Plant Belowground Biomass data (DP1.10067.001) from the NEON Data
#' Portal, or input data tables with an equivalent structure, this function uses the bbc_rootmass,
#' bbc_chemistryPooling, and bbc_rootChemistry tables to generate a single table that contains
#' both mass and chemistry data for each sizeCategory within a given sampleID.
#'
#' @details For table joining to be successful, all input data frames must contain data from the
#' same site x month combination(s).
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
  chemExpCols <- c("")




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
  temp <- rootChem %>%
    #--> add na.rm = TRUE to mean() arguments...

  bbc_rootchem <- bbc_rootchem %>%
    dplyr::group_by(cnSampleID) %>%
    dplyr::summarise(nitrogenPercent = mean(nitrogenPercent),
                     carbonPercent = mean(carbonPercent),
                     CNratio = mean(CNratio),
                     .groups = "drop")



}
