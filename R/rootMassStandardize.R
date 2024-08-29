###################################################################################################
#' @title Standardize Root Dry Mass Data to Current Size Category Bins

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description NEON root mass data collected prior to 2019 were sorted to four sizeCategories and
#' were parsed into "live" and "dead" rootStatus categories. Beginning in 2019 the two smallest 
#' sizeCategories (< 0.5mm and 0.5-1mm) were combined into a single < 1mm sizeCategory and roots 
#' were no longer sorted to "live/dead". This function combines the older smallest two sizeCategories
#' into the current < 1mm category and pools together roots previously categorized as "live" and
#' "dead". Input data should be the 'bbc_rootmass' table from the NEON Plant Belowground Biomass
#' data product (DP1.10067.001) retrieved using the neonUtilities::loadByProduct() function 
#' (preferred), downloaded from the NEON Data Portal, or input data tables with an equivalent 
#' structure and representing the same site x month combinations. 
#'
#' @details NEON weighs a minimum of 5% of samples a second time so that data users can estimate
#' the uncertainty associated with different technicians weighing dried roots; QA samples of this
#' nature are identified via qaDryMass == "Y". The function calculates the mean when QA masses 
#' exist and any 'remarks' are concatenated. Samples with Sampling Impractical values other than "OK"
#' are removed prior to summarizing the input data.
#'
#' @param inputMass The 'bbc_rootmass' table for the site x month combination(s) of interest
#' (required). [data.frame]
#' 
#' @return A table containing root mass data for three sizeCategories (< 1mm, 1-2mm, and 2-10mm)
#' and dryMass values pooled across previously utilized "live/dead" rootStatus categories. The 
#' output no longer contains the 'rootStatus' field. 
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
#' #   Standardize downloaded root data to current sizeCategories and rootStatus
#' df <- neonPlants::rootMassStandardize(
#' inputMass = bbc$bbc_rootmass
#' )
#'
#' }
#' 
#' @export rootMassStandardize

###################################################################################################

rootMassStandardize <- function(inputMass) {
  
  ### Verify user-supplied inputMass table contains correct data
  rootMass <- inputMass
  
  #   Check for required columns
  massExpCols <- c("domainID", "siteID", "plotID", "samplingImpractical", "sampleID", "subsampleID", 
                   "sizeCategory", "rootStatus", "dryMass", "qaDryMass", "remarks")
  
  if (length(setdiff(massExpCols, colnames(rootMass))) > 0) {
    stop(glue::glue("Required columns missing from 'inputMass':", '{paste(setdiff(massExpCols, colnames(rootMass)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(rootMass) == 0) {
    stop(glue::glue("Table 'inputMass' has no data."))
  }
  
  
  
  ### Combine 'qaDryMass' replicates and concatenate 'remarks'
  rootMass <- rootMass %>%
    dplyr::filter(.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical)) %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$collectDate,
                    .data$sampleID,
                    .data$subsampleID,
                    .data$sizeCategory,
                    .data$rootStatus) %>%
    dplyr::summarise(dryMass = dplyr::case_when(all(is.na(dryMass)) ~ NA,
                                                TRUE ~ round(mean(dryMass, na.rm = TRUE), digits = 4)),
                     mycorrhizaeVisible = dplyr::case_when(all(is.na(mycorrhizaeVisible)) ~ NA,
                                                           dplyr::n_distinct(mycorrhizaeVisible, na.rm = TRUE) == 1 ~ 
                                                             paste(unique(mycorrhizaeVisible[!is.na(mycorrhizaeVisible)]), collapse = ", "),
                                                           TRUE ~ paste(mycorrhizaeVisible, collapse = ", ")),
                     remarks = dplyr::case_when(all(is.na(remarks)) ~ NA,
                                                dplyr::n_distinct(remarks, na.rm = TRUE) == 1 ~ 
                                                  paste(unique(remarks[!is.na(remarks)]), collapse = ", "),
                                                TRUE ~ paste(remarks, collapse = ", ")),
                     .groups = "drop")
  
  
  
  ### Combine live/dead root mass into single dryMass
  rootMass <- rootMass %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$collectDate,
                    .data$sampleID,
                    .data$sizeCategory) %>%
    dplyr::summarise(subsampleID = glue::glue(unique(.data$sampleID), unique(.data$sizeCategory), .sep = "."),
                     dryMass = sum(dryMass, na.rm = TRUE),
                     mycorrhizaeVisible = dplyr::case_when(all(is.na(mycorrhizaeVisible)) ~ NA,
                                                           dplyr::n_distinct(mycorrhizaeVisible, na.rm = TRUE) == 1 ~ 
                                                             paste(unique(mycorrhizaeVisible[!is.na(mycorrhizaeVisible)]), collapse = ", "),
                                                           TRUE ~ paste(mycorrhizaeVisible, collapse = ", ")),
                     remarks = dplyr::case_when(all(is.na(remarks)) ~ NA,
                                                dplyr::n_distinct(remarks, na.rm = TRUE) == 1 ~ 
                                                  paste(unique(remarks[!is.na(remarks)]), collapse = ", "),
                                                TRUE ~ paste(remarks, collapse = ", ")),
                     .groups = "drop") 
  
  rootMass <- rootMass %>%
    dplyr::relocate(.data$sizeCategory,
                    .after = .data$subsampleID)
  
  
  
  ### Combine < 0.5mm and 0.5-1mm sizeCategory dryMass into single 0-1mm sizeCategory
  olderFineMass <- rootMass %>%
    dplyr::filter(.data$sizeCategory %in% c("0-05", "05-1")) %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$collectDate,
                    .data$sampleID) %>%
    dplyr::summarise(dryMass = sum(dryMass, na.rm = TRUE),
                     mycorrhizaeVisible = dplyr::case_when(all(is.na(mycorrhizaeVisible)) ~ NA,
                                                           dplyr::n_distinct(mycorrhizaeVisible, na.rm = TRUE) == 1 ~ 
                                                             paste(unique(mycorrhizaeVisible[!is.na(mycorrhizaeVisible)]), collapse = ", "),
                                                           TRUE ~ paste(mycorrhizaeVisible, collapse = ", ")),
                     remarks = dplyr::case_when(all(is.na(remarks)) ~ NA,
                                                dplyr::n_distinct(remarks, na.rm = TRUE) == 1 ~ 
                                                  paste(unique(remarks[!is.na(remarks)]), collapse = ", "),
                                                TRUE ~ paste(remarks, collapse = ", ")),
                     .groups = "drop") 
  
  olderFineMass <- olderFineMass %>%
    dplyr::mutate(subsampleID = paste(.data$sampleID, "0-1", sep = "."),
                  sizeCategory = "0-1",
                  .after = sampleID)
  
  rootMass <- rootMass %>%
    dplyr::filter(!.data$sizeCategory %in% c("0-05", "05-1")) %>%
    dplyr::bind_rows(olderFineMass) %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$plotID,
                   .data$sampleID,
                   .data$sizeCategory)
  
  
  
  ### Return output
  return(rootMass)
  
} # end function





