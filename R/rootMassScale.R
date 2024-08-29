###################################################################################################
#' @title Scale Root Biomass by Size Category to Area or Volume

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description Join NEON Plant Belowground Biomass data tables (DP1.10067.001) to calculate fine 
#' root biomass by sizeCategory per unit area and per unit soil volume, as well as total fine root
#' biomass. Fine root fragment mass (root fragments < 1 cm length) can optionally be calculated 
#' for the subset of cores for which it is generated, and fragment mass is not included in the summed
#' total fine root biomass.
#' 
#' Input data should be the 'bbc_percore' and 'bbc_rootmass' tables retrieved using the 
#' neonUtilities::loadByProduct() function (preferred), downloaded from the NEON Data Portal, or
#' input data tables with an equivalent structure and representing the same site x month 
#' combinations. 
#'
#' @details NEON weighs a minimum of 5% of samples a second time so that data users can estimate
#' the uncertainty associated with different technicians weighing dried roots; QA samples of this
#' nature are identified via qaDryMass == "Y". The function calculates the mean when QA masses 
#' exist and any 'remarks' are concatenated. Samples with Sampling Impractical values other than "OK"
#' are removed prior to generating output data.
#' 
#' If inputMass data collected prior to 2019 are provided, the 0-0.5mm and 0.5-1mm sizeCategories
#' are combined into the current 0-1mm sizeCategory.
#' 
#' @param inputCore The 'bbc_percore' table for the site x month combination(s) of interest
#' (required). [data.frame]
#'
#' @param inputMass The 'bbc_rootmass' table for the site x month combination(s) of interest
#' (required). [data.frame]
#' 
#' @param inputDilution The 'bbc_dilution' table for the site x month combination(s) of interest
#' (optional, defaults to NA). [data.frame]
#' 
#' @return A table containing root mass data per unit area or per unit volume for three 
#' sizeCategories (< 1mm, 1-2mm, and 2-10mm) as well as total fine root biomass summed across all
#' sizeCategories. The output no longer contains the 'rootStatus' field.
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
#' #   Calculate root mass per unit area
#' df <- neonPlants::rootMassScale(
#' inputCore = bbc$bbc_percore,
#' inputMass = bbc$bbc_rootmass,
#' inputDilution = bbc$bbc_dilution
#' )
#'
#' }
#' 
#' @export rootMassScale

###################################################################################################

rootMassScale <- function(inputCore,
                          inputMass,
                          inputDilution = NA) {
  
  ### Verify user-supplied inputCore table contains expected data
  rootCore <- inputCore
  
  #   Check for required columns
  coreExpCols <- c("domainID", "siteID", "plotID", "clipID", "coreID", "collectDate", "eventID", "sampleID",
                   "rootSampleArea", "rootSampleDepth")
  
  if (length(setdiff(coreExpCols, colnames(rootCore))) > 0) {
    stop(glue::glue("Required columns missing from 'inputCore':", '{paste(setdiff(coreExpCols, colnames(rootCore)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(rootCore) == 0) {
    stop(glue::glue("Table 'inputCore' has no data."))
  }
  
  
  
  ### Verify user-supplied inputMass table contains expected data
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
  
  #   Warn if older data were provided with deprecated 0-0.5mm and 0.5-1mm sizeCategories
  if ("0-05" %in% rootMass$sizeCategory | "05-1" %in% rootMass$sizeCategory) {
    warning("Deprecated '0-0.5mm' or '0.5-1mm' sizeCategories detected, binning output to current '0-1mm' sizeCategory.")
  }
  
  
  
  ### Verify inputDilution table contains expected data, if provided
  if (is.data.frame(inputDilution)) {
    
    rootDilution <- inputDilution
    
    #   Check for required columns
    dilExpCols <- c("sampleID", "dilutionSubsampleVolume", "sampleVolume", "dryMass")
    
    if (length(setdiff(massExpCols, colnames(rootMass))) > 0) {
      stop(glue::glue("Required columns missing from 'inputDilution':", '{paste(setdiff(dilExpCols, colnames(rootDilution)), collapse = ", ")}',
                      .sep = " "))
    }
    
    #   Check for data
    if (nrow(rootDilution) == 0) {
      stop(glue::glue("Table 'inputDilution' has no data."))
    }
    
  } # end inputDilution conditional
  
  
  
  ### Standardize rootMass data to current sizeCategory definitions and average qaDryMass = Y
  rootMass <- neonPlants::rootMassStandardize(inputMass = rootMass)
  
  
  
  ### Join rootMass data with rootCore data
  rootCore <- dplyr::filter(.data = rootCore,
                            samplingImpractical == "OK")
  
  coreMass <- dplyr::full_join(x = rootCore,
                               y = rootMass,
                               by = c("domainID", "siteID", "plotID", "collectDate", "sampleID")) %>%
    dplyr::rename(coreRemarks = remarks.x,
                  massRemarks = remarks.y)
  
  
  
  ### Calculate dilution sampling fragment mass, if applicable
  if (is.data.frame(inputDilution)) {
    
    rootDilution <- rootDilution %>%
      dplyr::filter(!is.na(.data$dryMass)) %>%
      dplyr::mutate(fragMass = round(dryMass*(sampleVolume/dilutionSubsampleVolume),
                                     digits = 4),
                    .before = dryMass) %>%
      dplyr::filter(.data$fragMass >= 0) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$collectDate,
                      .data$sampleID) %>%
      dplyr::summarise(dryMass = round(mean(fragMass, na.rm = TRUE), digits = 4),
                       .groups = "drop") %>%
      dplyr::mutate(subsampleID = paste(sampleID, "DIL", sep = "."),
                    sizeCategory = "frag",
                    .after = sampleID)
    
    #   Join rootDilution data with rootCore data
    coreDilMass <- dplyr::right_join(x = rootCore,
                                     y = rootDilution,
                                     by = c("domainID", "siteID", "plotID", "collectDate", "sampleID")) %>%
      dplyr::rename(coreRemarks = remarks)
    
    coreMass <- dplyr::bind_rows(coreMass,
                                 coreDilMass) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$eventID,
                     .data$plotID,
                     .data$subsampleID)
    
  } # end dilution conditional
  
  
  
  ### Pivot sizeCategory masses wider so that total fine root biomass may be summed by sampleID
  test <- dplyr::select(.data = coreMass, -subsampleID)
  temp <- tidyr::pivot_wider(data = test,
                             names_from = sizeCategory,
                             values_from = dryMass,
                             names_prefix = "dryMass_")
  #--> appears to work
  
  
  
  
  
  
  
} # end function