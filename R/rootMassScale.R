###################################################################################################
#' @title Scale Root Biomass by Size Category to Mass Per Area and Mass Per Volume

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description Join NEON Plant Belowground Biomass data tables (DP1.10067.001) to calculate fine 
#' root biomass by sizeCategory as well as total fine root biomass per unit area and per unit soil
#' volume. Fine root fragment mass (root fragments < 1 cm length) can optionally be calculated 
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
#' @param includeFragments Indicator to determine whether mass of root fragments < 1 cm length 
#' calculated from dilution sampling should be included in the 'totalDryMass' outputs. Defaults
#' to FALSE. If set to TRUE, the 'bbc_dilution' table must be provided to the 'inputDilution' 
#' argument. [logical]
#' 
#' @return A table containing root mass data per unit area ("g/m2") and per unit volume ("g/m3")
#' for three sizeCategories (< 1mm, 1-2mm, and 2-10mm) as well as total fine root biomass summed 
#' across all sizeCategories. The output no longer contains the 'rootStatus' field.
#' 
#' @examples
#' \dontrun{
#' #   Obtain NEON Plant Belowground Biomass core data
#' bbc <- neonUtilities::loadByProduct(
#' dpID = "DP1.10067.001",
#' site = "all",
#' startdate = "2022-07",
#' enddate = "2022-08",
#' tabl = "all",
#' check.size = FALSE
#' )
#' 
#' #   Calculate root mass per unit area and per unit volume
#' df <- neonPlants::rootMassScale(
#' inputCore = bbc$bbc_percore,
#' inputMass = bbc$bbc_rootmass,
#' inputDilution = bbc$bbc_dilution,
#' includeFragments = FALSE
#' )
#'
#' }
#' 
#' @export rootMassScale

###################################################################################################

rootMassScale <- function(inputCore,
                          inputMass,
                          inputDilution = NA,
                          includeFragments = FALSE) {
  
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
    stop("Table 'inputCore' has no data.")
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
    stop("Table 'inputMass' has no data.")
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
      stop("Table 'inputDilution' has no data.")
    }
    
  } # end inputDilution conditional
  
  
  
  ### Verify includeFragments is logical
  if (!is.logical(includeFragments)) {
    stop("The 'inputFragments' argument must be of type logical.")
  }
  
  
  
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
  
  coreMass <- dplyr::arrange(.data = coreMass,
                             domainID,
                             siteID,
                             eventID,
                             plotID,
                             subsampleID)
  
  
  
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
  
  
  
  ### Pivot sizeCategory masses wider and sum to calculate total fine root biomass by sampleID
  coreMass <- dplyr::select(.data = coreMass, -subsampleID)
  coreMass <- tidyr::pivot_wider(data = coreMass,
                                 names_from = sizeCategory,
                                 values_from = dryMass,
                                 names_prefix = "dryMass")
  
  #   Conditionally rename fragment column if exists
  if (is.data.frame(inputDilution)) {
  
    coreMass <- dplyr::rename(.data = coreMass,
                            dryMassFrag = dryMassfrag)
  
  }
  
  
  
  ### Conditionally calculate 'totalDryMass' dependent on includeFragments argument
  #   Default: totalDryMass does not include fragment mass
  if (isFALSE(includeFragments)) {
    
    coreMass <- dplyr::rowwise(data = coreMass) %>%
      dplyr::mutate(totalDryMass = sum(`dryMass0-1`, `dryMass1-2`, `dryMass2-10`, 
                                       na.rm = TRUE)) %>%
      dplyr::ungroup()
    
  }
  
  #   Optional: Include fragment mass in totalDryMass
  if (isTRUE(includeFragments)) {
    
    #   Check that inputDilution data frame is provided
    if (!is.data.frame(inputDilution)) {
      stop("A valid 'inputDilution' data frame must be provided when 'includeFragments' is TRUE.")
    } 
    
    #   Sum totalDryMass WITH fragment mass
    coreMass <- dplyr::rowwise(data = coreMass) %>%
      dplyr::mutate(totalDryMass = sum(`dryMass0-1`, `dryMass1-2`, `dryMass2-10`, `dryMassFrag`,
                                       na.rm = TRUE)) %>%
      dplyr::ungroup()
    
  } # end fragment TRUE conditional
  
  
  
  ### Scale dryMass values to "g/m2" and "g/cm3"
  coreMass <- dplyr::rowwise(data = coreMass) %>%
    dplyr::mutate(totalDryMass = sum(`dryMass0-1`, `dryMass1-2`, `dryMass2-10`, 
                                     na.rm = TRUE)) %>%
    dplyr::mutate(`dryMass0-1PerArea` = "blah",
                  `dryMass1-2PerArea` = "blah",
                  `dryMass2-10PerArea` = "blah",
                  totalMassPerArea = round(totalDryMass/rootSampleArea, digits = 1)) %>%
    dplyr::mutate(`dryMass0-1PerVolume` = "blah",
                  `dryMass1-2PerVolume` = "blah",
                  `dryMass2-10PerVolume` = "blah",
                  totalMassPerVolume = round(totalDryMass/(rootSampleArea * (rootSampleDepth/100)), digits = 1)) %>%
    dplyr::ungroup()
  
  
  
  ### Return output
  return(coreMass)
  
} # end function