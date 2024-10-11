#' @title Scale Root Biomass by Size Category to Mass Per Area and Mass Per Volume

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description Join NEON Plant Belowground Biomass data tables (DP1.10067.001) to calculate fine 
#' root biomass by sizeCategory as well as total fine root biomass per unit area and per unit soil
#' volume. Fine root fragment mass (root fragments < 1 cm length) can optionally be calculated 
#' for the subset of cores for which it is generated, and fragment mass can optionally be included
#' in the summed total fine root biomass.
#' 
#' Data inputs are NEON Plant Belowground Biomass data (DP1.10067.001) retrieved using the 
#' neonUtilities::loadByProduct() function (preferred), data downloaded from the NEON Data Portal, 
#' or input data tables with an equivalent structure and representing the same site x month combinations. 
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
#' @param inputRootList A list object comprised of Plant Below Ground Biomass tables (DP1.10067.001) 
#' downloaded using the neonUtilities::loadByProduct() function (defaults to required). If list 
#' input is provided, the table input arguments must all be NA. [list]
#' 
#' @param includeDilution Indicator for whether mass of root fragments < 1 cm length should be
#' calculated, as estimated via the Dilution Sampling method (Defaults to TRUE). If TRUE and
#' inputDilution is NA, the 'bbc_dilution' table will be extracted from the list input. [logical]
#' 
#' @param inputCore The 'bbc_percore' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputRootList' argument must be NA. [data.frame]
#'
#' @param inputMass The 'bbc_rootmass' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputRootList' argument must be NA. [data.frame]
#' 
#' @param inputDilution The 'bbc_dilution' table for the site x month combination(s) of interest
#' (optional, defaults to NA). If table input is provided, the 'inputRootList' argument must be 
#' NA. [data.frame]
#' 
#' @param includeFragInTotal Indicator for whether mass of root fragments < 1 cm length 
#' calculated from dilution sampling should be included when summing across sizeCategory to 
#' calculate the 'totalDryMass'. Defaults to FALSE. If set to TRUE and 'inputRootList' is NA, 
#' the 'bbc_dilution' table must be provided to the 'inputDilution' argument. [logical]
#' 
#' @return A table containing root mass data per unit area ("g/m2") and per unit volume ("g/m3")
#' for three sizeCategories (< 1mm, 1-2mm, and 2-10mm) as well as total fine root biomass summed 
#' across all sizeCategories. The output no longer contains the 'rootStatus' field and QA dryMass
#' samples are averaged.
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
#' df <- neonPlants::scaleRootMass(
#' inputRootList = bbc,
#' includeDilution = TRUE,
#' inputCore = NA,
#' inputMass = NA,
#' inputDilution = NA,
#' includeFragInTotal = FALSE
#' )
#'
#' }
#' 
#' @export scaleRootMass


scaleRootMass <- function(inputRootList,
                          includeDilution = TRUE,
                          inputCore = NA,
                          inputMass = NA,
                          inputDilution = NA,
                          includeFragInTotal = FALSE) {
  
  ### Test that user has supplied arguments as required by function ####
  
  ### Verify 'includeDilution' and 'includeFragInTotal' are of type logical
  #   Check 'includeDilution'
  if (!is.logical(includeDilution)) {
    stop(glue::glue("Argument 'includeDilution' must be type logical; supplied input is {class(includeDilution)}"))
  }
  
  #   Check 'includeFragInTotal'
  if (!is.logical(includeFragInTotal)) {
    stop(glue::glue("Agrument 'includeFragInTotal' must be type logical; supplied input is {class(includeFragInTotal)}"))
  }
  
  
  
  ### Verify user-supplied 'inputRootList' object contains correct data if not NA
  if (!is.logical(inputRootList)) {
    
    #   Check that input is a list
    if (!inherits(inputRootList, "list")) {
      stop(glue::glue("Argument 'inputRootList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputRootList)}"))
    }
    
    #   Check that required tables within list match expected names
    listExpNames <- c("bbc_percore", "bbc_rootmass", "bbc_dilution")
    
    #   Account for includeDilution argument and whether 'bbc_dilution' is required
    if (isTRUE(includeDilution)) {
      
      #   All expected tables required when includeDilution == TRUE
      if (length(setdiff(listExpNames, names(inputRootList))) > 0) {
        stop(glue::glue("Required tables missing from 'inputRootList':",
                        '{paste(setdiff(listExpNames, names(inputRootList)), collapse = ", ")}',
                        .sep = " "))
      }
      
    } else if (isFALSE(includeDilution)) {
      
      #   Table 'bbc_dilution' not required when includeDilution == FALSE
      if (length(setdiff(listExpNames[1:2], names(inputRootList))) > 0) {
        stop(glue::glue("Required tables missing from 'inputRootList':",
                        '{paste(setdiff(listExpNames, names(inputRootList)), collapse = ", ")}',
                        .sep = " "))
      }
      
    } # end includeDilution conditional
  } # end is.logical 'inputRootList'
  
  
  
  ### Verify table inputs are NA if 'inputRootList' is supplied
  if (inherits(inputRootList, "list") & (!is.logical(inputCore) | !is.logical(inputMass) | !is.logical(inputDilution))) {
    stop("When 'inputRootList' is supplied all table input arguments must be NA")
  }
  
  
  
  ### Verify 'inputCore' and 'inputMass' are data frames if 'inputRootList' is NA
  if (is.logical(inputRootList) & 
      (!inherits(inputCore, "data.frame") | !inherits(inputMass, "data.frame"))) {
    
    stop("Data frames must be supplied for all table inputs if 'inputRootList' is NA")
  }
  
  
  
  ### Verify 'inputDilution' is a data frame if 'inputRootList' is NA and 'includeDilution' is TRUE
  if (is.logical(inputRootList) & isTRUE(includeDilution) & !inherits(inputDilution, "data.frame")) {
    
    stop("A data frame must be supplied to 'inputDilution' when 'inputRootList' is NA and includeDilution is TRUE")
  }
  
  
  
  ### Conditionally define input tables ####
  if (inherits(inputRootList, "list")) {
    
    rootCore <- inputRootList$bbc_percore
    rootMass <- inputRootList$bbc_rootmass
    
    if (isTRUE(includeDilution)) {
      rootDilution <- inputRootList$bbc_dilution
    }
    
  } else {
    
    rootCore <- inputCore
    rootMass <- inputMass
    
    if (isTRUE(includeDilution)) {
      rootDilution <- inputDilution
    }
    
  }
  
  
  ###---> continue with table-specific tests below
  
  
  
  
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
  
  
  
  ### Verify inputDilution table contains expected data, if provided
  if (is.data.frame(inputDilution)) {
    
    rootDilution <- inputDilution
    
    #   Check for required columns
    dilExpCols <- c("sampleID", "dilutionSubsampleVolume", "sampleVolume", "dryMass")
    
    if (length(setdiff(dilExpCols, colnames(rootDilution))) > 0) {
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
  
  
  
  ### Message if older data were provided with deprecated 0-0.5mm and 0.5-1mm sizeCategories
  if ("0-05" %in% rootMass$sizeCategory | "05-1" %in% rootMass$sizeCategory) {
    message("Deprecated '0-0.5mm' or '0.5-1mm' sizeCategories detected, binning output to current '0-1mm' sizeCategory.")
  }
  
  
  
  ### Standardize rootMass data to current sizeCategory definitions and average qaDryMass = Y
  rootMass <- neonPlants::standardizeRootMass(inputMass = rootMass)
  
  #   Collapse mycorrhizaeVisible and massRemarks to single string per sampleID to avoid downstream
  #   dupes when pivot_wider() is used; these will be re-joined by sampleID after wide table is created
  stringMassCols <- dplyr::select(.data = rootMass,
                                  sampleID,
                                  mycorrhizaeVisible,
                                  remarks) %>%
    dplyr::group_by(sampleID) %>%
    dplyr::summarise(mycorrhizaeVisible = dplyr::case_when(all(is.na(mycorrhizaeVisible)) ~ NA,
                                                           dplyr::n_distinct(mycorrhizaeVisible, na.rm = TRUE) == 1 ~ 
                                                             paste(unique(mycorrhizaeVisible[!is.na(mycorrhizaeVisible)]), collapse = ", "),
                                                           TRUE ~ paste(mycorrhizaeVisible, collapse = ", ")),
                     massRemarks = dplyr::case_when(all(is.na(remarks)) ~ NA,
                                                    dplyr::n_distinct(remarks, na.rm = TRUE) == 1 ~ 
                                                      paste(unique(remarks[!is.na(remarks)]), collapse = ", "),
                                                    TRUE ~ paste(remarks, collapse = ", ")),
                     .groups = "drop")
  
  #   Remove mycorrhizaeVisible and remarks from rootMass for clean downstream pivot_wider() result
  rootMass <- dplyr::select(.data = rootMass,
                            -subsampleID,
                            -mycorrhizaeVisible,
                            -remarks)
  
  
  
  ### Join rootMass data with rootCore data
  rootCore <- dplyr::filter(.data = rootCore,
                            samplingImpractical == "OK")
  
  coreMass <- dplyr::full_join(x = rootCore,
                               y = rootMass,
                               by = c("domainID", "siteID", "plotID", "collectDate", "sampleID")) %>%
    dplyr::rename(coreRemarks = remarks)
  
  coreMass <- dplyr::arrange(.data = coreMass,
                             domainID,
                             siteID,
                             eventID,
                             plotID,
                             sampleID,
                             sizeCategory)
  
  
  
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
      dplyr::mutate(sizeCategory = "frag",
                    .after = sampleID)
    
    #   Join rootDilution data with rootCore data
    coreDilMass <- dplyr::right_join(x = rootCore,
                                     y = rootDilution,
                                     by = c("domainID", "siteID", "plotID", "collectDate", "sampleID")) %>%
      dplyr::rename(coreRemarks = remarks)
    
    #   Bind standard mass rows with dilution mass rows; split out massRemarks then re-join since value is
    #   always 'NA' for dilution rows and causes dupes when pivot_wider() is used and a value exists for
    #   standard masses.
    coreMass <- dplyr::bind_rows(coreMass,
                                 coreDilMass) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$eventID,
                     .data$plotID,
                     .data$sampleID,
                     .data$sizeCategory)
    
  } # end dilution conditional
  
  
  
  ### Pivot sizeCategory masses wider and sum to calculate total fine root biomass by sampleID
  coreMass <- tidyr::pivot_wider(data = coreMass,
                                 names_from = sizeCategory,
                                 values_from = dryMass,
                                 names_prefix = "dryMass")
  
  #   Re-join collapsed strings for mycorrhizaeVisible and massRemarks columns
  coreMass <- dplyr::left_join(x = coreMass,
                               y = stringMassCols,
                               by = "sampleID")
  
  coreMass <- dplyr::relocate(.data = coreMass,
                              mycorrhizaeVisible,
                              massRemarks,
                              .after = release)
  
  #   Conditionally rename fragment column if exists
  if (is.data.frame(inputDilution)) {
  
    coreMass <- dplyr::rename(.data = coreMass,
                            dryMassFrag = dryMassfrag)
  
  }
  
  
  
  ### Conditionally calculate 'totalDryMass' dependent on includeFragments argument
  ##  Default: totalDryMass does not include fragment mass
  if (isFALSE(includeFragments)) {
    
    coreMass <- dplyr::rowwise(data = coreMass) %>%
      dplyr::mutate(totalDryMass = sum(`dryMass0-1`, `dryMass1-2`, `dryMass2-10`, 
                                       na.rm = TRUE)) %>%
      dplyr::ungroup()
    
  } # end fragment FALSE conditional
  
  
  ##  Optional: Include fragment mass in totalDryMass
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
    dplyr::mutate(`dryMass0-1PerArea` = round(`dryMass0-1`/rootSampleArea, digits = 1),
                  `dryMass1-2PerArea` = round(`dryMass1-2`/rootSampleArea, digits = 1),
                  `dryMass2-10PerArea` = round(`dryMass2-10`/rootSampleArea, digits = 1),
                  totalMassPerArea = round(totalDryMass/rootSampleArea, digits = 1)) %>%
    dplyr::mutate(`dryMass0-1PerVolume` = round(`dryMass0-1`/(rootSampleArea * (rootSampleDepth/100)), digits = 1),
                  `dryMass1-2PerVolume` = round(`dryMass1-2`/(rootSampleArea * (rootSampleDepth/100)), digits = 1),
                  `dryMass2-10PerVolume` = round(`dryMass2-10`/(rootSampleArea * (rootSampleDepth/100)), digits = 1),
                  totalMassPerVolume = round(totalDryMass/(rootSampleArea * (rootSampleDepth/100)), digits = 1)) %>%
    dplyr::ungroup()
  
  
  
  ### Return output
  return(coreMass)
  
} # end function