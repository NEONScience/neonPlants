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
#' @details Input data may be provided either as a list generated from the neonUtilities::laodByProduct()
#' function or as individual tables. However, if both list and table inputs are provided at the same time
#' the function will error out. 
#' 
#' If inputMass data collected prior to 2019 are provided, the 0-0.5mm and 0.5-1mm 
#' sizeCategories are combined into the current 0-1mm sizeCategory.
#' 
#' NEON weighs a minimum of 5% of samples a second time so that data users can estimate
#' the uncertainty associated with different technicians weighing dried roots; QA samples of this
#' nature are identified via qaDryMass == "Y". The function calculates the mean when QA masses 
#' exist and any 'remarks' are concatenated. Samples with Sampling Impractical values other than "OK"
#' are removed prior to generating output data.
#' 
#' @param inputRootList A list object comprised of Plant Below Ground Biomass tables (DP1.10067.001) 
#' downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table
#' input arguments must all be NA; similarly, if list input is missing, table inputs must be
#' provided for 'inputCore' and 'inputMass' arguments at a minimum. [list]
#' 
#' @param includeDilution Indicator for whether mass of root fragments < 1 cm length should be
#' calculated, as estimated via the Dilution Sampling method (Defaults to TRUE). If TRUE and
#' 'inputDilution' is NA, the 'bbc_dilution' table will be extracted from the list input. [logical]
#' 
#' @param inputCore The 'bbc_percore' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputRootList' argument must be missing.
#' [data.frame]
#'
#' @param inputMass The 'bbc_rootmass' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputRootList' argument must be missing.
#' [data.frame]
#' 
#' @param inputDilution The 'bbc_dilution' table for the site x month combination(s) of interest
#' (optional, defaults to NA). If table input is provided, the 'inputRootList' argument must be 
#' missing. [data.frame]
#' 
#' @param includeFragInTotal Indicator for whether mass of root fragments < 1 cm length 
#' calculated from dilution sampling should be included when summing across sizeCategory to 
#' calculate the 'totalDryMass'. Defaults to FALSE. If set to TRUE and 'inputRootList' is missing, 
#' the 'bbc_dilution' table must be provided to the 'inputDilution' argument. [logical]
#' 
#' @return Three tables containing root mass data at varying spatial scales. The first 
#' "coreRootMass" table contains root mass data at the scale of the field-collected core, 
#' reported per unit area ("g/m2") and per unit volume ("g/m3") for three sizeCategories (< 1mm, 
#' 1-2mm, and 2-10mm) as well as total fine root biomass summed across all sizeCategories (separate
#' columns for "g/m2", "g/m3", and "Mg/ha"); output no longer contains the 'rootStatus' field, 
#' and QA dryMass samples are averaged. 
#' 
#' The second "plotRootMass" table contains mean root mass data at the scale of the plot for 
#' each eventID in the data, reported per unit area ("g/m2") and per unit volume ("g/m3") for three 
#' sizeCategories (< 1mm, 1-2mm, and 2-10mm) as well as total fine root biomass summed across all 
#' sizeCategories (separate columns for "g/m2", "Mg/ha", and "g/m3"). Uncertainty at the plot scale
#' is not reported because intra-plot replication is frequently insufficient and not required by the
#' sampling design, but the number of core samples used to calculate the mean is reported. The 
#' startDate and endDate indicate the start/end dates of core collection for a given plotID within 
#' a bout.
#' 
#' The third "siteRootMass" table provides mean site-level total root mass data for each 
#' eventID in the data, calculated from plot-level data and reported per unit area ("g/m2", "Mg/ha")
#' and per unit volume ("g/m3"). The standard deviation across plots is calculated and the number
#' of plots for each siteID x eventID combination is reported.
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
    stop(glue::glue("Argument 'includeFragInTotal' must be type logical; supplied input is {class(includeFragInTotal)}"))
  }
  
  
  
  ### Verify user-supplied 'inputRootList' object contains correct data if not missing
  if (!missing(inputRootList)) {
    
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
    
  } else {
    
    inputRootList <- NULL
    
  } # end missing conditional
  
  
  
  ### Verify table inputs are NA if 'inputRootList' is supplied
  if (inherits(inputRootList, "list") & (!is.logical(inputCore) | !is.logical(inputMass) | !is.logical(inputDilution))) {
    stop("When 'inputRootList' is supplied all table input arguments must be NA")
  }
  
  
  
  ### Verify 'inputCore' and 'inputMass' are data frames if 'inputRootList' is missing
  if (is.null(inputRootList) & 
      (!inherits(inputCore, "data.frame") | !inherits(inputMass, "data.frame"))) {
    
    stop("Data frames must be supplied for all table inputs if 'inputRootList' is not provided")
  }
  
  
  
  ### Verify 'inputDilution' is a data frame if 'inputRootList' is missing and 'includeDilution' is TRUE
  if (is.null(inputRootList) & isTRUE(includeDilution) & !inherits(inputDilution, "data.frame")) {
    
    stop("A data frame must be supplied to 'inputDilution' when 'inputRootList' is not provided and includeDilution is TRUE")
  }
  
  
  
  ### Verify 'includeDilution' is TRUE if 'includeFragInTotal' is TRUE
  if (isTRUE(includeFragInTotal) & isFALSE(includeDilution)) {
    
    stop("Valid dilution sampling data must be provided and 'includeDilution' must be TRUE when includeFragInTotal is TRUE")
  }
  
  
  
  ### Conditionally define input tables ####
  if (inherits(inputRootList, "list")) {
    
    rootCore <- inputRootList$bbc_percore
    rootMass <- inputRootList$bbc_rootmass
    
    if (isTRUE(includeDilution)) {
      rootDilution <- inputRootList$bbc_dilution
    } else {
      rootDilution <- inputDilution
    }
    
  } else if (is.null(inputRootList)) {
    
    rootCore <- inputCore
    rootMass <- inputMass
    rootDilution <- inputDilution
    
  }
  
  
  
  ### Verify input tables contain required columns and data ####
  
  ### Verify 'inputCore' table contains required data
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
  
  
  
  ### Verify 'inputMass' table contains expected data
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
  
  
  
  ### Verify inputDilution table contains expected data, if required
  if (is.data.frame(rootDilution)) {
    
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
  
  
  
  ### Message if older data were provided with deprecated 0-0.5mm and 0.5-1mm sizeCategories
  if ("0-05" %in% rootMass$sizeCategory | "05-1" %in% rootMass$sizeCategory) {
    message("Deprecated '0-0.5mm' or '0.5-1mm' sizeCategories detected, binning output to current '0-1mm' sizeCategory.")
  }
  
  
  
  ### Standardize rootMass data to current sizeCategory definitions and average qaDryMass = Y
  rootMass <- neonPlants::standardizeRootMass(inputMass = rootMass)
  
  #   Collapse mycorrhizaeVisible and massRemarks to single string per sampleID to avoid downstream
  #   dupes when pivot_wider() is used; these will be re-joined by sampleID after wide table is created
  stringMassCols <- dplyr::select(.data = rootMass,
                                  "sampleID",
                                  "mycorrhizaeVisible",
                                  "remarks") %>%
    dplyr::group_by(.data$sampleID) %>%
    dplyr::summarise(mycorrhizaeVisible = dplyr::case_when(all(is.na(.data$mycorrhizaeVisible)) ~ NA,
                                                           dplyr::n_distinct(.data$mycorrhizaeVisible, na.rm = TRUE) == 1 ~ 
                                                             paste(unique(.data$mycorrhizaeVisible[!is.na(.data$mycorrhizaeVisible)]), collapse = ", "),
                                                           TRUE ~ paste(.data$mycorrhizaeVisible, collapse = ", ")),
                     massRemarks = dplyr::case_when(all(is.na(.data$remarks)) ~ NA,
                                                    dplyr::n_distinct(.data$remarks, na.rm = TRUE) == 1 ~ 
                                                      paste(unique(.data$remarks[!is.na(.data$remarks)]), collapse = ", "),
                                                    TRUE ~ paste(.data$remarks, collapse = ", ")),
                     .groups = "drop")
  
  #   Remove mycorrhizaeVisible and remarks from rootMass for clean downstream pivot_wider() result
  rootMass <- dplyr::select(.data = rootMass,
                            -"subsampleID",
                            -"mycorrhizaeVisible",
                            -"remarks")
  
  
  
  ### Join rootMass data with rootCore data
  rootCore <- dplyr::filter(.data = rootCore,
                            .data$samplingImpractical == "OK")
  
  coreMass <- dplyr::full_join(x = rootCore,
                               y = rootMass,
                               by = c("domainID", "siteID", "plotID", "collectDate", "sampleID")) %>%
    dplyr::rename(coreRemarks = "remarks")
  
  coreMass <- dplyr::arrange(.data = coreMass,
                             .data$domainID,
                             .data$siteID,
                             .data$eventID,
                             .data$plotID,
                             .data$sampleID,
                             .data$sizeCategory)
  
  
  
  ### Calculate dilution sampling fragment mass, if applicable
  if (is.data.frame(rootDilution)) {
    
    rootDilution <- rootDilution %>%
      dplyr::filter(!is.na(.data$dryMass)) %>%
      dplyr::mutate(fragMass = round(.data$dryMass * (.data$sampleVolume / .data$dilutionSubsampleVolume),
                                     digits = 4),
                    .before = "dryMass") %>%
      dplyr::filter(.data$fragMass >= 0) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$collectDate,
                      .data$sampleID) %>%
      dplyr::summarise(dryMass = round(mean(.data$fragMass, na.rm = TRUE), digits = 4),
                       .groups = "drop") %>%
      dplyr::mutate(sizeCategory = "frag",
                    .after = "sampleID")
    
    #   Join rootDilution data with rootCore data
    coreDilMass <- dplyr::right_join(x = rootCore,
                                     y = rootDilution,
                                     by = c("domainID", "siteID", "plotID", "collectDate", "sampleID")) %>%
      dplyr::rename(coreRemarks = "remarks")
    
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
                                 names_from = "sizeCategory",
                                 values_from = "dryMass",
                                 names_prefix = "dryMass")
  
  #   Rename dryMass columns to remove hyphens (hyphens require special handling in column names)
  coreMass <- dplyr::rename(.data = coreMass,
                            dryMass1 = "dryMass0-1",
                            dryMass2 = "dryMass1-2",
                            dryMass10 = "dryMass2-10")
  
  #   Re-join collapsed strings for mycorrhizaeVisible and massRemarks columns
  coreMass <- dplyr::left_join(x = coreMass,
                               y = stringMassCols,
                               by = "sampleID")
  
  coreMass <- dplyr::relocate(.data = coreMass,
                              "mycorrhizaeVisible",
                              "massRemarks",
                              .after = "release")
  
  #   Conditionally rename fragment column if exists
  if (is.data.frame(rootDilution)) {
    
    coreMass <- dplyr::rename(.data = coreMass,
                              dryMassFrag = "dryMassfrag")
    
  }
  
  
  
  ### Conditionally calculate 'totalDryMass' dependent on 'includeFragInTotal' argument
  ##  Default: 'totalDryMass' does not include fragment mass
  if (isFALSE(includeFragInTotal)) {
    
    coreMass <- dplyr::rowwise(data = coreMass) %>%
      dplyr::mutate(totalDryMass = sum(.data$dryMass1, .data$dryMass2, .data$dryMass10, 
                                       na.rm = TRUE)) %>%
      dplyr::ungroup()
    
  } # end fragment FALSE conditional
  
  
  ##  Optional: Include fragment mass in totalDryMass
  if (isTRUE(includeFragInTotal)) {
    
    #   Check that inputDilution data frame is provided
    if (!is.data.frame(rootDilution)) {
      stop("A valid 'inputDilution' data frame must be provided when 'includeFragInTotal' is TRUE.")
    } 
    
    #   Sum totalDryMass WITH fragment mass
    coreMass <- dplyr::rowwise(data = coreMass) %>%
      dplyr::mutate(totalDryMass = sum(.data$dryMass1, .data$dryMass2, .data$dryMass10, .data$dryMassFrag,
                                       na.rm = TRUE)) %>%
      dplyr::ungroup()
    
  } # end fragment TRUE conditional
  
  
  
  ### Scale core-level dryMass values to "g/m2", "g/cm3", and "Mg/ha"
  coreMass <- dplyr::rowwise(data = coreMass) %>%
    dplyr::mutate(dryMass1_gm2 = round(.data$dryMass1 / .data$rootSampleArea, 
                                       digits = 1),
                  dryMass2_gm2 = round(.data$dryMass2 / .data$rootSampleArea, 
                                       digits = 1),
                  dryMass10_gm2 = round(.data$dryMass10 / .data$rootSampleArea, 
                                        digits = 1),
                  totalMass_gm2 = round(.data$totalDryMass / .data$rootSampleArea, 
                                        digits = 1)) %>%
    
    dplyr::mutate(dryMass1_gm3 = round(.data$dryMass1 / (.data$rootSampleArea * (.data$rootSampleDepth/100)), 
                                       digits = 1),
                  dryMass2_gm3 = round(.data$dryMass2 / (.data$rootSampleArea * (.data$rootSampleDepth/100)), 
                                       digits = 1),
                  dryMass10_gm3 = round(.data$dryMass10 / (.data$rootSampleArea * (.data$rootSampleDepth/100)), 
                                        digits = 1),
                  totalMass_gm3 = round(.data$totalDryMass / (.data$rootSampleArea * (.data$rootSampleDepth/100)), 
                                        digits = 1)) %>%
    
    dplyr::mutate(totalMass_Mgha = round(.data$totalMass_gm2 / 100,
                                         digits = 4)) %>%
    dplyr::relocate("totalMass_gm2",
                    .before = "totalMass_gm3") %>%
    dplyr::ungroup()
  
  
  
  ### Scale dryMass values from core to plot-level ("g/m2", "g/m3", and "Mg/ha")
  plotMass <- coreMass %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$eventID) %>%
    dplyr::summarise(startDate = min(collectDate),
                     endDate = max(collectDate),
                     dryMass1_gm2 = round(mean(dryMass1_gm2, na.rm = TRUE),
                                          digits = 1),
                     dryMass2_gm2 = round(mean(dryMass2_gm2, na.rm = TRUE),
                                          digits = 1),
                     dryMass10_gm2 = round(mean(dryMass10_gm2, na.rm = TRUE),
                                           digits = 1),
                     dryMass1_gm3 = round(mean(dryMass1_gm3, na.rm = TRUE),
                                          digits = 1),
                     dryMass2_gm3 = round(mean(dryMass2_gm3, na.rm = TRUE),
                                          digits = 1),
                     dryMass10_gm3 = round(mean(dryMass10_gm3, na.rm = TRUE),
                                           digits = 1),
                     totalMass_gm2 = round(mean(totalMass_gm2, na.rm = TRUE),
                                           digits = 1),
                     totalMass_gm3 = round(mean(totalMass_gm3, na.rm = TRUE),
                                           digits = 1),
                     totalMass_Mgha = round(mean(totalMass_Mgha, na.rm = TRUE),
                                            digits = 4),
                     coreNum = n(),
                     .groups = "drop")
  
  
  
  ### Scale total dryMass values from plot to site-level ("g/m2", "g/m3", "Mg/ha")
  siteMass <- plotMass %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$eventID) %>%
    dplyr::summarise(startDate = min(startDate),
                     endDate = max(endDate),
                     rootMassMean_gm2 = round(mean(totalMass_gm2, na.rm = TRUE),
                                              digits = 1),
                     rootMassSD_gm2 = round(sd(totalMass_gm2, na.rm = TRUE),
                                            digits = 0),
                     rootMassMean_gm3 = round(mean(totalMass_gm3, na.rm = TRUE),
                                              digits = 1),
                     rootMassSD_gm3 = round(sd(totalMass_gm3, na.rm = TRUE),
                                            digits = 0),
                     rootMassMean_Mgha = round(mean(totalMass_Mgha, na.rm = TRUE),
                                               digits = 4),
                     rootMassSD_Mgha = round(sd(totalMass_Mgha, na.rm = TRUE),
                                             digits = 2),
                     rootPlotNum = n(),
                     .groups = "drop")
  
  
  
  ### Return output
  output <- list(coreRootMass = coreMass,
                 plotRootMass = plotMass,
                 siteRootMass = siteMass)
  
  return(output)
  
} # end function