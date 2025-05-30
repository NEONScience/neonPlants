#' @title Join NEON aquatic plant clip harvest data into a single table with merged taxonomic identifications

#' @author Madaline Ritter \email{ritterm1@battelleecology.org} \cr

#' @description Join the 'apl_clipHarvest', 'apl_biomass', 'apl_taxonomyProcessed' and 'apc_morphospecies' tables to generate a single table that contains clip harvest data with taxonomic identifications for each sampleID. Data inputs are NEON Aquatic Plant Bryophyte Macroalgae Clip Harvest (DP1.20066.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations. 
#'
#' @details Input data may be provided either as a list or as individual tables. However, if both list and table inputs are provided at the same time the function will error out. For table joining to be successful, inputs must contain data from the same site x month combination(s) for all tables.
#' 
#' Only data from bout 2 (midsummer sampling) is returned in the joined output table, as other bouts do not include taxonomy data. If the input does not include any bout 2 data, the function will error out.
#' 
#' In the joined output table, the 'acceptedTaxonID' and associated taxonomic fields are populated from the first available identification in the following order: 'apl_taxonomyProcessed', 'apl_biomass', or 'apc_morphospecies'. For samples identified both in the field and by an expert taxonomist, the expert identification is retained in the output. A new field, 'taxonIDSourceTable', is included in the output and indicates the source table for each sample's identification.
#' 
#' @param inputDataList A list object comprised of Aquatic Plant Bryophyte Macroalgae Clip Harvest tables (DP1.20066.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided. [list]
#'
#' @param inputBio The 'apl_biomass' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputClip The 'apl_clipHarvest' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputTaxProc The 'apl_taxonomyProcessed' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputMorph The 'apc_morphospecies' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @return A table containing clip harvest data with all associated taxonomic information for each apl_clipHarvest record where targetTaxaPresent == 'Y'.
#' 
#' @examples
#' \dontrun{
#' #   Obtain NEON Aquatic Plant Point Count data
#' apl <- neonUtilities::loadByProduct(
#' dpID = "DP1.20066.001",
#' site = "all",
#' startdate = "2018-07",
#' enddate = "2018-08",
#' tabl = "all",
#' check.size = FALSE
#' )
#' 
#' #   Join downloaded root data
#' df <- neonPlants::joinAquClipHarvest(
#' inputDataList = apl,
#' inputBio = NA,
#' inputClip = NA,
#' inputTaxProc = NA,
#' inputMorph = NA
#' )
#'
#' }
#' 
#' @export joinClipHarvest



joinAquClipHarvest <- function(inputDataList,
                              inputBio = NA,
                              inputClip = NA,
                              inputTaxProc = NA,
                              inputMorph = NA) {
  
  ### Test that user has supplied arguments as required by function ####
  
  ### Verify user-supplied inputDataList object contains correct data if not NA
  if (!missing(inputDataList)) {
    
    #   Check that input is a list
    if (!inherits(inputDataList, "list")) {
      stop(glue::glue("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"))
    }
    
    #   Check that required tables within list match expected names
    listExpNames <- c("apl_biomass", "apl_clipHarvest")
    
    #   Determine dataType or stop with appropriate message
    if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from 'inputDataList':",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
    }
  } else {
    
    inputDataList <- NULL
    
  } # end missing conditional
  
  
  ### Verify table inputs are NA if inputDataList is supplied
  if (inherits(inputDataList, "list") & (!is.logical(inputBio) | !is.logical(inputClip) | !is.logical(inputTaxProc) | !is.logical(inputMorph))) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }
  
  
  ### Verify all table inputs are data frames if inputDataList is NA
  if (is.null(inputDataList) & 
      (!inherits(inputBio, "data.frame") | !inherits(inputClip, "data.frame") 
       # | !inherits(inputTaxProc, "data.frame") | !inherits(inputMorph, "data.frame")
       )) {
    
    stop("Data frames must be supplied for table inputs if 'inputDataList' is missing")
    
  }
  
  
  ### Conditionally define input tables ####
  if (inherits(inputDataList, "list")) {
    
    apBio <- inputDataList$apl_biomass
    apClip <- inputDataList$apl_clipHarvest
    apTaxProc <- inputDataList$apl_taxonomyProcessed
    apMorph <- inputDataList$apc_morphospecies
      
  } else {
    
    apBio <- inputBio
    apClip <- inputClip
    apTaxProc <- inputTaxProc
    apMorph <- inputMorph
    
  }
  
  
  
  ### Verify input tables contain required columns and data ####
  
  ### Verify 'apBio' table contains required data
  #   Check for required columns
  # bioExpCols <- c("sampleID", "taxonID", "scientificName", "morphospeciesID")
  bioExpCols <- c("sampleID", "taxonID", "scientificName", "morphospeciesID", "identifiedDate", "sampleCondition", "identificationHistoryID", "dataQF", "publicationDate", "release", "division", "class", "order",  "family", "genus", "section", "specificEpithet", "scientificNameAuthorship", "identificationQualifier", "identificationReferences", "remarks", "identifiedBy", "uid")
  
  
  if (length(setdiff(bioExpCols, colnames(apBio))) > 0) {
    stop(glue::glue("Required columns missing from 'inputBio':", '{paste(setdiff(bioExpCols, colnames(apBio)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(apBio) == 0) {
    stop(glue::glue("Table 'inputBio' has no data."))
  }
  
  
  
  ### Verify 'apClip' table contains required data
  #   Check for required columns
  clipExpCols <- c("namedLocation", "eventID", "boutNumber", "fieldID", "benthicArea", "domainID", "siteID", "startDate", "collectDate", "fieldIDCode", "recordedBy", "remarks")
  
  if (length(setdiff(clipExpCols, colnames(apClip))) > 0) {
    stop(glue::glue("Required columns missing from 'inputClip':", '{paste(setdiff(clipExpCols, colnames(apClip)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(apClip) == 0) {
    stop(glue::glue("Table 'inputClip' has no data."))
  }
  
  #   Check for bout 2 data
  if (nrow(apClip%>%filter(boutNumber == '2')) == 0){
    stop(glue::glue("The input data does not contain any bout 2 records. No taxonomy data to join."))
  }
  
  
  ### Verify 'apTaxProc' table contains required data if data exists
  taxProcExpCols <- c("sampleID", "taxonID", "identifiedDate", "sampleCondition", "identificationHistoryID", "dataQF", "publicationDate", "release", "division", "class", "order", "family", "genus", "section", "specificEpithet", "scientificNameAuthorship", "identificationQualifier", "identificationReferences", "remarks", "identifiedBy", "morphospeciesID", "uid", "domainID", "siteID", "namedLocation", "collectDate", "sampleCode")
  
  #   Check for data
  if(exists("apTaxProc")){
    if (is.null(apTaxProc) | nrow(apTaxProc) == 0) {
      message(glue::glue("Warning: Table 'inputTaxProc' has no data. Join will not include processed taxonomy data."))
    } else {
      
      #   Check for required columns if data exists
      if (length(setdiff(taxProcExpCols, colnames(apTaxProc))) > 0) {
        stop(glue::glue("Required columns missing from 'inputTaxProc':", '{paste(setdiff(taxProcExpCols, colnames(apTaxProc)), collapse = ", ")}',
                        .sep = " "))
      }
    }
  }
  
  
  ### Verify 'apMorph' table contains required data if data exists
  morphExpCols <- c("morphospeciesID", "taxonID", "scientificName", "identificationQualifier","identificationReferences", "identifiedBy", "dataQF" )
  
  
  #   Check for data
  if(exists("apMorph")){
    if(is.null(apMorph) | nrow(apMorph) == 0){
      message("Warning: Table 'inputMorph' has no data. Joined output does not include identifications from the morphospecies table.")
    } else {
      
      #   Check for required columns if data exists
      if (length(setdiff(morphExpCols, colnames(apMorph))) > 0) {
        stop(glue::glue("Required columns missing from 'inputMorph':", '{paste(setdiff(morphExpCols, colnames(apMorph)), collapse = ", ")}',
                        .sep = " "))
      }
    }
  }
  
  ### Verify that 'apClip' table contains bout 2 data
  
  ### Join apBio and apTaxProc tables using sampleID
  
  if (!is.null(apTaxProc) && nrow(apTaxProc) > 0) {
    # message("Join taxonomyProcessed taxonomic identifications.")
    #   Select needed columns from apTaxProc
    apTaxProc <- apTaxProc %>% 
      dplyr::select(-"uid", -"domainID", -"siteID", -"namedLocation", -"collectDate", -"morphospeciesID", -"sampleCode")
    
    #   Update expert taxonomist identifications
    apJoin1 <- apBio %>% 
      dplyr::left_join(apTaxProc, by = "sampleID", suffix = c("_bio", "_taxProc"), relationship = "many-to-many") %>% 
      dplyr::mutate(
        
        identifiedDate = dplyr::case_when(
          !is.na(.data$taxonID_taxProc) ~ .data$identifiedDate_taxProc,
          TRUE ~ .data$identifiedDate_bio),
        
        sampleCondition = dplyr::case_when(
          !is.na(.data$sampleCondition_bio)&!is.na(.data$sampleCondition_taxProc)~paste0("biomass ",.data$sampleCondition_bio," | taxonProcessed ",.data$sampleCondition_taxProc),
          !is.na(.data$sampleCondition_bio)&is.na(.data$sampleCondition_taxProc)~paste0("biomass ",.data$sampleCondition_bio),
          is.na(.data$sampleCondition_bio)&!is.na(.data$sampleCondition_taxProc)~paste0("taxonProcessed ",.data$sampleCondition_taxProc),
          TRUE ~ NA),
        
        taxonIDSourceTable = dplyr::case_when(
          !is.na(.data$taxonID_taxProc)~"apl_taxonomyProcessed",
          is.na(.data$taxonID_taxProc)&!is.na(.data$taxonID_bio)~"apl_biomass",
          TRUE~NA),
        
        tempTaxonID = dplyr::if_else(!is.na(taxonID_taxProc), taxonID_taxProc, taxonID_bio),
        
        scientificName = dplyr::if_else(!is.na(taxonID_taxProc), scientificName_taxProc, scientificName_bio),
        
        identificationHistoryID = dplyr::case_when(
          !is.na(.data$identificationHistoryID_bio)&!is.na(.data$identificationHistoryID_taxProc)~paste0(.data$identificationHistoryID_bio," | ",.data$identificationHistoryID_taxProc),
          is.na(.data$identificationHistoryID_taxProc)&!is.na(.data$identificationHistoryID_bio)~.data$identificationHistoryID_bio,
          !is.na(.data$identificationHistoryID_taxProc)&is.na(.data$identificationHistoryID_bio)~.data$identificationHistoryID_taxProc,
          TRUE~NA),
        
        biomassDataQF = dataQF_bio,
        taxProcessedDataQF = dataQF_taxProc,
        
        biomassPublicationDate = publicationDate_bio,
        taxProcessedPublicationDate = publicationDate_taxProc,
        
        biomassRelease = release_bio,
        taxProcessedRelease = release_taxProc,
        
        # phylum = dplyr::if_else(!is.na(taxonID_taxProc), phylum_taxProc, phylum_bio),
        division = dplyr::if_else(!is.na(taxonID_taxProc), division_taxProc, division_bio),
        class = dplyr::if_else(!is.na(taxonID_taxProc), class_taxProc, class_bio),
        order = dplyr::if_else(!is.na(taxonID_taxProc), order_taxProc, order_bio),
        family = dplyr::if_else(!is.na(taxonID_taxProc), family_taxProc, family_bio), 
        genus = dplyr::if_else(!is.na(taxonID_taxProc), genus_taxProc, genus_bio), 
        section = dplyr::if_else(!is.na(taxonID_taxProc), section_taxProc, section_bio),
        specificEpithet = dplyr::if_else(!is.na(taxonID_taxProc), specificEpithet_taxProc, specificEpithet_bio), 
        # infraspecificEpithet = dplyr::if_else(!is.na(taxonID_taxProc), infraspecificEpithet_taxProc, infraspecificEpithet_bio), 
        # variety = dplyr::if_else(!is.na(taxonID_taxProc), variety_taxProc, variety_bio),
        # form = dplyr::if_else(!is.na(taxonID_taxProc), form_taxProc, form_bio), 
        scientificNameAuthorship = dplyr::if_else(!is.na(taxonID_taxProc), scientificNameAuthorship_taxProc, scientificNameAuthorship_bio), 
        identificationQualifier = dplyr::if_else(!is.na(taxonID_taxProc), identificationQualifier_taxProc, identificationQualifier_bio),
        identificationReferences = dplyr::if_else(!is.na(taxonID_taxProc), identificationReferences_taxProc, identificationReferences_bio), 
        taxonRank = dplyr::if_else(!is.na(taxonID_taxProc), taxonRank_taxProc, taxonRank_bio),
        
        remarks = dplyr::case_when(
          !is.na(.data$remarks_bio)&!is.na(.data$remarks_taxProc) ~ paste0("biomass remarks - ", .data$remarks_bio, " | taxonProcessed remarks - ", .data$remarks_taxProc ),
          is.na(.data$remarks_taxProc)&!is.na(.data$remarks_bio) ~ paste0("biomass remarks - ", .data$remarks_bio),
          !is.na(.data$remarks_taxProc)&is.na(.data$remarks_bio) ~ paste0("taxonProcessed remarks - ", .data$remarks_taxProc),
          TRUE ~ NA),
        
        identifiedBy = dplyr::if_else(!is.na(taxonID_taxProc), identifiedBy_taxProc, identifiedBy_bio),
        identifiedDate = dplyr::if_else(!is.na(identifiedDate_taxProc), identifiedDate_taxProc, identifiedDate_bio)
        
      ) %>% 
      select(-matches("_taxProc"), 
             -matches("_bio"),
             -"targetTaxaPresent", -"uid")
    
  } else {
    # message("No data joined from apl_taxonomyProcessed table.")
    # rename columns if no taxProc join
    apJoin1 <- apBio %>% 
      mutate(tempTaxonID = taxonID,
             remarks = dplyr::if_else(is.na(remarks), NA, paste0("biomass remarks - ", remarks)),
             perTaxonRelease = release,
             taxonIDSourceTable = dplyr::if_else(is.na(taxonID), NA, "apl_biomass"),
             perTaxonDataQF = dataQF,
             perTaxonPublicationDate = publicationDate) %>% 
      select(-"taxonID", -"release", -"dataQF", -"publicationDate", -"uid")
  } 
  
  ### Join apJoin1 and apMorph tables
  
  #   Select needed columns from apMorph
  if(!is.null(apMorph) && nrow(apMorph) > 0){
    # message("Join morphospecies taxonomic identifications.")
    apMorph <- apMorph %>%
      dplyr::select("taxonID",
                    "scientificName",
                    "morphospeciesID",
                    "identificationQualifier",
                    "identificationReferences",
                    "identifiedBy",
                    # "identifiedDate",
                    "dataQF"
      )
    
    # Update morphospecies taxon identifications
    apJoin2 <- apJoin1 %>%
      dplyr::mutate(
        morphospeciesID = dplyr::if_else(!is.na(morphospeciesID), paste0(morphospeciesID, ".", substr(collectDate, 1, 4)), morphospeciesID)
      ) %>% 
      dplyr::left_join(apMorph, by = "morphospeciesID", suffix = c("_bio", "_morph")) %>% 
      # filter(!is.na(taxonID) & acceptedTaxonID %in% c('2PLANT', 'UNKALG')) %>% 
      dplyr::mutate(
        taxonIDSourceTable = dplyr::if_else(!is.na(taxonID) & tempTaxonID %in% c('2PLANT', 'UNKALG'), "apc_morphospecies", taxonIDSourceTable),
        acceptedTaxonID = dplyr::if_else(!is.na(taxonID) & tempTaxonID %in% c('2PLANT', 'UNKALG'), taxonID, tempTaxonID),
        scientificName = dplyr::if_else( !is.na(taxonID) & tempTaxonID %in% c('2PLANT', 'UNKALG'), scientificName_morph, scientificName_bio),
        identificationQualifier = dplyr::if_else( !is.na(taxonID) & tempTaxonID %in% c('2PLANT', 'UNKALG'), identificationQualifier_morph, identificationQualifier_bio),
        identificationReferences = dplyr::if_else( !is.na(taxonID) & tempTaxonID %in% c('2PLANT', 'UNKALG'), identificationReferences_morph, identificationReferences_bio),
        identifiedBy = dplyr::if_else( !is.na(taxonID) & tempTaxonID %in% c('2PLANT', 'UNKALG'), identifiedBy_morph, identifiedBy_bio),
        identifiedDate = NA, #not currently in pub table
        morphospeciesDataQF = dataQF
        
      ) %>% 
      dplyr::select(
        -"taxonID", -"tempTaxonID", -"dataQF",
        -matches("_morph"), 
        -matches("_bio")
      )
  } else {
    # message("No data joined from apc_morphospecies table.")
    apJoin2 <- apJoin1 %>% 
      mutate(acceptedTaxonID = tempTaxonID) %>% 
        select(-"tempTaxonID")
  }
  
  
  ### Join apClip and apBio tables 
  
  #  Update morphospecies taxon identifications
  joinClipHarvest <- apClip %>%
    dplyr::select(-"benthicArea", -"namedLocation", -"domainID", -"siteID",  -"startDate", -"collectDate", -"fieldIDCode" ) %>% 
    dplyr::left_join(apJoin2, by = "fieldID", suffix = c("_clip", "_bio")) %>% 
    dplyr::mutate(
      remarks = dplyr::case_when(
        !is.na(.data$remarks_bio)&!is.na(.data$remarks_clip) ~ paste0("clipHarvest remarks - ", .data$remarks_clip, " | ", .data$remarks_bio),
        !is.na(.data$remarks_bio)&is.na(.data$remarks_clip) ~ .data$remarks_bio,
        is.na(.data$remarks_bio)&!is.na(.data$remarks_clip) ~ paste0("clipHarvest remarks - ", .data$remarks_clip),
        TRUE ~ NA),
      recordedBy = dplyr::if_else(!is.na(.data$recordedBy_clip), .data$recordedBy_clip, .data$recordedBy_bio),
      clipDataQF = .data$dataQF,
      clipPublicationDate = publicationDate,
      clipRelease = release
    ) %>% 
    dplyr::select(
      -"dataQF", -"publicationDate", -"release",
      -matches("_bio"),
      -matches("_clip")
    )
  
  
  #  Filter out bout 1 and 3 data
  joinClipHarvest <- joinClipHarvest %>% filter(boutNumber == '2')
  
  
  return(joinClipHarvest)
  
} #function closer
