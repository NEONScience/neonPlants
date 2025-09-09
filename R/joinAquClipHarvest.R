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
#' A single sampleID in 'apl_biomass' may correspond to more than one taxa in 'apl_taxonomyProcessed'. When tables are joined, the taxon with the greatest 'algalParameterValue' in 'apl_taxonomyProcessed' will be listed as the 'acceptedTaxonID' and a new field, 'additionalTaxa', is included in the output and includes all other taxa associated with the sampleID. If more than one taxon shares the same max 'algalParameterValue', the first row in the input table is returned as the 'acceptedTaxonID'. Detailed taxonomic information for any additionalTaxa, including algalParameterValues, can be found in the input apl_taxonomyProcessed table.
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
#' @return A table containing bout 2 clip harvest data with all associated taxonomic information for each apl_clipHarvest record where targetTaxaPresent = 'Y' and an identification has been published..
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' 
#' @examples
#' \dontrun{
#' #   Obtain NEON Aquatic Plant Clip Harvest data
#' apl <- neonUtilities::loadByProduct(
#' dpID = "DP1.20066.001",
#' site = "all",
#' startdate = "2018-07",
#' enddate = "2018-08",
#' tabl = "all",
#' check.size = FALSE
#' )
#' 
#' #   Join downloaded clip harvest data
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
#' @export joinAquClipHarvest


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
      stop(
        glue::glue(
          "Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"
        )
      )
    }
    
    #   Check that required tables within list match expected names
    listExpNames <- c("apl_biomass", "apl_clipHarvest")
    
    #   Determine dataType or stop with appropriate message
    if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(
        glue::glue(
          "Required tables missing from 'inputDataList':",
          '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
          .sep = " "
        )
      )
    }
  } else {
    
    inputDataList <- NULL
    
  } # end missing conditional
  
  
  ### Verify table inputs are NA if inputDataList is supplied
  if (!is.null(inputDataList)) {
    if (!isTRUE(is.na(inputBio)) || !isTRUE(is.na(inputClip)) ||
        !isTRUE(is.na(inputTaxProc)) || !isTRUE(is.na(inputMorph))) {
      stop("When 'inputDataList' is supplied, all table input arguments must be NA.")
    }
  }
  
  
  ### Verify all table inputs are data frames if inputDataList is NA
  if (is.null(inputDataList) &
      (
        !is.data.frame(inputBio) || !is.data.frame(inputClip)
      )) {
    stop("Data frames must be supplied for table inputs if 'inputDataList' is missing")
    
  }
  
  
  ### Conditionally define input tables ####
  if (inherits(inputDataList, "list")) {
    apBio <- inputDataList$apl_biomass
    apClip <- inputDataList$apl_clipHarvest
    if (!is.null(inputDataList$apl_taxonomyProcessed)) {
      apTaxProc <- inputDataList$apl_taxonomyProcessed
    } else{
      apTaxProc <- NA
    }
    if (!is.null(inputDataList$apc_morphospecies)) {
      apMorph <- inputDataList$apc_morphospecies
    } else{
      apMorph <- NA
    }
    
  } else {
    apBio <- inputBio
    apClip <- inputClip
    apTaxProc <- inputTaxProc
    apMorph <- inputMorph
    
  }
  
  
  
  ### Verify input tables contain required columns and data ####
  
  ### Verify 'apBio' table contains required data
  #   Check for required columns
  bioExpCols <- c(
    "sampleID", "taxonID", "scientificName", "morphospeciesID", "identifiedDate",
    "sampleCondition", "identificationHistoryID", "dataQF", "publicationDate",
    "release", "division", "class", "order", "family", "genus",  "section",
    "specificEpithet", "scientificNameAuthorship", "identificationQualifier",
    "identificationReferences", "remarks", "identifiedBy", "uid"
  )
  
  
  if (length(setdiff(bioExpCols, colnames(apBio))) > 0) {
    stop(
      glue::glue(
        "Required columns missing from 'inputBio':",
        '{paste(setdiff(bioExpCols, colnames(apBio)), collapse = ", ")}',
        .sep = " "
      )
    )
  }
  
  #   Check for data
  if (nrow(apBio) == 0) {
    stop(glue::glue("Table 'inputBio' has no data."))
  }
  
  
  
  ### Verify 'apClip' table contains required data
  #   Check for required columns
  clipExpCols <- c(
    "namedLocation", "eventID", "boutNumber", "fieldID", "benthicArea", "domainID",
    "siteID", "startDate", "collectDate", "fieldIDCode", "recordedBy", "remarks"
  )
  
  if (length(setdiff(clipExpCols, colnames(apClip))) > 0) {
    stop(
      glue::glue(
        "Required columns missing from 'inputClip':",
        '{paste(setdiff(clipExpCols, colnames(apClip)), collapse = ", ")}',
        .sep = " "
      )
    )
  }
  
  #   Check for data
  if (nrow(apClip) == 0) {
    stop(glue::glue("Table 'inputClip' has no data."))
  }
  
  #   Check for bout 2 data
  if (nrow(apClip %>% dplyr::filter(.data$boutNumber == '2')) == 0) {
    stop(
      glue::glue(
        "The input data does not contain any bout 2 records. No taxonomy data to join."
      )
    )
  }
  
  
  ### Verify 'apTaxProc' table contains required data if data exists
  taxProcExpCols <- c(
    "sampleID", "taxonID", "identifiedDate", "sampleCondition", 
    "identificationHistoryID", "dataQF", "publicationDate", "release", 
    "division", "class", "order", "family", "genus", "section", "specificEpithet",
    "scientificNameAuthorship", "identificationQualifier", 
    "identificationReferences", "remarks", "identifiedBy", "morphospeciesID", 
    "uid", "domainID", "siteID", "namedLocation", "collectDate", "sampleCode"
  )
  
  #   Check for data
  if (is.data.frame(apTaxProc)) {
    if(nrow(apTaxProc) == 0) {
      message(
        glue::glue(
          "Warning: Table 'inputTaxProc' has no data. Join will not include processed taxonomy data."
        )
      )
    } else {
      #   Check for required columns if data exists
      if (length(setdiff(taxProcExpCols, colnames(apTaxProc))) > 0) {
        stop(
          glue::glue(
            "Required columns missing from 'inputTaxProc':",
            '{paste(setdiff(taxProcExpCols, colnames(apTaxProc)), collapse = ", ")}',
            .sep = " "
          )
        )
      }
    }
  }
  
  
  ### Verify 'apMorph' table contains required data if data exists
  morphExpCols <- c(
    "morphospeciesID", "taxonID", "scientificName", "identificationQualifier",
    "identificationReferences", "identifiedBy", "dataQF"
  )
  
  
  #   Check for data
  if (is.data.frame(apMorph)){
    if(nrow(apMorph) == 0) {
      message(
        "Warning: Table 'inputMorph' has no data. Joined output does not include identifications from the morphospecies table."
      )
    } else {
      #   Check for required columns if data exists
      if (length(setdiff(morphExpCols, colnames(apMorph))) > 0) {
        stop(
          glue::glue(
            "Required columns missing from 'inputMorph':",
            '{paste(setdiff(morphExpCols, colnames(apMorph)), collapse = ", ")}',
            .sep = " "
          )
        )
      }
    }
  }
  
  
  
  ### Join apBio and apTaxProc tables using sampleID ####
  
  if (is.data.frame(apTaxProc) && nrow(apTaxProc) > 0) {
    
    #   Select needed columns from apTaxProc
    apTaxProc <- apTaxProc %>%
      dplyr::select(
        -"uid",
        -"domainID",
        -"siteID",
        -"namedLocation",
        -"collectDate",
        -"morphospeciesID",
        -"sampleCode"
      ) %>%
      dplyr::mutate(algalParameterValue = as.numeric(algalParameterValue))
      # dplyr::mutate(identifiedDate = as.character(identifiedDate)) #biomass identifiedDate is character, not date
    
    #   Preprocess apTaxProc to determine primary/additional taxa per sampleID
    apTaxProc_main <- apTaxProc %>%
      group_by(sampleID) %>%
      slice_max(order_by = algalParameterValue, n = 1, with_ties = FALSE) %>% #select first row if max algalParameterValue is shared between more than one sampleID
      ungroup()
    
    apTaxProc_additional <- apTaxProc %>%
      group_by(sampleID) %>%
      arrange(desc(algalParameterValue)) %>%
      summarise(additionalTaxa = if(length(taxonID) > 1) paste(taxonID[-1], collapse = "|") else NA_character_)
    
    apTaxProc_main <- apTaxProc_main %>%
      left_join(apTaxProc_additional, by = "sampleID")
    
    #   Columns conditionally replaced with taxProc data
    join1_cols <- c(
      "division", "class", "order", "family",
      "genus", "section", "specificEpithet",
      "scientificNameAuthorship", "identificationQualifier", "identificationReferences",
      "taxonRank", "identifiedBy", "identifiedDate"
    )
    
    #   Update expert taxonomist identifications
    apJoin1 <- apBio %>%
      dplyr::left_join(
        apTaxProc_main,
        by = "sampleID",
        suffix = c("_bio", "_taxProc")#,
        # relationship = "many-to-many"
      ) %>%
      dplyr::mutate(
        
        sampleCondition = dplyr::case_when(
          !is.na(.data$sampleCondition_bio) & !is.na(.data$sampleCondition_taxProc) ~ 
            paste0("biomass ", .data$sampleCondition_bio," | taxProcessed ", .data$sampleCondition_taxProc),
          !is.na(.data$sampleCondition_bio) & is.na(.data$sampleCondition_taxProc) ~ 
            paste0("biomass ", .data$sampleCondition_bio),
          is.na(.data$sampleCondition_bio) & !is.na(.data$sampleCondition_taxProc) ~ 
            paste0("taxProcessed ", .data$sampleCondition_taxProc),
          TRUE ~ NA
        ),
        
        taxonIDSourceTable = dplyr::case_when(
          !is.na(.data$taxonID_taxProc) ~ "apl_taxonomyProcessed",
          is.na(.data$taxonID_taxProc) & !is.na(.data$taxonID_bio) ~ "apl_biomass",
          TRUE ~ NA
        ),
        
        tempTaxonID = dplyr::if_else(
          !is.na(.data$taxonID_taxProc), .data$taxonID_taxProc, .data$taxonID_bio
        ),
        
        scientificName = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$scientificName_taxProc,
          .data$scientificName_bio
        ),
        
        identificationHistoryID = dplyr::case_when(
          !is.na(.data$identificationHistoryID_bio) & !is.na(.data$identificationHistoryID_taxProc) ~
            paste0(.data$identificationHistoryID_bio," | ",.data$identificationHistoryID_taxProc),
          is.na(.data$identificationHistoryID_taxProc) & !is.na(.data$identificationHistoryID_bio) ~
            .data$identificationHistoryID_bio,
          !is.na(.data$identificationHistoryID_taxProc) & is.na(.data$identificationHistoryID_bio) ~
            .data$identificationHistoryID_taxProc,
          TRUE ~ NA
        ),
        
        biomassDataQF = .data$dataQF_bio,
        taxProcessedDataQF = .data$dataQF_taxProc,
        biomassPublicationDate = .data$publicationDate_bio,
        taxProcessedPublicationDate = .data$publicationDate_taxProc,
        biomassRelease = .data$release_bio,
        taxProcessedRelease = .data$release_taxProc,
        
        remarks = dplyr::case_when(
          !is.na(.data$remarks_bio) & !is.na(.data$remarks_taxProc) ~ 
            paste0( "biomass remarks - ", .data$remarks_bio, " | taxProcessed remarks - ",  .data$remarks_taxProc),
          is.na(.data$remarks_taxProc) & !is.na(.data$remarks_bio) ~ 
            paste0("biomass remarks - ", .data$remarks_bio),
          !is.na(.data$remarks_taxProc) & is.na(.data$remarks_bio) ~ 
            paste0("taxProcessed remarks - ", .data$remarks_taxProc),
          TRUE ~ NA
        )
      )
    
    for (col in join1_cols) {
      taxProc_col <- paste0(col, "_taxProc")
      bio_col <- paste0(col, "_bio")
      apJoin1[[col]] <- dplyr::if_else(
        !is.na(apJoin1$taxonID_taxProc),
        if (taxProc_col %in% names(apJoin1)) as.character(apJoin1[[taxProc_col]]) else NA_character_,
        if (bio_col %in% names(apJoin1)) as.character(apJoin1[[bio_col]]) else NA_character_
      )
    }
    apJoin1 <- apJoin1 %>%
      dplyr::select(-"uid", -"targetTaxaPresent",
                    -dplyr::matches("_taxProc"),-dplyr::matches("_bio"))
  
    
  } else {
    message("No data joined from apl_taxonomyProcessed table.")
    # rename columns if no taxProc join
    apJoin1 <- apBio %>%
      dplyr::mutate(
        tempTaxonID = .data$taxonID,
        remarks = dplyr::if_else(is.na(.data$remarks), NA, paste0("biomass remarks - ", .data$remarks)),
        perTaxonRelease = .data$release,
        taxonIDSourceTable = dplyr::if_else(is.na(.data$taxonID), NA, "apl_biomass"),
        perTaxonDataQF = .data$dataQF,
        perTaxonPublicationDate = .data$publicationDate
      ) %>%
      dplyr::select(-"taxonID",
             -"release",
             -"dataQF",
             -"publicationDate",
             -"uid")
  }
  
  
  
  
  
  ### Join apJoin1 and apMorph tables ####
  
  #   Select needed columns from apMorph
  if (is.data.frame(apMorph) && nrow(apMorph) > 0) {
    # message("Join morphospecies taxonomic identifications.")
    apMorph <- apMorph %>%
      dplyr::select(
        "taxonID", "scientificName", "morphospeciesID", "identificationQualifier",
        "identificationReferences", "identifiedBy", "morphospeciesResolvedDate",
        ## Uncomment next two lines once morph table has been updated
        # "phylum", "division", "class", "order", "family", "genus", "section", 
        # "specificEpithet", "infraspecificEpithet", "variety", "form", "taxonRank",
        "dataQF"
      )%>% 
      dplyr::rename(identifiedDate="morphospeciesResolvedDate")
    
    # Update morphospecies taxon identifications
    apJoin2 <- apJoin1 %>%
      dplyr::mutate(
        morphospeciesID = dplyr::if_else(
          !is.na(.data$morphospeciesID),
          paste0(.data$morphospeciesID, ".", substr(.data$collectDate, 1, 4)),
          .data$morphospeciesID
        )
      ) %>%
      dplyr::left_join(apMorph, by="morphospeciesID", suffix=c("_bio","_morph")) %>%
      dplyr::mutate(
        taxonIDSourceTable = dplyr::if_else(
          !is.na(.data$taxonID) & .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          "apc_morphospecies", .data$taxonIDSourceTable),
        
        acceptedTaxonID = dplyr::if_else(
          !is.na(.data$taxonID) & .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          .data$taxonID, .data$tempTaxonID),
        
        morphospeciesDataQF = .data$dataQF
      )
    
    #   Columns conditionally replaced with morph data
    join2_cols <- c(
      "scientificName", "identificationQualifier", "identificationReferences",
      "identifiedBy", "identifiedDate"
      ## Uncomment next two lines once morph table has been updated
      # , "phylum", "division", "class", "order",
      # "family", "genus", "section", "specificEpithet", "infraspecificEpithet",
      # "variety", "form", "taxonRank"
    )
    
    for (col in join2_cols) {
      morph_col <- paste0(col, "_morph")
      bio_col <- paste0(col, "_bio")
      apJoin2[[col]] <- dplyr::if_else(
        !is.na(apJoin2$taxonID) & apJoin2$tempTaxonID %in% c("2PLANT", "UNKALG"),
        if (morph_col %in% names(apJoin2)) as.character(apJoin2[[morph_col]]) else NA_character_,
        if (bio_col %in% names(apJoin2)) as.character(apJoin2[[bio_col]]) else NA_character_
      )
    }
    
    apJoin2 <- apJoin2 %>%
      dplyr::select(
        -"taxonID", -"tempTaxonID", -"dataQF",
        -dplyr::matches("_morph"),-dplyr::matches("_bio"))


  } else {
    message("No data joined from apc_morphospecies table.")
    
    apJoin2 <- apJoin1 %>%
      dplyr::mutate(acceptedTaxonID = .data$tempTaxonID) %>%
      dplyr::select(-"tempTaxonID")
  }
  
  
  
  
  
  ### Join apClip and apBio tables ####
  
  joinClipHarvest <- apClip %>%
    dplyr::select(
      -"benthicArea", -"namedLocation", -"domainID", -"siteID",
      -"startDate", -"collectDate", -"fieldIDCode"
    ) %>%
    dplyr::left_join(apJoin2, by = "fieldID", suffix = c("_clip", "_bio")) %>%
    dplyr::mutate(
      remarks = dplyr::case_when(
        !is.na(.data$remarks_bio) &
          !is.na(.data$remarks_clip) ~ paste0(
            "clipHarvest remarks - ",
            .data$remarks_clip,
            " | ",
            .data$remarks_bio
          ),!is.na(.data$remarks_bio) &
          is.na(.data$remarks_clip) ~ .data$remarks_bio,
        is.na(.data$remarks_bio) &
          !is.na(.data$remarks_clip) ~ paste0("clipHarvest remarks - ", .data$remarks_clip),
        TRUE ~ NA
      ),
      recordedBy = dplyr::if_else(
        !is.na(.data$recordedBy_clip),
        .data$recordedBy_clip,
        .data$recordedBy_bio
      ),
      clipDataQF = .data$dataQF,
      clipPublicationDate = .data$publicationDate,
      clipRelease = .data$release
    ) %>%
    dplyr::select(-"dataQF",
                  -"publicationDate",
                  -"release",-dplyr::matches("_bio"),-dplyr::matches("_clip"))
  
  
  ###  Filter out bout 1 and 3 data ####
  joinClipHarvest <- joinClipHarvest %>% dplyr::filter(.data$boutNumber == '2')
  
  ###  Re-format date columns ####
  joinClipHarvest$processingDate <- as.Date(joinClipHarvest$processingDate)
  
  joinClipHarvest$identifiedDate <- as.Date(joinClipHarvest$identifiedDate)
  
  joinClipHarvest$collectDate <- as.POSIXct(joinClipHarvest$collectDate, 
    format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  
  joinClipHarvest$startDate <- as.POSIXct(joinClipHarvest$startDate,
    format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  
  joinClipHarvest$biomassPublicationDate <- as.POSIXct(joinClipHarvest$biomassPublicationDate,
    format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  
  joinClipHarvest$clipPublicationDate <- as.POSIXct(joinClipHarvest$clipPublicationDate,
    format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  
  if (is.data.frame(apTaxProc) && nrow(apTaxProc) > 0){
    joinClipHarvest$taxProcessedPublicationDate <- as.POSIXct(joinClipHarvest$taxProcessedPublicationDate,
                                                              format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  }
  
  return(joinClipHarvest)
  
} #function closer
