#' @title Join NEON aquatic plant point count data into a single table with merged taxonomic identifications

#' @author Madaline Ritter \email{ritterm1@battelleecology.org} \cr

#' @description Join the 'apc_pointTransect', 'apc_perTaxon', 'apc_taxonomyProcessed' and 'apc_morphospecies' tables to generate a single table that contains point count data with taxonomic identifications for each sampleID. Data inputs are NEON Aquatic Plant, Bryophyte, Lichen, and Macroalgae Point Counts in Wadeable Streams (DP1.20072.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations. 
#'
#' @details Input data may be provided either as a list or as individual tables. However, if both list and table inputs are provided at the same time the function will error out. For table joining to be successful, inputs must contain data from the same site x month combination(s) for all tables.
#' 
#' In the joined output table, the 'acceptedTaxonID' and associated taxonomic fields are populated from the first available identification in the following order: 'apc_taxonomyProcessed', 'apc_perTaxon', or 'apc_morphospecies'. For samples identified both in the field and by an expert taxonomist, the expert identification is retained in the output. A new field, 'taxonIDSourceTable', is included in the output and indicates the source table for each sample's identification.
#' 
#' If a single sample in 'apc_taxonomyProcessed' contains multiple macroalgae species, each species will be represented as a separate row in 'apc_pointTransect' for every point associated with that sampleID.
#' 
#' @param inputDataList A list object comprised of Aquatic Plant, Bryophyte, Lichen, and Macroalgae Point Count tables (DP1.20072.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided. [list]
#'
#' @param inputPoint The 'apc_pointTransect' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputPerTax The 'apc_perTaxon' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputTaxProc The 'apc_taxonomyProcessed' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputMorph The 'apc_morphospecies' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @return A table containing point transect data with all associated taxonomic information for each point where targetTaxaPresent == 'Y'.
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' 
#' @examples
#' \dontrun{
#' #   Obtain NEON Aquatic Plant Point Count data
#' apc <- neonUtilities::loadByProduct(
#' dpID = "DP1.20072.001",
#' site = "all",
#' startdate = "2018-07",
#' enddate = "2018-08",
#' tabl = "all",
#' check.size = FALSE
#' )
#' 
#' #   Join downloaded point count data
#' df <- neonPlants::joinAquPointCount(
#' inputDataList = apc,
#' inputPoint = NA,
#' inputPerTax = NA,
#' inputTaxProc = NA,
#' inputMorph = NA
#' )
#'
#' }
#' 
#' @export joinAquPointCount



joinAquPointCount <- function(inputDataList,
                              inputPoint = NA,
                              inputPerTax = NA,
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
    listExpNames <- c("apc_pointTransect", "apc_perTaxon")
    
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
    if (!isTRUE(is.na(inputPoint)) || !isTRUE(is.na(inputPerTax)) ||
        !isTRUE(is.na(inputTaxProc)) || !isTRUE(is.na(inputMorph))) {
      stop("When 'inputDataList' is supplied, all table input arguments must be NA.")
    }
  }
  
  
  ### Verify all table inputs are data frames if inputDataList is NA
  if (is.null(inputDataList) &
      (
        !is.data.frame(inputPoint) || !is.data.frame(inputPerTax)
      )) {
    stop("Data frames must be supplied for table inputs if 'inputDataList' is missing")
    
  }
  
  
  ### Conditionally define input tables ####
  if (inherits(inputDataList, "list")) {
    apPoint <- inputDataList$apc_pointTransect
    apPerTax <- inputDataList$apc_perTaxon
    if (!is.null(inputDataList$apc_taxonomyProcessed)) {
      apTaxProc <- inputDataList$apc_taxonomyProcessed
    } else{
      apTaxProc <- NA
    }
    if (!is.null(inputDataList$apc_morphospecies)) {
      apMorph <- inputDataList$apc_morphospecies
    } else{
      apMorph <- NA
    }
    
  } else {
    apPoint <- inputPoint
    apPerTax <- inputPerTax
    apTaxProc <- inputTaxProc
    apMorph <- inputMorph
    
  }
  
  
  
  ### Verify input tables contain required columns and data ####
  
  ### Verify 'apPoint' table contains required data
  #   Check for required columns
  pointExpCols <- c(
    "domainID",
    "siteID",
    "namedLocation",
    "pointNumber",
    "collectDate",
    "eventID",
    "remarks"
  )
  
  if (length(setdiff(pointExpCols, colnames(apPoint))) > 0) {
    stop(
      glue::glue(
        "Required columns missing from 'inputPoint':",
        '{paste(setdiff(pointExpCols, colnames(apPoint)), collapse = ", ")}',
        .sep = " "
      )
    )
  }
  
  #   Check for data
  if (nrow(apPoint) == 0) {
    stop(glue::glue("Table 'inputPoint' has no data."))
  }
  
  
  
  ### Verify 'apPerTax' table contains required data
  #   Check for required columns
  perTaxExpCols <- c(
    "sampleID",
    "taxonID",
    "scientificName",
    "morphospeciesID",
    "sampleCondition",
    "identificationHistoryID",
    "dataQF",
    "publicationDate",
    "release",
    "phylum",
    "division",
    "class",
    "order",
    "family",
    "genus",
    "section",
    "specificEpithet",
    "infraspecificEpithet",
    "variety",
    "form",
    "scientificNameAuthorship",
    "identificationQualifier",
    "identificationReferences",
    "taxonRank",
    "remarks",
    "identifiedBy",
    "identifiedDate",
    "uid"
  )
  
  if (length(setdiff(perTaxExpCols, colnames(apPerTax))) > 0) {
    stop(
      glue::glue(
        "Required columns missing from 'inputPerTax':",
        '{paste(setdiff(perTaxExpCols, colnames(apPerTax)), collapse = ", ")}',
        .sep = " "
      )
    )
  }
  
  #   Check for data
  if (nrow(apPerTax) == 0) {
    stop(glue::glue("Table 'inputPerTax' has no data."))
  }
  
  
  
  ### Verify 'apTaxProc' table contains required data if data exists
  taxProcExpCols <- c(
    "sampleID",
    "acceptedTaxonID",
    "scientificName",
    "sampleCondition",
    "identificationHistoryID",
    "dataQF",
    "publicationDate",
    "release",
    "phylum",
    "division",
    "class",
    "order",
    "family",
    "genus",
    "section",
    "specificEpithet",
    "infraspecificEpithet",
    "variety",
    "form",
    "scientificNameAuthorship",
    "identificationQualifier",
    "identificationReferences",
    "taxonRank",
    "remarks",
    "identifiedBy",
    "identifiedDate",
    "morphospeciesID",
    "uid",
    "domainID",
    "siteID",
    "namedLocation",
    "collectDate"
  )
  
  #   Check for data
  if (is.data.frame(apTaxProc)) {
    if (nrow(apTaxProc) == 0) {
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
    "taxonID",
    "scientificName",
    "morphospeciesID",
    "identificationQualifier",
    "identificationReferences",
    "identifiedBy",
    "dataQF"
  )
  
  
  #   Check for data
  if (is.data.frame(apMorph)) {
    if (nrow(apMorph) == 0) {
      message(
        "Warning: Table 'inputMorph' has no data. Join will not include identifications from the morphospecies table."
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
  
  
  ### Join apPerTax and apTaxProc tables ####
  # message(paste('apTaxProc = ', apTaxProc))
  is.data.frame(apTaxProc) && nrow(apTaxProc) > 0
  
  if (is.data.frame(apTaxProc) && nrow(apTaxProc) > 0) {
    #   Select needed columns from apTaxProc
    apTaxProc <- apTaxProc %>%
      dplyr::select(
        -"uid",
        -"domainID",
        -"siteID",
        -"namedLocation",
        -"collectDate",
        -"morphospeciesID"
      ) %>%
      dplyr::rename(taxonID = "acceptedTaxonID")
    
    #   Update expert taxonomist identifications
    apJoin1 <- apPerTax %>%
      dplyr::left_join(
        apTaxProc,
        by = "sampleID",
        suffix = c("_perTax", "_taxProc"),
        relationship = "many-to-many"
      ) %>%
      dplyr::mutate(
        sampleCondition = dplyr::case_when(
          !is.na(.data$sampleCondition_perTax) &
            !is.na(.data$sampleCondition_taxProc) ~ paste0(
              "perTaxon ",
              .data$sampleCondition_perTax,
              " | taxonProcessed ",
              .data$sampleCondition_taxProc
            ),!is.na(.data$sampleCondition_perTax) &
            is.na(.data$sampleCondition_taxProc) ~ paste0("perTaxon ", .data$sampleCondition_perTax),
          is.na(.data$sampleCondition_perTax) &
            !is.na(.data$sampleCondition_taxProc) ~ paste0("taxonProcessed ", .data$sampleCondition_taxProc),
          TRUE ~ NA
        ),
        taxonIDSourceTable = dplyr::case_when(
          !is.na(.data$taxonID_taxProc) ~ "apc_taxonomyProcessed",
          is.na(.data$taxonID_taxProc) &
            !is.na(.data$taxonID_perTax) ~ "apc_perTaxon",
          TRUE ~ NA
        ),
        tempTaxonID = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$taxonID_taxProc,
          .data$taxonID_perTax
        ),
        scientificName = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$scientificName_taxProc,
          .data$scientificName_perTax
        ),
        identificationHistoryID = dplyr::case_when(
          !is.na(.data$identificationHistoryID_perTax) &
            !is.na(.data$identificationHistoryID_taxProc) ~ paste0(
              .data$identificationHistoryID_perTax,
              " | ",
              .data$identificationHistoryID_taxProc
            ),
          is.na(.data$identificationHistoryID_taxProc) &
            !is.na(.data$identificationHistoryID_perTax) ~ .data$identificationHistoryID_perTax,!is.na(.data$identificationHistoryID_taxProc) &
            is.na(.data$identificationHistoryID_perTax) ~ .data$identificationHistoryID_taxProc,
          TRUE ~ NA
        ),
        perTaxonDataQF = .data$dataQF_perTax,
        taxProcessedDataQF = .data$dataQF_taxProc,
        perTaxonPublicationDate = .data$publicationDate_perTax,
        taxProcessedPublicationDate = .data$publicationDate_taxProc,
        taxProcessedRelease = .data$release_taxProc,
        perTaxonRelease = .data$release_perTax,
        phylum = dplyr::if_else(!is.na(.data$taxonID_taxProc), .data$phylum_taxProc, .data$phylum_perTax),
        division = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$division_taxProc,
          .data$division_perTax
        ),
        class = dplyr::if_else(
          !is.na(.data$taxonID_taxProc), 
          .data$class_taxProc, 
          .data$class_perTax),
        order = dplyr::if_else(!is.na(.data$taxonID_taxProc), 
                               .data$order_taxProc, 
                               .data$order_perTax),
        family = dplyr::if_else(!is.na(.data$taxonID_taxProc), 
                                .data$family_taxProc, 
                                .data$family_perTax),
        genus = dplyr::if_else(!is.na(.data$taxonID_taxProc), 
                               .data$genus_taxProc, 
                               .data$genus_perTax),
        section = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$section_taxProc,
          .data$section_perTax
        ),
        specificEpithet = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$specificEpithet_taxProc,
          .data$specificEpithet_perTax
        ),
        infraspecificEpithet = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$infraspecificEpithet_taxProc,
          .data$infraspecificEpithet_perTax
        ),
        variety = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$variety_taxProc,
          .data$variety_perTax
        ),
        form = dplyr::if_else(!is.na(.data$taxonID_taxProc), 
                              .data$form_taxProc, 
                              .data$form_perTax),
        scientificNameAuthorship = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$scientificNameAuthorship_taxProc,
          .data$scientificNameAuthorship_perTax
        ),
        identificationQualifier = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$identificationQualifier_taxProc,
          .data$identificationQualifier_perTax
        ),
        identificationReferences = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$identificationReferences_taxProc,
          .data$identificationReferences_perTax
        ),
        taxonRank = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$taxonRank_taxProc,
          .data$taxonRank_perTax
        ),
        remarks = dplyr::case_when(
          !is.na(.data$remarks_perTax) &
            !is.na(.data$remarks_taxProc) ~ paste0(
              "perTaxon remarks - ",
              .data$remarks_perTax,
              " | taxonProcessed remarks - ",
              .data$remarks_taxProc
            ),
          is.na(.data$remarks_taxProc) &
            !is.na(.data$remarks_perTax) ~ paste0("perTaxon remarks - ", .data$remarks_perTax),!is.na(.data$remarks_taxProc) &
            is.na(.data$remarks_perTax) ~ paste0("taxonProcessed remarks - ", .data$remarks_taxProc),
          TRUE ~ NA
        ),
        identifiedBy = dplyr::if_else(
          !is.na(.data$taxonID_taxProc),
          .data$identifiedBy_taxProc,
          .data$identifiedBy_perTax
        ),
        identifiedDate = dplyr::if_else(
          !is.na(.data$identifiedDate_taxProc),
          .data$identifiedDate_taxProc,
          .data$identifiedDate_perTax
        )
        
      ) %>%
      dplyr::select(-dplyr::matches("_taxProc"),-dplyr::matches("_perTax"),-"targetTaxaPresent",
             -"uid")
    
  } else {
    message("No data joined from apc_taxonomyProcessed table.")
    # rename columns if no taxProc join
    apJoin1 <- apPerTax %>%
      dplyr::mutate(
        tempTaxonID = .data$taxonID,
        remarks = dplyr::if_else(is.na(.data$remarks), NA, paste0("perTaxon remarks - ", .data$remarks)),
        perTaxonRelease = .data$release,
        taxonIDSourceTable = dplyr::if_else(is.na(.data$taxonID), NA, "apc_perTaxon"),
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
        "taxonID",
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
      dplyr::mutate(morphospeciesID = dplyr::if_else(
        !is.na(.data$morphospeciesID),
        paste0(.data$morphospeciesID, ".", substr(.data$collectDate, 1, 4)),
        .data$morphospeciesID
      )) %>%
      dplyr::left_join(apMorph,
                       by = "morphospeciesID",
                       suffix = c("_perTax", "_morph")) %>%
      # filter(!is.na(taxonID) & acceptedTaxonID %in% c('2PLANT', 'UNKALG')) %>%
      dplyr::mutate(
        taxonIDSourceTable = dplyr::if_else(
          !is.na(.data$taxonID) &
            .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          "apc_morphospecies",
          .data$taxonIDSourceTable
        ),
        acceptedTaxonID = dplyr::if_else(
          !is.na(.data$taxonID) &
            .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          .data$taxonID,
          .data$tempTaxonID
        ),
        scientificName = dplyr::if_else(
          !is.na(.data$taxonID) &
            .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          .data$scientificName_morph,
          .data$scientificName_perTax
        ),
        identificationQualifier = dplyr::if_else(
          !is.na(.data$taxonID) &
            .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          .data$identificationQualifier_morph,
          .data$identificationQualifier_perTax
        ),
        identificationReferences = dplyr::if_else(
          !is.na(.data$taxonID) &
            .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          .data$identificationReferences_morph,
          .data$identificationReferences_perTax
        ),
        identifiedBy = dplyr::if_else(
          !is.na(.data$taxonID) &
            .data$tempTaxonID %in% c('2PLANT', 'UNKALG'),
          .data$identifiedBy_morph,
          .data$identifiedBy_perTax
        ),
        identifiedDate = NA #not currently in pub table
        #dataQF = ifelse(!is.na(taxonID), )#either that may be relevant?
        
      ) %>%
      dplyr::select(-"taxonID",
                    -"tempTaxonID",-dplyr::matches("_morph"),-dplyr::matches("_perTax"))
  } else {
    message("No data joined from apc_morphospecies table.")
    
    apJoin2 <- apJoin1 %>% 
      dplyr::mutate(acceptedTaxonID = .data$tempTaxonID) %>% 
      dplyr::select(-"tempTaxonID")
  }
  
  
  ### Join apPoint and apPerTax tables ####
  
  # Update morphospecies taxon identifications
  joinPointCounts <- apPoint %>%
    dplyr::left_join(
      apJoin2,
      by = c(
        "domainID",
        "siteID",
        "namedLocation",
        "pointNumber",
        "collectDate",
        "eventID"
      ),
      suffix = c("_point", "_perTax")
    ) %>%
    dplyr::mutate(
      remarks = dplyr::case_when(
        !is.na(.data$remarks_perTax) &
          !is.na(.data$remarks_point) ~ paste0(
            "pointTransect remarks - ",
            .data$remarks_point,
            " | ",
            .data$remarks_perTax
          ),!is.na(.data$remarks_perTax) &
          is.na(.data$remarks_point) ~ .data$remarks_perTax,
        is.na(.data$remarks_perTax) &
          !is.na(.data$remarks_point) ~ paste0("pointTransect remarks - ", .data$remarks_point),
        TRUE ~ NA
      )
    )
  
  return(joinPointCounts)
  
} #function closer
