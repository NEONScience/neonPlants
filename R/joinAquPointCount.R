#' @title Join NEON aquatic plant point count data into a single table with merged taxonomic identifications

#' @author Madaline Ritter \email{ritterm1@battelleecology.org} \cr

#' @description Join the 'apc_pointTransect', 'apc_perTaxon', 'apc_taxonomy' (Processed or Raw) and 'apc_morphospecies' tables to generate a single table that contains point count data with taxonomic identifications for each sampleID. Data inputs are NEON Aquatic Plant, Bryophyte, Lichen, and Macroalgae Point Counts in Wadeable Streams (DP1.20072.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations. 
#'
#' @details Input data may be provided either as a list or as individual tables. However, if both list and table inputs are provided at the same time the function will error out. For table joining to be successful, inputs must contain data from the same site x month combination(s) for all tables. If both processed and raw taxonomy tables are provided as list inputs, the function will default to the 'apc_taxonomyProcessed' table for joining. If the 'apc_morphospecies' table is not provided, the function will proceed with joining point count data and per taxon identifications without incorporating morphospecies identifications.
#' 
#' In the joined output table, the 'acceptedTaxonID' and associated taxonomic fields are populated from the first available identification in the following order: 'apc_taxonomyProcessed', 'apc_taxonomyRaw', 'apc_perTaxon', or 'apc_morphospecies'. For samples identified both in the field and by an expert taxonomist, the expert identification is retained in the output. A new field, 'taxonIDSourceTable', is included in the output and indicates the source table for each sample's identification.
#' 
#' If a single sample in 'apc_taxonomyProcessed' contains multiple macroalgae species, each species will be represented as a separate row in 'apc_pointTransect' for every point associated with that sampleID.
#' 
#' @param inputDataList A list object comprised of Aquatic Plant, Bryophyte, Lichen, and Macroalgae Point Count tables (DP1.20072.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided. [list]
#'
#' @param inputPoint The 'apc_pointTransect' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputPerTax The 'apc_perTaxon' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputTaxonomy The 'apc_taxonomyProcessed' or 'apc_taxonomyRaw' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputMorph The 'apc_morphospecies' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @return A table containing point transect data joined with taxonomic identifications. For points where targetTaxaPresent == 'Y', taxonomic information is joined from the first available source table.
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
#' startdate = "2018-03",
#' enddate = "2018-05",
#' tabl = "all",
#' package = 'expanded',
#' check.size = FALSE
#' )
#' 
#' #   Join downloaded point count data
#' df <- neonPlants::joinAquPointCount(
#' inputDataList = apc,
#' inputPoint = NA,
#' inputPerTax = NA,
#' inputTaxonomy = NA,
#' inputMorph = NA
#' )
#'
#' }
#' 
#' @export joinAquPointCount



joinAquPointCount <- function(inputDataList,
                              inputPoint = NA,
                              inputPerTax = NA,
                              inputTaxonomy = NA,
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
        !isTRUE(is.na(inputTaxonomy)) || !isTRUE(is.na(inputMorph))) {
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
      apTax <- inputDataList$apc_taxonomyProcessed
      apTax$Table <- "apc_taxonomyProcessed"
    } else if (!is.null(inputDataList$apc_taxonomyRaw)) {
      apTax <- inputDataList$apc_taxonomyRaw
      apTax$Table <- "apc_taxonomyRaw"
      apTax$taxonRank <- NA #not currently published in taxonomyRaw table. Delete when taxonomy WG completes standardization
      apTax<- dplyr::rename(apTax, acceptedTaxonID = "taxonID")
    }else{
      apTax <- NA
    }
    if (!is.null(inputDataList$apc_morphospecies)) {
      apMorph <- inputDataList$apc_morphospecies
    } else{
      apMorph <- NA
    }
    
  } else {
    apPoint <- inputPoint
    apPerTax <- inputPerTax
    apTax <- inputTaxonomy
    if (is.data.frame(apTax) && "acceptedTaxonID" %in% colnames(apTax)) {
      apTax$Table <- "apc_taxonomyProcessed"
    }else if(is.data.frame(apTax) && "taxonID" %in% colnames(apTax)){
      apTax$Table <- "apc_taxonomyRaw"
      apTax$taxonRank <- NA #not currently published in taxonomyRaw table. Delete when taxonomy WG completes standardization
      apTax <- dplyr::rename(apTax, acceptedTaxonID = "taxonID")
    }
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
  }else{
    # apPoint$collectDate <- as.POSIXct(apPoint$collectDate, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
    apPoint$collectDate <- as.POSIXct(apPoint$collectDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
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
  
  
  
  ### Verify 'apTax' table contains required data if data exists
  expertTaxExpCols <- c(
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
    "collectDate",
    "Table"
  )
  
  #   Check for data
  if (is.data.frame(apTax)) {
    if (nrow(apTax) == 0) {
      message(
        glue::glue(
          "Warning: Table 'inputTaxonomy' has no data. Join will not include processed taxonomy data."
        )
      )
    } else {
      #   Check for required columns if data exists
      if (length(setdiff(expertTaxExpCols, colnames(apTax))) > 0) {
        stop(
          glue::glue(
            "Required columns missing from 'inputTaxonomy':",
            '{paste(setdiff(expertTaxExpCols, colnames(apTax)), collapse = ", ")}',
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
    "morphospeciesResolvedDate",
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
  
  
  
  
  
  ### Join apPerTax and apTax tables ####
  
  if (is.data.frame(apTax) && nrow(apTax) > 0) {
    #   Select needed columns from apTax
    apTax <- apTax %>%
      dplyr::select(
        -"uid",
        -"domainID",
        -"siteID",
        -"namedLocation",
        -"collectDate",
        -"morphospeciesID"
      ) %>%
      dplyr::rename(taxonID = "acceptedTaxonID")
    
    #   Columns conditionally replaced with expertTax data
    join1_cols <- c(
      "scientificName", 
      "phylum", "division", "class", "order",
      "family", "genus", "section", "specificEpithet", "infraspecificEpithet",
      "variety", "form", "scientificNameAuthorship", "identificationQualifier", "identificationReferences",
      "taxonRank", "identifiedBy", "identifiedDate"
    )
    
    #   Update expert taxonomist identifications
    apJoin1 <- apPerTax %>%
      dplyr::left_join(
        apTax,
        by = "sampleID",
        suffix = c("_perTax", "_expertTax"),
        relationship = "many-to-many"
      ) %>%
      dplyr::mutate(
        sampleCondition = dplyr::case_when(
          !is.na(.data$sampleCondition_perTax) &
            !is.na(.data$sampleCondition_expertTax) ~ paste0(
              "perTaxon ",
              .data$sampleCondition_perTax,
              " | expertTax ",
              .data$sampleCondition_expertTax
            ),!is.na(.data$sampleCondition_perTax) &
            is.na(.data$sampleCondition_expertTax) ~ paste0("perTaxon ", .data$sampleCondition_perTax),
          is.na(.data$sampleCondition_perTax) &
            !is.na(.data$sampleCondition_expertTax) ~ paste0("expertTax ", .data$sampleCondition_expertTax),
          TRUE ~ NA
        ),
        taxonIDSourceTable = dplyr::case_when(
          !is.na(.data$taxonID_expertTax) & (unique(apTax$Table) == 'apc_taxonomyProcessed') ~ "apc_taxonomyProcessed",
          !is.na(.data$taxonID_expertTax) & (unique(apTax$Table) == 'apc_taxonomyRaw') ~ "apc_taxonomyRaw",
          is.na(.data$taxonID_expertTax) & !is.na(.data$taxonID_perTax) ~ "apc_perTaxon",
          TRUE ~ NA
        ),
        tempTaxonID = dplyr::if_else(
          !is.na(.data$taxonID_expertTax),
          .data$taxonID_expertTax,
          .data$taxonID_perTax
        ),
        identificationHistoryID = dplyr::case_when(
          !is.na(.data$identificationHistoryID_perTax) &
            !is.na(.data$identificationHistoryID_expertTax) ~ paste0(
              .data$identificationHistoryID_perTax,
              " | ",
              .data$identificationHistoryID_expertTax
            ),
          is.na(.data$identificationHistoryID_expertTax) &
            !is.na(.data$identificationHistoryID_perTax) ~ .data$identificationHistoryID_perTax,!is.na(.data$identificationHistoryID_expertTax) &
            is.na(.data$identificationHistoryID_perTax) ~ .data$identificationHistoryID_expertTax,
          TRUE ~ NA
        ),
        perTaxonDataQF = .data$dataQF_perTax,
        expertTaxDataQF = .data$dataQF_expertTax,
        perTaxonPublicationDate = .data$publicationDate_perTax,
        expertTaxPublicationDate = .data$publicationDate_expertTax,
        expertTaxRelease = .data$release_expertTax,
        perTaxonRelease = .data$release_perTax,
        remarks = dplyr::case_when(
          !is.na(.data$remarks_perTax) & !is.na(.data$remarks_expertTax) ~ paste0(
              "perTaxon remarks - ", .data$remarks_perTax, " | expertTax remarks - ",.data$remarks_expertTax
              ),
          is.na(.data$remarks_expertTax) &
            !is.na(.data$remarks_perTax) ~ paste0("perTaxon remarks - ", .data$remarks_perTax),!is.na(.data$remarks_expertTax) &
            is.na(.data$remarks_perTax) ~ paste0("expertTax remarks - ", .data$remarks_expertTax),
          TRUE ~ NA
        )
      ) 
    
    for (col in join1_cols) {
      expertTax_col <- paste0(col, "_expertTax")
      perTax_col <- paste0(col, "_perTax")
      apJoin1[[col]] <- dplyr::if_else(
        !is.na(apJoin1$taxonID_expertTax),
        if (expertTax_col %in% names(apJoin1)) as.character(apJoin1[[expertTax_col]]) else NA_character_,
        if (perTax_col %in% names(apJoin1)) as.character(apJoin1[[perTax_col]]) else NA_character_
      )
    }
    
    apJoin1 <- apJoin1 %>%
      dplyr::select(-"uid", -"targetTaxaPresent",
                    -dplyr::matches("_expertTax"),-dplyr::matches("_perTax"))
    
    
  } else {
    message("No data joined from apc_taxonomyProcessed table.")
    #   rename columns if no expertTax join
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
    apMorph <- apMorph %>%
      dplyr::select(
        "taxonID",
        "scientificName",
        "morphospeciesID",
        "identificationQualifier",
        "identificationReferences",
        "identifiedBy",
        "morphospeciesResolvedDate",
        ## Uncomment next lines once morph table has been updated
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
        "taxonRank",
        "dataQF"
      ) %>% 
      dplyr::rename(identifiedDate="morphospeciesResolvedDate")
    
    apJoin2 <- apJoin1 %>%
      dplyr::mutate(morphospeciesID = dplyr::if_else(
        !is.na(.data$morphospeciesID),
        paste0(.data$morphospeciesID, ".", substr(.data$collectDate, 1, 4)),
        .data$morphospeciesID
      )) %>%
      dplyr::left_join(apMorph,
                       by = "morphospeciesID",
                       suffix = c("_perTax", "_morph")) %>%
      dplyr::mutate(
        taxonIDSourceTable = dplyr::if_else(
          !is.na(.data$taxonID) & .data$tempTaxonID %in% c("2PLANT", "UNKALG"), 
          "apc_morphospecies", .data$taxonIDSourceTable),
        acceptedTaxonID = dplyr::if_else(
          !is.na(.data$taxonID) & .data$tempTaxonID %in% c("2PLANT", "UNKALG"), 
          .data$taxonID, .data$tempTaxonID),
        morphospeciesDataQF = .data$dataQF
      )
    
    #   Columns conditionally replaced with morph data
    join2_cols <- c(
      "scientificName", "identificationQualifier", "identificationReferences",
      "identifiedBy", "identifiedDate"
      ## Uncomment next two lines once morph table has been updated
      , "phylum", "division", "class", "order",
      "family", "genus", "section", "specificEpithet", "infraspecificEpithet",
      "variety", "form", "taxonRank"
    )
    
    for (col in join2_cols) {
      morph_col <- paste0(col, "_morph")
      perTax_col <- paste0(col, "_perTax")
      apJoin2[[col]] <- dplyr::if_else(
        !is.na(apJoin2$taxonID) & apJoin2$tempTaxonID %in% c("2PLANT", "UNKALG"),
        if (morph_col %in% names(apJoin2)) as.character(apJoin2[[morph_col]]) else NA_character_,
        if (perTax_col %in% names(apJoin2)) as.character(apJoin2[[perTax_col]]) else NA_character_
      )
    }
    
    apJoin2 <- apJoin2 %>%
      dplyr::select(
        -"taxonID", -"tempTaxonID", -"dataQF",
        -dplyr::matches("_morph"),-dplyr::matches("_perTax"))
    
    
  } else {
    message("No data joined from apc_morphospecies table.")
    
    apJoin2 <- apJoin1 %>% 
      dplyr::mutate(acceptedTaxonID = .data$tempTaxonID) %>% 
      dplyr::select(-"tempTaxonID")
  }
  
  
  
  
  
  ### Join apPoint and apPerTax tables ####
  
  apc_joinPointCounts <- apPoint %>%
    dplyr::rename(
      pointPublicationDate = "publicationDate",
      pointRelease = "release",
      pointDataQF = "dataQF"
      ) %>% 
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
      )) %>% 
    dplyr::select(-"remarks_perTax", -"remarks_point", -"Table")
  
  
  ###  Re-format date columns ####
  apc_joinPointCounts$identifiedDate <- as.Date(apc_joinPointCounts$identifiedDate)
  
  apc_joinPointCounts$collectDate <- as.POSIXct(apc_joinPointCounts$collectDate, 
    format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  
  apc_joinPointCounts$pointPublicationDate <- as.POSIXct(apc_joinPointCounts$pointPublicationDate, 
    format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  
  apc_joinPointCounts$perTaxonPublicationDate <- as.POSIXct(apc_joinPointCounts$perTaxonPublicationDate, 
    format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  
  if (is.data.frame(apTax) && nrow(apTax) > 0) {
    apc_joinPointCounts$expertTaxPublicationDate <- as.POSIXct(apc_joinPointCounts$expertTaxPublicationDate, 
      format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  }

  
  return(apc_joinPointCounts)
  
} #function closer
