#' @title Estimate biomass increment of trees at NEON sites
#'
#' @author
#' Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#' Claire K Lunch \email{clunch@BattelleEcology.org} \cr
#'
#' @description Calculate the biomass change of trees between time points.
#'
#' @details A helper function for the estimateWoodProd() function.
#'
#' @param biomassTable Table (generated in estimateWoodProd()) containing biomass records for each tree, with columns for plotID, individualID, taxonID, and its biomass in consecutive years.
#'
#' @param missing Select how missing individuals are handled. The default is "filter", which removes trees that were missed during a sampling event and for which plantStatus and biomass cannot be inferred/estimated. These "missing" individuals are filtered to a "missing" table before any productivity calculations are made; in other words, "missing" individuals are ignored for the productivity calculation. The alternate option is "retain", which retains "missing" individuals and assumes that they are dead; this option thus results in missing individuals contributing to both the mortality and increment components of ANPP. [character]
#'
#' @param flagged Select how individuals flagged for implausibly large stem diameter increments are handled (options are "filter" and "retain"). Default is "retain", which retains flagged individuals and includes them in calculations regardless of stem diameter increment; although flagged individuals contribute to productivity with this option, they are still sent to the "flagged" output table for review. Alternatively, the "filter" option removes all records for individuals with >= 3.5 cm absolute annual stemDiameter increment, including recruited individuals for which inferred initial diameter is 10 cm; flagged individuals are sent to the "flagged" output table. [character]
#'
#' @return The input table, with new columns indicating estimated biomass change for each tree in each year with various units.
#'
#' @keywords internal
#'

estimateIncrement <- function(biomassTable,
                              missing,
                              flagged) {



  ### SESSION: SET SESSION BEHAVIOR FOR 'dplyr::summarise()' ####
  sessionInform <- getOption("dplyr.summarise.inform", default = TRUE)
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = sessionInform), add = TRUE)



  ### INPUT PREP ################################################
  agbDF <- biomassTable


  ### For missing = "filter", filter out transitionStatus = "missing" and all subsequent years
  if (missing == "filter") {

    ##  Identify transitionStatus = "missing" and records thereafter within each individualID
    missingDF <- agbDF %>%
      dplyr::group_by(.data$individualID) %>%
      dplyr::arrange(.data$year,
                     .by_group = TRUE) %>%
      dplyr::filter(cumsum(dplyr::coalesce(.data$transitionStatus == "missing", FALSE)) > 0) %>%
      dplyr::ungroup()


    ##  Retain all records before transitionStatus = "missing" records
    agbDF <- agbDF %>%
      dplyr::group_by(.data$individualID) %>%
      dplyr::arrange(.data$year,
                     .by_group = TRUE) %>%
      dplyr::filter(!cumsum(dplyr::coalesce(.data$transitionStatus == "missing", FALSE)) > 0) %>%
      dplyr::ungroup()

  } # End missing == "filter" conditional



  ### For missing = "retain", assume "missing" are "dead"
  #--> Also update liveDeadStatus, transitionStatus, and statusFlag fields
  if (missing == "retain") {

    ##  Create empty 'missingDF' data frame; needed for output but not populated in this case
    missingDF <- data.frame()


    ##  Update liveDeadStatus, transitionStatus, and statusFlag fields
    agbDF <- agbDF %>%
      dplyr::mutate(

        liveDeadStatus = dplyr::replace_when(.data$liveDeadStatus,
                                             .data$transitionStatus == "missing" ~ "dead"),

        statusFlag = dplyr::replace_when(.data$statusFlag,
                                         .data$transitionStatus == "missing" ~ TRUE),

        transitionStatus = dplyr::replace_when(.data$transitionStatus,
                                               .data$transitionStatus == "missing" ~ "mortality")
      )

  } # End missing == "retain" conditional



  ### Identify previously flagged stemDiameter increments and conditionally remove from increment calculation based on user input

  ##  Filter and cache flagged individualIDs for output
  #--> Note: All records for an individualID are filtered if an individualID is flagged at any point in time
  flaggedDF <- agbDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::filter(any(.data$incrementFlag == TRUE))


  ##  Filter flagged individuals with user-supplied 'flagged' argument
  if (flagged == "filter") {

    agbDF <- agbDF %>%
      dplyr::filter(!.data$individualID %in% flaggedDF$individualID)

  }



  ### CALCULATE 'growthInterval' RELATIVE TO LAST SAMPLING EVENT #################
  #--> Use 'predictedDate' if available, otherwise use 'year'
  #--> Calculating 'growthInterval' at this stage ensures accuracy *after* missing/flagged individuals removed

  agbDF <- agbDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(

      growthInterval = dplyr::case_when(
        #   When available, use 'predictedDate' to calculate 'growthInterval'
        !is.na(.data$predictedDate) & !is.na(dplyr::lag(.data$predictedDate)) ~ 
          lubridate::time_length(.data$predictedDate - dplyr::lag(.data$predictedDate), unit = "years"),

        #   Otherwise use 'year' from PPPY table to calculate 'growthInterval' if mass data are present
        (is.na(.data$predictedDate) | is.na(dplyr::lag(.data$predictedDate))) & !is.na(.data$agb_kg) & !is.na(dplyr::lag(.data$agb_kg)) ~
          .data$year - dplyr::lag(.data$year),

        #   Return NA when biomass data unavailable and 'year' based interval not relevant
        TRUE ~ NA_real_
      ),

      #   Reduce 'growthInterval' to a single digit
      growthInterval = round(.data$growthInterval, digits = 1)

    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate("growthInterval", .after = "predictedDate")



  ### ESTIMATE BIOMASS INCREMENT PER INTERVAL ####################################
  #--> Clark Method 1 used for "tree" growthForms --> determine individual increments from live trees existing in both time points + increment from recruits + increment from half of sampling interval when mortality occurs and sampling interval is > 1 year

  agbDF <- agbDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    
    dplyr::mutate(
      agbIncr_kgyr = dplyr::case_when(
        
        #   Calculate mass increment (kg/y) when liveDeadStatus = "live" at both timepoints
        .data$liveDeadStatus == "live" & dplyr::lag(.data$liveDeadStatus) == "live" ~
          (.data$agb_kg - dplyr::lag(.data$agb_kg)) / .data$growthInterval,
        
        #   Calculate mass increment (kg/y) for sampling interval mid-point when mortality occurs
        .data$transitionStatus == "mortality" & dplyr::lag(.data$liveDeadStatus) == "live" & !is.na(.data$predictedDate) &
          !is.na(dplyr::lag(.data$predictedDate)) ~ 
          (.data$agb_kg - dplyr::lag(.data$agb_kg)) / .data$growthInterval,

        TRUE ~ NA_real_
      ),
      
      agbIncr_kgyr = round(agbIncr_kgyr, digits = 1),

      
      agbIncr_kg = dplyr::case_when(
        
        #   Calculate mass increment (kg) when liveDeadStatus = "live" at both timepoints
        .data$liveDeadStatus == "live" & dplyr::lag(.data$liveDeadStatus) == "live" ~ 
          .data$agb_kg - dplyr::lag(.data$agb_kg),
        
        #   Calculate mass increment (kg) for sampling interval mid-point when mortality occurs
        .data$transitionStatus == "mortality" & dplyr::lag(.data$liveDeadStatus) == "live" & !is.na(.data$predictedDate) &
          !is.na(dplyr::lag(.data$predictedDate)) ~ 
          .data$agb_kg - dplyr::lag(.data$agb_kg),

        TRUE ~ NA_real_
      ),
      
      agbIncr_kg = round(agbIncr_kg, digits = 1),

      .before = "agb_kg"
    )



  ### OUTPUT #####################################################################
  output <- list(agbIncrDF = agbDF,
                 missingDF = missingDF,
                 flaggedDF = flaggedDF)

  return(output)

}
