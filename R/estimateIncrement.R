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
#' @return The input table, with new columns indicating estimated biomass change for each tree in each year.
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



  ### RECRUITMENT: ESTIMATE BIOMASS PRODUCTION FROM INGROWTH ####

  ### Estimate biomass for "recruitment" individuals at DBH = 10 cm

  ##  Identify recruits from input dataset and remove columns not needed by allometry mass function
  temp <- agbDF %>%
    dplyr::filter(.data$transitionStatus == "recruitment") %>%
    dplyr::select(-c("plotType", "nlcdClass", "eventType", "dataCollected", "targetTaxaPresent", "treesPresent",
                     "totalSampledAreaTrees", "year", "transitionStatus", "statusFlag", "agb_kg", "massFlag", "source")) %>%
    dplyr::mutate(stemDiameter = 10) %>%
    dplyr::ungroup()

  #--> Note: For allometries that depend on 'height', there is no growthForm-based threshold that can be used for 'height' when recruitment stemDiameter = 10 cm. There are allometries that allow height estimation from stemDiameter...Chave?
  #--> Note: Setting stemDiameter = 10 cm may not be correct for secondary multi-boles, as in this case, qualifying diameter depends on the diameter of the largest bole and is not a static value.


  ##  Create biomass estimates with allometric wood mass function
  recruitDF <- estimateAllometricWoodyMass(appIndTable = temp,
                                           growthFormSubset = "tree")

  recruitDF <- recruitDF %>%
    dplyr::rename("recruit_kg" = "agb_kg")


  ##  Join "recruit_kg" to primary dataset and assign value to 'agb_kg' for year prior to recruitment
  #--> Additionally update liveDeadStatus to "live", update 'statusFlag' and 'massFlag' to TRUE, and add stemDiameter = 10 cm
  agbDF <- agbDF %>%
    dplyr::left_join(recruitDF %>%
                       dplyr::select("plotID", "eventID", "individualID", "recruit_kg"),
                     by = c("plotID", "eventID", "individualID")) %>%
    dplyr::relocate("recruit_kg",
                    .before = "agb_kg") %>%
    dplyr::mutate(

      agb_kg = dplyr::replace_when(.data$agb_kg,
                                   !is.na(dplyr::lead(.data$recruit_kg)) ~ dplyr::lead(.data$recruit_kg)),

      massFlag = dplyr::replace_when(.data$massFlag,
                                     !is.na(dplyr::lead(.data$recruit_kg)) ~ TRUE),

      stemDiameter = dplyr::replace_when(.data$stemDiameter,
                                         !is.na(dplyr::lead(.data$recruit_kg)) ~ 10),

      liveDeadStatus = dplyr::replace_when(.data$liveDeadStatus,
                                           !is.na(dplyr::lead(.data$recruit_kg)) ~ "live"),

      statusFlag = dplyr::replace_when(.data$statusFlag,
                                       !is.na(dplyr::lead(.data$recruit_kg)) ~ TRUE)

    ) %>%
    dplyr::select(-"recruit_kg")



  ### INCREMENT FLAG: IDENTIFY UNEXPECTED ANNUAL stemDiameter INCREMENTS ##########
  #--> Note: A number of individuals at ABBY appear to have legitimate stemDiameter increments > 3 AND < 4 cm/y
  #--> Note: Flag not applied to year a secondary stem of a multi-bole tree is recruited --> no standard qualifying diameter
  agbDF <- agbDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(

      incrementFlag = dplyr::case_when(

        .data$transitionStatus == "recruitment" & grepl("[0-9]{5}[A-Za-z]{1}$", .data$individualID) ~ FALSE,
        abs(.data$stemDiameter - dplyr::lag(.data$stemDiameter)) / (.data$year - dplyr::lag(.data$year)) > 3.5 ~ TRUE,
        TRUE ~ FALSE),

      .before = "stemDiameter"

    )

  #   Filter and cache flagged individualIDs for output
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
  #--> Use 'date' if available, otherwise use 'year'
  #--> Calculating 'growthInterval' at this stage ensures accuracy *after* missing/flagged individuals removed, and takes advantage of recruit mass being filled in so an interval is calculated for all recruits

  agbDF <- agbDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(

      growthInterval = dplyr::case_when(
        #   When available, use 'date' to calculate 'growthInterval'
        !is.na(.data$date) & !is.na(dplyr::lag(.data$date)) ~ as.numeric(difftime(.data$date, lag(.data$date), units = "days")) / 365.25,

        #   Otherwise use 'year' from PPPY table to calculate 'growthInterval' if mass data are present
        (is.na(.data$date) | is.na(dplyr::lag(.data$date))) & !is.na(.data$agb_kg) & !is.na(dplyr::lag(.data$agb_kg)) ~
          .data$year - dplyr::lag(.data$year),

        #   Return NA when biomass data unavailable and 'year' based interval not relevant
        TRUE ~ NA_real_
      ),

      #   Reduce 'growthInterval' to a single digit
      growthInterval = round(.data$growthInterval, digits = 1)

    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate("growthInterval", .after = "date")



  ### ESTIMATE BIOMASS INCREMENT PER INTERVAL ####################################
  #--> Clark Method 1 used for "tree" growthForms --> determine individual increments from live trees existing in both time points + increment from recruits; no estimate of mortality is required.

  agbDF <- agbDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(

      #   Calculate mass increment (kg/y) when liveDeadStatus = "live" at both timepoints
      agbIncr_kgyr = dplyr::case_when(
        .data$liveDeadStatus == "live" & dplyr::lag(.data$liveDeadStatus) == "live" ~
          round((.data$agb_kg - dplyr::lag(.data$agb_kg)) / .data$growthInterval, digits = 1),

        TRUE ~ NA_real_
      ),

      #   Calculate mass increment (kg) when liveDeadStatus = "live" at both timepoints
      agbIncr_kg = dplyr::case_when(
        .data$liveDeadStatus == "live" & dplyr::lag(.data$liveDeadStatus) == "live" ~
          round(.data$agb_kg - dplyr::lag(.data$agb_kg), digits = 1),

        TRUE ~ NA_real_
      ),

      .before = "agb_kg"
    )



  ### OUTPUT #####################################################################
  output <- list(agbIncrDF = agbDF,
                 missingDF = missingDF,
                 flaggedDF = flaggedDF)

  return(output)

}
