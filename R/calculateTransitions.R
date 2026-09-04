#' @title Find trees at NEON sites whose status has changed between sampling events
#'
#' @author
#' Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#' Claire K Lunch \email{clunch@BattelleEcology.org} \cr
#'
#' @description Identifies individual trees whose status has changed between surveys (e.g. live to dead), and infers the appropriate status for missed events when possible; for example, when a tree is "live" then is not sampled for several eventIDs and is "live" again, it is inferred to be alive for the eventIDs that were missed. Biomass is also inferred via interpolation for missed eventIDs but biomass is not inferred for missed eventIDs outside the recorded data (i.e., no extrapolation). The growthInterval is then determined using dates for both observed and inferred biomass.
#'
#' @details This is a helper function for the estimateWoodProd() function, used to estimate mortality, recruitment, and missingness.
#'
#' @param biomassTable Table (generated in estimateWoodProd()) containing biomass records for individual trees. [data.frame]
#'
#' @param plotYearTable The 'vst_perplotperyear' table for the site x month combination(s) of interest; this table is passed by the user into the estimateWoodProd() function. [data.frame]
#'
#' @return A table with one record for each individual whose status has changed, noting its status and biomass at each time point.
#'
#' @keywords internal

calculateTransitions <- function(biomassTable,
                                 plotYearTable) {


  ### SESSION: SET SESSION BEHAVIOR FOR 'dplyr::summarise()' ####
  sessionInform <- getOption("dplyr.summarise.inform", default = TRUE)
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = sessionInform), add = TRUE)



  ### INPUT DATA PREPARATION ####
  agbDF <- biomassTable


  ##  Prep vst_perplotperyear data
  plotDF <- plotYearTable

  #   Remove unneeded per plot records
  plotDF <- plotDF %>%
    dplyr::filter(.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical),
                  .data$dataCollected != "dendrometerOnly" | is.na(.data$dataCollected))

  #   Reduce to needed columns
  plotDF <- plotDF %>%
    dplyr::select("domainID",
                  "siteID",
                  "plotID",
                  "plotType",
                  "eventID",
                  "eventType",
                  "dataCollected",
                  "targetTaxaPresent",
                  "treesPresent",
                  "totalSampledAreaTrees")

  #   Standardize 'treesPresent' elements to "Y", "N", and NA
  #--> Set treesPresent = "N" when targetTaxaPresent = "N" to simplify presence absence logic for "tree" growth forms
  plotDF <- plotDF %>%
    dplyr::mutate(
      treesPresent = dplyr::replace_when(
        .data$treesPresent,
        .data$treesPresent == "Present - sampled" ~ "Y",
        .data$treesPresent == "Present - not sampled" ~ "Y",
        .data$treesPresent == "Present - sampling criteria not met" ~ "N",
        .data$targetTaxaPresent == "N" & is.na(.data$treesPresent) ~ "N",
        .data$treesPresent == "notAssessed" ~ NA_character_
      )
    )



  ### GENERATE 'TRANSITIONS' DATA FRAME ####

  ### Create "complete" plotDF: Contains a row for all combinations of plotID x eventID
  #--> Note that tidyr::complete() output is not sensible if > 1 siteID supplied; estimateWoodProd function prevents this
  plotDF <- plotDF %>%
    tidyr::complete(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$eventID)

  ### Join plot and biomass data and expand
  #--> Create a record for an individualID in each plot-event when the individual occurs in that plot in at least one eventID
  #--> Allows accounting for individuals when whole plot mortality or recruitment occurs or a plot is missed

  ##  Use full_join to associate plot data with individual data
  #--> Reveals plots-events with no individuals and individuals with no plot-events
  transitionDF <- dplyr::full_join(plotDF,
                                   agbDF %>%
                                     dplyr::select("plotID", "eventID", "individualID"),
                                   by = c("plotID", "eventID"))

  #   Identify unique plot-event combinations
  plotEventDF <- transitionDF %>%
    dplyr::distinct(.data$plotID, .data$eventID)

  #   Identify unique plot-individual combinations
  plotIndivDF <- transitionDF %>%
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::distinct(.data$plotID, .data$individualID)

  #   For each individual in a plot, create a row for each plot-event combination
  plotEventIndivDF <- plotEventDF %>%
    dplyr::left_join(plotIndivDF,
                     by = "plotID",
                     relationship = "many-to-many")


  ##  Fill in original plot-level data where they exist, otherwise they are NA
  transFilledDF <- plotEventIndivDF %>%
    dplyr::left_join(transitionDF %>%
                       dplyr::select(-"individualID") %>%
                       dplyr::distinct(.data$plotID,
                                       .data$eventID,
                                       .keep_all = TRUE),
                     by = c("plotID", "eventID"),
                     relationship = "many-to-many")


  ##  Fill in original individual-level data where they exist, otherwise they are NA
  transFilledDF <- transFilledDF %>%
    dplyr::left_join(agbDF %>%
                       dplyr::select(-"domainID",
                                     -"siteID",
                                     -"plotType",
                                     -"eventType",
                                     -"dataCollected"),
                     by = c("plotID",
                            "eventID",
                            "individualID"))



  ### DETERMINE 'transitionStatus' PER RECORD ####

  ##  Retain rows from last time plot or individual was sampled to determine 'transitionStatus' and 'growthInterval'; remove rows where no observations were made
  transFilterDF <- transFilledDF %>%
    dplyr::filter(!is.na(.data$date) | (!is.na(.data$totalSampledAreaTrees) & !is.na(.data$treesPresent))) %>%
    dplyr::relocate("eventID",
                    "individualID",
                    .before = "date")


  ##  Fill in 'year' when missing; happens for plotIDs not in estimateWoodMass output
  transFilterDF <- transFilterDF %>%
    dplyr::mutate(
      year = dplyr::replace_when(
        .data$year,
        is.na(.data$year) ~ as.numeric(stringr::str_extract(string = .data$eventID, pattern = "20[0-9]{2}$"))
      )
    )


  ##  Infer liveDeadStatus for missed eventIDs
  #--> An individual can be inferred to be "live" or "dead" when records with liveDeadStatus = "lost" or NA are book-ended by consistent values of "live" or "dead"; helps avoid false "recruitment"
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::group_modify(~{
      df <- .x
      #   Original liveDeadStatus data as 's0'
      s0 <- df$liveDeadStatus
      #   Working copy of liveDeadStatus as 's'
      s  <- s0
      #   liveDeadStatus rows that are not NA and not "lost"
      nz <- which(!is.na(s) & s != "lost")

      if (length(nz) >= 2) {
        for (k in seq_len(length(nz) - 1)) {
          i <- nz[k]
          j <- nz[k + 1]

          if (s[i] %in% c("live", "dead") &&
              identical(s[i], s[j])) {

            gap <- s[(i + 1):(j - 1)]
            if (all(is.na(gap) | gap == "lost")) {
              s[(i + 1):(j - 1)] <- s[i]
            }
          }
        }
      }

      df$liveDeadStatus <- s

      #   Flag rows originally NA or "lost" that were changed to either "live" or "dead"
      df$statusFlag <- (is.na(s0) | (s0 == "lost" & !is.na(s0))) & !((is.na(s0) & is.na(s)) | (!is.na(s0) & !is.na(s) & s0 == s))
      df
    }) %>%
    dplyr::relocate("individualID",
                    .before = "eventID") %>%
    dplyr::relocate("statusFlag",
                    .after = "liveDeadStatus")


  ##  Determine 'transitionStatus' relative to last time the plot or individual was sampled
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(
      transitionStatus = dplyr::case_when(

        #   Assign "noChange" when current and prior liveDeadStatus are identical and neither are "lost"
        .data$liveDeadStatus != "lost" & dplyr::lag(.data$liveDeadStatus) != "lost" &
          .data$liveDeadStatus == dplyr::lag(.data$liveDeadStatus) ~ "noChange",

        #   Assign "mortality" when current liveDeadStatus is "dead" and prior is "live"
        .data$liveDeadStatus == "dead" & dplyr::lag(.data$liveDeadStatus) == "live" ~ "mortality",

        #   Assign "recruitment" when current liveDeadStatus is both "live" and not the first row, and prior liveDeadStatus is NA
        #--> When row_number() == 1, lag() always returns NA
        .data$liveDeadStatus == "live" & is.na(dplyr::lag(.data$liveDeadStatus)) & dplyr::row_number() != 1 ~ "recruitment",

        #   Assign "mortality" when plot-level mortality for "tree" growthForms, and previously the individual was "live" and plot was sampled
        (is.na(.data$liveDeadStatus) & .data$treesPresent == "N") &
          (dplyr::lag(.data$liveDeadStatus) == "live" & dplyr::lag(.data$treesPresent) == "Y") ~ "mortality",

        #   Assign "mortality" when plot-level mortality for trees, and previously the individual was "live" and plot was sampled
        (is.na(.data$liveDeadStatus) & .data$treesPresent == "N") &
          (dplyr::lag(.data$liveDeadStatus) == "live" & dplyr::lag(.data$treesPresent) == "Y") ~ "mortality",

        #   Assign "missing" when liveDeadStatus is NA or "lost" (cannot be inferred) and the plot was sampled, and was previously "live"
        ((is.na(.data$liveDeadStatus) | .data$liveDeadStatus == "lost") & .data$treesPresent == "Y") &
          dplyr::lag(.data$liveDeadStatus) == "live" ~ "missing",

        TRUE ~ NA_character_
      ),
      .after = "liveDeadStatus"
    )



  ### APPROXIMATE MISSING MASS VALUES ####
  #--> Calculated 'growthInterval' data will be based off both inferred and allometrically estimated mass values
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(
      estimatedMass = {
        x <- .data$year
        y <- .data$agb_kg
        ok <- !is.na(y)

        if (sum(ok) < 2) {
          #   Return NA if not enough points to interpolate
          rep(NA_real_, length(y))

        } else {
          #   Rule 1 does not extrapolate to NAs outside the data
          round(stats::approx(x = x[ok],
                              y = y[ok],
                              xout = x,
                              rule = 1)$y,
                digits = 2)
        }
      },
    .after = "agb_kg")

  #   Conditionally populate 'agb_kg' with missing mass values; create 'massFlag' to record when an estimated mass is used
  transFilterDF <- transFilterDF %>%
    dplyr::mutate(
      massFlag = dplyr::case_when(
        is.na(.data$agb_kg) & !is.na(.data$estimatedMass) ~ TRUE,
        TRUE ~ FALSE
      ),
      agb_kg = dplyr::replace_when(
        .data$agb_kg,
        is.na(.data$agb_kg) & !is.na(.data$estimatedMass) ~ .data$estimatedMass
      )
    ) %>%
    dplyr::relocate("massFlag",
                    .after = "agb_kg") %>%
    dplyr::select(-"estimatedMass")



  ### OUTPUT #######################################################

  ##  Relocate columns for output
  transFilterDF <- transFilterDF %>%
    dplyr::relocate(c("plotID", "subplotID"), .after = "year") %>%
    dplyr::relocate("nlcdClass", .after = "plotType") %>%
    dplyr::relocate(("taxonID":"scientificName"), .after = "ninetyCrownDiameter") %>%
    dplyr::relocate("source", .after = "scientificName")


  ##  Return output
  return(transFilterDF)

}
