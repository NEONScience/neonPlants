#' @title Identify trees at NEON sites whose status has changed between sampling events and process data for recruits
#'
#' @author
#' Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#' Claire K Lunch \email{clunch@BattelleEcology.org} \cr
#'
#' @description Identifies individual trees whose status has changed between surveys (e.g. live to dead), and infers the appropriate status for missed events when possible; for example, when a tree is "live" then is not sampled for several eventIDs and is "live" again, it is inferred to be alive for the eventIDs that were missed. The stemDiameter is also inferred via interpolation for missed eventIDs but stemDiameter is not inferred for missed eventIDs outside the recorded data (i.e., no extrapolation). The sampling interval is then determined using dates for both observed and inferred stemDiameter, and suspicious stemDiameter increment rates are flagged, including for recruits. Where possible, inappropriate stemDiameter for recruits is replaced with a modeled value (i.e., the standard assumption of 10 cm stemDiameter for recruits is clearly incorrect and it is possible to model a more appropriate diameter).
#'
#' @details This is a helper function for the estimateWoodProd() function, used to estimate mortality, recruitment, and missingness, and to infer missing and recruited stemDiameter when appropriate. For both "recruitment" and "mortality" events, increment is calculated relative to the mid-point between two sampling events when the sampling interval is more than 1 year.
#'
#' @param appIndTable Table (generated in estimateWoodProd()) containing records with stemDiameter for individual trees. [data.frame]
#'
#' @param plotYearTable The 'vst_perplotperyear' table for the site x month combination(s) of interest; this table is passed by the user into the estimateWoodProd() function. [data.frame]
#'
#' @return A table with one record for each individual, noting its status, stemDiameter, and any generated flags at each time point.
#'
#' @keywords internal

calculateTransitions <- function(appIndTable,
                                 plotYearTable) {
  
  
  ### SESSION: SET SESSION BEHAVIOR FOR 'dplyr::summarise()' ####
  sessionInform <- getOption("dplyr.summarise.inform", default = TRUE)
  options(dplyr.summarise.inform = FALSE)
  on.exit(options(dplyr.summarise.inform = sessionInform), add = TRUE)
  
  
  
  ### INPUT DATA PREPARATION ####
  aiDF <- appIndTable
  
  
  ##  Prep vst_perplotperyear data
  #   Reduce to needed columns
  plotDF <- plotYearTable
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
  #--> Allows accounting for individuals when whole plot mortality or recruitment occurs, or a plot or individual is missed
  
  ##  Use full_join to associate plot data with individual data
  #--> Reveals plots-events with no individuals and individuals with no plot-events
  transitionDF <- dplyr::full_join(plotDF,
                                   aiDF %>%
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
    dplyr::left_join(aiDF %>%
                       dplyr::select(-"domainID",
                                     -"siteID",
                                     -"plot_eventID"),
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
  
  
  ##  Create 'year' column
  transFilterDF <- transFilterDF %>%
    dplyr::mutate(year = dplyr::case_when(
      !is.na(.data$eventID) ~ as.numeric(stringr::str_extract(string = .data$eventID, pattern = "20[0-9]{2}$")),
      TRUE ~ NA_integer_
    ),
    .before = "eventID"
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
  
  
  ##  Infer 'liveDeadStatus' for resurrected individualIDs - i.e., "live" -> "dead" -> "live" with any number of NA rows separating the "live" rows from the "dead" row
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::group_modify(~{
      df <- .x
      #   Original liveDeadStatus data as 's0'
      s0 <- df$liveDeadStatus
      #   Working copy of liveDeadStatus as 's'
      s  <- s0
      
      #   liveDeadStatus rows that are "live" or "dead" (non-NA, non-"lost")
      nz <- which(!is.na(s) & s != "lost")
      
      if (length(nz) >= 3) {
        for (k in seq_len(length(nz) - 2)) {
          i <- nz[k]
          j <- nz[k + 1]
          m <- nz[k + 2]
          
          # Check for pattern: "live"..."dead"..."live"
          if (s[i] == "live" &&
              s[j] == "dead" &&
              s[m] == "live") {
            
            # Change the "dead" record to "live"
            s[j] <- "live"
            
            # Also fill in any NAs or "lost" values between the first "live" and second "live"
            gap <- s[(i + 1):(m - 1)]
            if (any(is.na(gap) | gap == "lost")) {
              s[(i + 1):(m - 1)] <- "live"
            }
          }
        }
      }
      
      df$liveDeadStatus <- s
      
      #   Flag 'liveDeadStatus' rows that have changed status
      df$statusFlag <- ((!is.na(s0) & s0 != s) | (is.na(s0) & !is.na(s)))
      df
    })
  
  
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
  
  
  
  ### APPROXIMATE MISSING 'stemDiameter' VALUES ####
  
  ### Interpolate missing stemDiameter values
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(
      estimatedDiam = {
        x <- .data$year
        y <- .data$stemDiameter
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
                digits = 1)
        }
      },
      .after = "stemDiameter")
  
  #   Conditionally populate 'stemDiameter' with interpolated values; create estimatedDiamFlag to indicate when a stemDiameter is interpolated
  transFilterDF <- transFilterDF %>%
    dplyr::mutate(
      estimatedDiamFlag = dplyr::case_when(
        is.na(.data$stemDiameter) & !is.na(.data$estimatedDiam) ~ TRUE,
        TRUE ~ FALSE
      ),
      
      stemDiameter = dplyr::replace_when(
        .data$stemDiameter,
        is.na(.data$stemDiameter) & !is.na(.data$estimatedDiam) ~ .data$estimatedDiam
      )
    ) %>%
    dplyr::relocate("estimatedDiamFlag", .after = "stemDiameter") %>%
    dplyr::select(-"estimatedDiam")
  
  
  
  ### RECRUITS: ASSIGN RECRUITED 'stemDiameter' VALUE ####
  
  ### Replace NA 'stemDiameter' values with "10" cm for year prior to recruitment event; update 'estimatedDiamFlag', 'liveDeadStatus' and 'statusFlag' accordingly
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(
      stemDiameter = dplyr::replace_when(
        .data$stemDiameter,
        is.na(.data$stemDiameter) & dplyr::lead(.data$transitionStatus) == "recruitment" ~ 10
      ),
      
      estimatedDiamFlag = dplyr::replace_when(
        .data$estimatedDiamFlag,
        .data$stemDiameter == 10 & dplyr::lead(.data$transitionStatus) == "recruitment" ~ TRUE
      ),
      
      liveDeadStatus = dplyr::replace_when(
        .data$liveDeadStatus,
        .data$stemDiameter == 10 & dplyr::lead(.data$transitionStatus) == "recruitment" ~ "live"
      ),
      
      statusFlag = dplyr::replace_when(
        .data$statusFlag,
        .data$stemDiameter == 10 & dplyr::lead(.data$transitionStatus) == "recruitment" ~ TRUE
      )
    )
  
  
  
  ### Determine predicted recruitment date
  #--> Note: YYYY-10-01 is used when the interval between the "recruitment" year and the previous time the plot was sampled is 1 year; otherwise, the midpoint date between the "recruitment" sampling date and 10/01 of the year the plot was previously sampled is used.
  
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(
      
      predictedDate = dplyr::case_when(
        dplyr::lead(.data$transitionStatus) == "recruitment" & !is.na(dplyr::lead(.data$date)) & is.na(.data$date) &
          dplyr::lead(.data$year) != .data$year + 1 ~ 
          as.Date(glue::glue("{.data$year}-10-01")) + (dplyr::lead(.data$date) - as.Date(glue::glue("{.data$year}-10-01"))) / 2,
        dplyr::lead(.data$transitionStatus) == "recruitment" & !is.na(dplyr::lead(.data$date)) & is.na(.data$date) &
          dplyr::lead(.data$year) == .data$year + 1 ~ 
          as.Date(glue::glue("{.data$year}-10-01")),
        TRUE ~ .data$date
      ),
      
      .after = "date"
    )
  
  
  
  ### Flag unexpected 'stemDiameter' increments
  #--> Note: A number of individuals at ABBY appear to have legitimate annual stemDiameter increments > 3 AND < 4 cm/y
  #--> Note: Flag not applied to recruited secondary stems of a multi-bole tree --> no standard qualifying diameter
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    dplyr::mutate(
      
      incrementFlag = dplyr::case_when(
        .data$transitionStatus == "recruitment" & grepl("[0-9]{5}[A-Za-z]{1}$", .data$individualID) ~ FALSE,
        abs(.data$stemDiameter - dplyr::lag(.data$stemDiameter)) / 
          lubridate::time_length(.data$predictedDate - dplyr::lag(.data$predictedDate), unit = "years") > 3.5 ~ TRUE,
        TRUE ~ FALSE
      ),
      
      .after = "stemDiameter"
    ) %>%
    
    #   Update incrementFlag for recruited individuals to include flagging year prior to recruitment
    #--> Note: This flags when assuming a 10 cm recruitment diameter is inappropriate
    dplyr::mutate(
      incrementFlag = dplyr::replace_when(
        .data$incrementFlag,
        dplyr::lead(.data$transitionStatus) == "recruitment" & dplyr::lead(.data$incrementFlag) == TRUE ~ TRUE
      )
    )
  
  
  
  ### Update 'stemDiameter' with modeled value for recruits with incrementFlag == TRUE
  #--> Note: These individuals were likely missed in the previous bout and therefore appear inexplicably large when first recorded
  
  ##  Isolate all records associated with any individualID that is flagged for a suspicious "recruitment" event
  temp <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    #   Filter to individualIDs recruited at some point and with flagged 10 cm recruitment diameter
    dplyr::filter(any(.data$transitionStatus == "recruitment") & any(.data$stemDiameter == 10 & .data$incrementFlag == TRUE)) %>%
    
    #   Ensure at least 3 rows of data exist before setting 10 cm -> NA and model fitting
    dplyr::filter(sum(!is.na(.data$predictedDate) & !is.na(.data$stemDiameter)) >= 3) %>%
    dplyr::relocate("individualID", .before = "tempStemID")
  
  
  ##  Update recruitment diameter from 10 cm to NA for flagged rows, then model new predicted stemDiameter
  temp <- temp %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    
    #   Set offending recruitment stemDiameters to NA_real_
    dplyr::mutate(
      stemDiameter = dplyr::replace_when(
        .data$stemDiameter,
        dplyr::lead(.data$transitionStatus == "recruitment") & .data$stemDiameter == 10 & .data$incrementFlag == TRUE ~ NA_real_
      )
    ) %>%
    
    #   Model new predictedStemDiam (for all time points with a date)
    dplyr::mutate(
      model = list(lm(.data$stemDiameter ~ .data$predictedDate, 
                      data = dplyr::pick(dplyr::everything()) %>% 
                        dplyr::filter(!is.na(.data$predictedDate) & !is.na(.data$stemDiameter)))),
      predictedStemDiam = round(stats::predict(.data$model[[1]], newdata = dplyr::pick(dplyr::everything())),
                                digits = 1)
    ) %>%
    
    dplyr::relocate("predictedStemDiam", .after = "stemDiameter")
  
  
  ##  Update recruitment stemDiameter previously set to NA with with new predicted value; remove flags if "recruitment" no longer meets flagging criteria
  temp <- temp %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    
    dplyr::mutate(
      stemDiameter = dplyr::replace_when(
        .data$stemDiameter,
        dplyr::lead(.data$transitionStatus == "recruitment") & is.na(.data$stemDiameter) & .data$incrementFlag == TRUE ~
          .data$predictedStemDiam
      ),
      
      incrementFlag = dplyr::replace_when(
        .data$incrementFlag,
        dplyr::lead(.data$transitionStatus == "recruitment") & !is.na(.data$stemDiameter) & .data$incrementFlag == TRUE &
          abs(dplyr::lead(.data$stemDiameter) - .data$stemDiameter) / 
          lubridate::time_length(dplyr::lead(.data$predictedDate) - .data$predictedDate, unit = "years") < 3.5 ~ FALSE
      ),
      
      incrementFlag = dplyr::replace_when(
        .data$incrementFlag,
        .data$transitionStatus == "recruitment" & !is.na(dplyr::lag(.data$stemDiameter)) & dplyr::lag(.data$incrementFlag) == FALSE &
          .data$incrementFlag == TRUE & 
          abs(.data$stemDiameter - dplyr::lag(.data$stemDiameter)) / 
          lubridate::time_length(.data$predictedDate - dplyr::lag(.data$predictedDate), unit = "years") < 3.5 ~ FALSE
      )
    ) %>%
    
    #   Column clean-up
    dplyr::select(-"model",
                  -"predictedStemDiam")
  
  
  ##  Replace flagged recruitment stemDiameters in 'transFilterDF' with modeled stemDiameters from 'temp'
  
  #   Create unique individualID-eventID identifier for modified records
  temp <- temp %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-"),
                  .before = "plotID")
  
  #   Remove corresponding records from transFilterDF
  transFilterDF <- transFilterDF %>%
    dplyr::mutate(indivEventID = paste(.data$individualID, .data$eventID, sep = "-"),
                  .before = "plotID") %>%
    dplyr::filter(!indivEventID %in% temp$indivEventID) %>%
    dplyr::select(-"indivEventID")
  
  #   Recombine data frames
  transFilterDF <- dplyr::bind_rows(transFilterDF,
                                    temp %>%
                                      dplyr::select(-"indivEventID"))
  
  
  
  ### MORTALITY: ASSUME TREES DIE AT MID-POINT OF SAMPLING INTERVAL ####
  #--> Note: Similar to recruitment, no mid-point is calculated when sampling is annual
  
  transFilterDF <- transFilterDF %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::arrange(.data$year,
                   .by_group = TRUE) %>%
    
    dplyr::mutate(
      predictedDate = dplyr::replace_when(
        .data$predictedDate,
        .data$transitionStatus == "mortality" & !is.na(.data$date) & !is.na(dplyr::lag(.data$date)) &
          .data$year != dplyr::lag(.data$year) + 1 ~
          dplyr::lag(.data$date) + (.data$date - dplyr::lag(.data$date)) / 2
      )
    )
  
    
  
  ### OUTPUT #######################################################
  
  ##  Relocate columns for output
  transFilterDF <- transFilterDF %>%
    dplyr::relocate("plotID", .before = "subplotID") %>%
    dplyr::relocate("individualID", .before = "tempStemID") %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$individualID,
                   .data$year)
  
  
  ##  Return output
  return(transFilterDF)
  
}
