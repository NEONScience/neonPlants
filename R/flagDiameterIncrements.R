#' @title Identify anomalous NEON Vegetation Structure stemDiameter values
#'
#' @author Courtney L Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Identifies anomalous stemDiameter values per individualID by comparing time series data that result in increments > 3.5 cm/year (e.g., data entry errors). Rows with a stemDiameter that contribute to increments > 3.5 cm/year are iteratively flagged until none remain.
#'
#' @details This is a helper function for the estimateWoodProd() function, called within calculateTransitions().
#'
#' @param appIndTable Table (generated in estimateWoodProd()) containing records with stemDiameter for individual trees. Input data must be grouped by individualID and arranged by date within group. [data.frame]
#' 
#' @param threshold The increment threshold in cm/year beyond which stemDiameter values are flagged.
#' 
#' @param flaggedColumn The name of the logical (TRUE/FALSE) column that holds function output. If it does not exist, the function creates the column and sets the initial value to FALSE for all rows.
#'
#' @return A table with one record per observation per individual, with anomalous values identified in the column specified by the "flaggedColumn" argument.
#'
#' @keywords internal

flagDiameterIncrements <- function(appIndTable, 
                                   threshold = 3.5, 
                                   flaggedColumn = "incrementFlag") {
  
  #   Create the flag column as FALSE if it doesn't already exist
  if (!flaggedColumn %in% names(appIndTable)) {
    appIndTable[[flaggedColumn]] <- FALSE
  }
  
  repeat {
    idx <- which(!appIndTable[[flaggedColumn]])
    if (length(idx) < 2) break
    
    d <- appIndTable[idx, ]
    
    rateOf <- function(x) {
      yrs <- lubridate::time_length(x$estimatedDate - dplyr::lag(x$estimatedDate), unit = "year")
      abs((x$stemDiameter - dplyr::lag(x$stemDiameter)) / yrs)
    }
    
    absRate <- rateOf(d)
    if (all(is.na(absRate)) || max(absRate, na.rm = TRUE) <= threshold) break
    
    i <- which.max(absRate)   # offending pair is (i - 1, i)
    
    #   Function: Identify largest rate left after dropping a candidate; removal bridges the gap
    #--> Note: If no rate can be computed (fewer than two rows left), return 0 so there is no evidence against that candidate.
    worstAfterDrop <- function(drop) {
      r <- rateOf(d[-drop, ])
      if (all(is.na(r))) 0 else max(r, na.rm = TRUE)
    }
    
    flagPos <- if (worstAfterDrop(i - 1) <= worstAfterDrop(i)) i - 1 else i
    appIndTable[[flaggedColumn]][idx[flagPos]] <- TRUE
  }
  
  appIndTable
} # end function