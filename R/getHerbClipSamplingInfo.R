#' @title Find NEON herbaceous clip harvest sampling information
#'
#' @author Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Herbaceous clip harvest (DP1.10023.001) data are collected annually at NEON terrestrial sites with qualifying vegetation, and different plots are sampled in different years. Moreover, plot management may change through time at a site, with sites in a given year being managed for grazing, agricultural crops, neither, or with distinctly managed subsets of plots within a site. This function returns a table documenting which management activities occurred in which types of plots in a given year for each site.
#'
#' @details Herbaceous clip harvest sampling occurs in two types of plots, and management activities within plot type inform how data are used by the scaleHerbMass and estimateHerbProd functions:
#'    * Distributed plots: Sampled once per growing season every 5-6 years with a focus on site-level above-ground biomass; at sites with agricultural crops, these plots planted with crops may be sampled more than one time per year to enable site-level productivity estimates. Grazing exclosures are never deployed in these plots and Distributed plots are not used for site-level herbaceous productivity estimates at grazed sites.
#'    * Tower plots: Sampled once or multiple times per growing season with a focus on herbaceous productivity. At sites not managed for grazing or agriculture, Tower plots are sampled 1-2 times per year depending on the plant community. At grazed sites, Tower plots subjected to grazing are sampled with and without exclosures, and one to multiple times per year depending on the plant community, to enable estimating grazing consumption and final standing crop at the end of the season; not all Tower plots at grazed sites are grazed. At agricultural sites, each plot is clipped before each crop reaches maturity, meaning each plot may be clipped one or more times per year depending on the site host planting schedule; not all Tower plots at agricultural sites are planted with crops.
#'
#' Additional site-specific information:
#'    * The San Joaquin Experimental Range (SJER) site: The site experiences a Mediterranean growing season from roughly October to May, and herbaceous plants are largely dormant through the summer months. As such, data from August of one year through July of the following year must be downloaded for this site to capture a full growing season.
#'
#' @param sites 4-letter code for one more NEON sites. [character]
#' @param token NEON API token [character]
#'
#' @return A table listing the types of plots sampled for each site-year combination, the plot management activities associated with each plot type, and the number of unique plots and sampling events for each combination. Start and end dates indicate when sampling started in each site-year, and when sampling was completed within each site-year; for sites with more than one sampling event (e.g., grazed sites) this date range encompasses all sampling events. For the most recent year of data, it is possible that not all data collection has been completed; in this case, the end date reflects the end date of data in the database, and the release of more data may be pending.
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @examples
#' \dontrun{
#'
#' rmnpHerbClipInfo <- getHerbClipSamplingInfo(sites = "RMNP", token = myToken)
#'
#' herbClipInfo <- getHerbClipSamplingInfo(sites = c("KONZ", "RMNP"), token = myToken)
#'
#' }
#'
#' @export getHerbClipSamplingInfo


getHerbClipSamplingInfo <- function(sites = NA_character_,
                                    token = NA_character_) {



  ### Input exception handling
  #   Check 'sites' is not NA
  if(is.na(sites)) {
    stop("The 'sites' variable is required for this function.")
  }

  #   Check 'sites' is one of expected values
  expSites <- c("BART", "HARV", "BLAN", "SCBI", "SERC", "DSNY", "JERC", "OSBS", "GUAN", "LAJA", "STEI", "TREE", "UNDE", "KONA", "KONZ", "UKFS", "GRSM", "MLBS", "ORNL", "DELA", "LENO", "TALL", "DCFS", "NOGP", "WOOD", "CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "MOAB", "NIWO", "JORN", "SRER", "ONAQ", "ABBY", "WREF", "SJER", "SOAP", "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL", "PUUM")

  if (!all(sites %in% expSites)) {
    stop(glue::glue("Invalid site code:",
                    '{paste(setdiff(sites, expSites), collapse = ", ")}',
                    .sep = " "))
  }



  ### Retrieve 'hbp_perbout' table for all time
  hbpBout <- suppressMessages(neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                                           site = sites,
                                                           tabl = "hbp_perbout",
                                                           include.provisional = TRUE,
                                                           check.size = FALSE,
                                                           progress = FALSE,
                                                           token = token))

  #   Set date data type and define 'year' variable; create 'site-year' variable, filter out Sampling Impractical
  perBout <- hbpBout$hbp_perbout %>%
    dplyr::mutate(setDate = as.Date(.data$setDate),
                  collectDate = as.Date(.data$collectDate),
                  year = as.numeric(lubridate::year(.data$collectDate)),
                  year = dplyr::case_when(.data$siteID == "SJER" & .data$collectDate < as.Date(glue::glue("{.data$year}-07-15")) ~ (.data$year -1),
                                          TRUE ~ .data$year),
                  siteYear = paste(.data$siteID, .data$year, sep = "-"),
                  plotSiteYear = paste(.data$plotID, .data$siteID, .data$year, sep = "-"),
                  .before = "eventID") %>%
    dplyr::filter(.data$samplingImpractical == "OK" | is.na(.data$samplingImpractical))



  ### Separate records into grazed, cropped, combo, and standard 'siteID x year' data frames; each requires custom logic to determine plot number by management activity

  ##  Step 1: Identify records in 'siteID x year' combinations with both grazing and crops in a plot at any point
  comboSiteYearDF <- perBout %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$plotManagement == "Agricultural") & any(.data$exclosure == "Y"))


  ##  Step 2: Identify records in all 'siteID x year' combinations that supported grazing
  grazedSiteYearDF <- perBout %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$exclosure == "Y"),
                  !.data$siteYear %in% comboSiteYearDF$siteYear) %>%
    dplyr::ungroup()


  ##  Step 3: Identify records for all 'siteID x year' combinations that contained crops in a plot at any point
  cropSiteYearDF <- perBout %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$plotManagement == "Agricultural"),
                  !.data$siteYear %in% comboSiteYearDF$siteYear) %>%
    dplyr::ungroup()


  ##  Step 4: Identify all "standard" clips - i.e., no grazing, no crops at any point in a 'siteID x year'
  stdSiteYearDF <- perBout %>%
    dplyr::filter(!.data$siteYear %in% comboSiteYearDF$siteYear,
                  !.data$siteYear %in% grazedSiteYearDF$siteYear,
                  !.data$siteYear %in% cropSiteYearDF$siteYear)



  ### Summarize "standard" site-year sampling ####

  if (!nrow(stdSiteYearDF)) {

    summaryStd <- NULL

  } else {

    #   Identify the date range each plotType x plotManagement combo was sampled, as well as plot and eventID count
    summaryStd <- stdSiteYearDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotType) %>%
      dplyr::summarise(plotManagement = "Non-agricultural",
                       startDate = min(.data$collectDate),
                       endDate = max(.data$collectDate),
                       plotCount = length(unique(.data$plotID)),
                       eventCount = length(unique(.data$eventID)),
                       .groups = "drop")

  } # End nrow(stdSiteYearDF) conditional


  ### Summarize "cropped" site-year sampling ####

  if (!nrow(cropSiteYearDF)) {

    summaryCrop <- NULL

  } else {

    ##  Identify plots with plotManagement == "Agricultural" at any point in the year
    cropPlots <- cropSiteYearDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID) %>%
      dplyr::filter(any(.data$plotManagement == "Agricultural")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(plotManagement = "Agricultural")


    ##  Identify cropless plots at Agricultural sites
    croplessPlots <- cropSiteYearDF %>%
      dplyr::filter(!.data$plotSiteYear %in% cropPlots$plotSiteYear) %>%
      dplyr::mutate(plotManagement = "Non-agricultural")


    ##  Create "cropped" site-year summary
    summaryCrop <- dplyr::bind_rows(cropPlots,
                                    croplessPlots) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotType,
                      .data$plotManagement) %>%
      dplyr::summarise(startDate = min(.data$collectDate),
                       endDate = max(.data$collectDate),
                       plotCount = length(unique(.data$plotID)),
                       eventCount = length(unique(.data$eventID)),
                       .groups = "drop")

  } # End !nrow(cropSiteYearDF) conditional



  ### Summarize "grazed" site-year sampling ####

  if (!nrow(grazedSiteYearDF)) {

    summaryGraze <- NULL

  } else {

    ##  Identify Tower plots grazed at any point in the year (i.e., exclosure = "Y" at any point)
    grazePlots <- grazedSiteYearDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID) %>%
      dplyr::filter(.data$plotType == "tower",
                    any(.data$exclosure == "Y")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(plotManagement = "Grazed")


    ##  Identify ungrazed plots at grazed sites, includes Distributed plots
    grazelessPlots <- grazedSiteYearDF %>%
      dplyr::filter(!.data$plotSiteYear %in% grazePlots$plotSiteYear) %>%
      dplyr::mutate(plotManagement = "Non-agricultural")


    ##  Create "grazed" site-year summary
    summaryGraze <- dplyr::bind_rows(grazePlots,
                                     grazelessPlots) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotType,
                      .data$plotManagement) %>%
      dplyr::summarise(startDate = min(.data$collectDate),
                       endDate = max(.data$collectDate),
                       plotCount = length(unique(.data$plotID)),
                       eventCount = length(unique(.data$eventID)),
                       .groups = "drop")

  } # End !nrow(grazedSiteYearDF) conditional



  ### Summarize "cropped" and "grazed" (combo) site-year sampling ####

  if (!nrow(comboSiteYearDF)) {

    summaryCombo <- NULL

  } else {

    #   Identify "grazed" plots at combo sites
    grazePlots <- comboSiteYearDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID) %>%
      dplyr::filter(.data$plotType == "tower",
                    any(.data$exclosure == "Y")) %>%
      dplyr::mutate(plotManagement = "Grazed")

    #   Identify "cropped" plots at combo sites
    cropPlots <- comboSiteYearDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID) %>%
      dplyr::filter(any(.data$plotManagement == "Agricultural"))

    #   Identify "standard" plots at combo sites
    stdPlots <- comboSiteYearDF %>%
      dplyr::filter(!.data$plotSiteYear %in% grazePlots$plotSiteYear,
                    !.data$plotSiteYear %in% cropPlots$plotSiteYear) %>%
      dplyr::mutate(plotManagement = "Non-agricultural")


    ##  Create "combo" site-year summary
    summaryCombo <- dplyr::bind_rows(grazePlots,
                                     cropPlots,
                                     stdPlots) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotType,
                      .data$plotManagement) %>%
      dplyr::summarise(startDate = min(.data$collectDate),
                       endDate = max(.data$collectDate),
                       plotCount = length(unique(.data$plotID)),
                       eventCount = length(unique(.data$eventID)),
                       .groups = "drop")

  } # End !nrow(comboSiteYearDF) conditional



  ### Finalize output summary table ####
  summary <- dplyr::bind_rows(summaryStd,
                              summaryCrop,
                              summaryGraze,
                              summaryCombo) %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotType,
                   .data$plotManagement)

  return(summary)

} # End function
