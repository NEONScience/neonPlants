#' @title Estimate herbaceous ANPP (Above-ground Net Primary Productivity) at NEON sites
#'
#' @author
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#' Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Estimate above-ground herbaceous productivity at NEON sites using Herbaceous Clip Harvest data tables (DP1.10023.001).
#'
#' @details Input data should be a list of herbaceous biomass (DP1.10023.001) data frames downloaded via the neonUtilities::loadByProduct() function. The companion scaleHerbMass() function is called internally, and outputs from this function are used to estimate herbaceous productivity.
#'
#' At sites where herbaceous plants are not subject to grazing management and exclosures are not in use, and crops are not planted at any point during the year, the estimate of herbaceous ANPP is simply the standing biomass clipped from the bout with the greatest biomass.
#'
#' Where grazing management occurs in Tower plots and exclosures have been established, consumption is calculated for each Tower plot sampling event as average biomass within exclosures minus average biomass outside of exclosures; the consumption estimate is therefore inherently a "site-level" estimate rather than a "plot-level" estimate. Site-level consumption from each sampling bout is then summed across all bouts and added to the standing biomass of the last bout of the season to generate site-level estimates of herbaceous ANPP. For grazed sites, the Science Design only supports calculating consumption at the site level. The clip harvests within exclosures are not close enough to ambient clip harvests to support plot-level consumption estimates.
#'
#' At sites where crops are planted, productivity is determined on a plot-by-plot basis to account for the potential for multiple plantings throughout the season, as well as "wild-type" plots that are either fallow for the duration of the season or are not managed for agriculture.
#'
#' @param inputDataList An R list object produced by the neonUtilities::loadByProduct() function for the NEON Herbaceous Clip Harvest data product. [list]
#'
#' @param plotSubset The options are the default "all" (all Tower and Distributed plots), "tower" (all plots in the Tower airshed but no Distributed plots), and "distributed" (all Distributed plots, which are sampled on a 5-year interval and are spatially representative of the NLCD classes at a site, and no Tower plots). [character]
#'
#' @return A list that includes productivity summary data frames. Output tables include:
#'   * herb_ANPP_site - Summarizes herbaceous ANPP for each site x year combination ("Mg/ha/yr" and "g/m2/yr").
#'   * herb_ANPP_plot - Summarizes herbaceous ANPP for the sum of all herbaceous plants for each plot x year combination ("Mg/ha/yr" and "g/m2/yr"). Plot-level summaries are not returned for grazed Tower plots, for the reason outlined in details.
#'   * herb_ANPP_plot_herbgroup - Summarizes herbaceous ANPP by herbGroup ("Cool Season Graminoids", "N-fixing Plants", etc.) for each plot x year combination ("Mg/ha/yr" and "g/m2/yr"). Plot-level summaries are not returned for grazed Tower plots.
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Herbaceous clip harvest data
#' HbpDat <- neonUtilities::loadByProduct(
#' dpID = "DP1.10023.001",
#' package = "basic",
#' check.size = FALSE
#' )
#'
#' # Example with arguments at default values
#' df <-estimateHerbProd(inputDataList = HbpDat)
#'
#' # Example specifying an alternative plotSubset value
#' df <-estimateHerbProd(inputDataList = HbpDat,
#' plotSubset = "tower")
#'
#' }
#'
#' @export estimateHerbProd

estimateHerbProd = function(inputDataList,
                            plotSubset = "all") {

  options(dplyr.summarise.inform = FALSE)



  ### Check inputDataList properties ####
  #   Check that 'inputDataList' is an object with class 'list'
  if(!methods::is(inputDataList, class = "list" )){
    stop("The inputDataList argument is expected to be a list generated with neonUtilities::loadByProduct(). A character, data.frame, or NA argument is not allowed.")
  }



  ### Check plotSubset properties
  if (!plotSubset %in% c("all", "tower", "distributed")) {
    stop("The only valid plotSubset options are 'all', 'tower', 'distributed'.")
  }



  ### Generate scaleHerbMass outputs and unlist
  scaleHerbMassOutput <- neonPlants::scaleHerbMass(inputDataList = inputDataList,
                                                   plotSubset = plotSubset)

  hbp_agb <- scaleHerbMassOutput$hbp_agb

  rm(scaleHerbMassOutput)




  ### Verify scaleHerbMassOutputs tables contain required columns and data ####

  ### Verify scaleHerbMass output table 'hbp_agb' contains required data
  #   Check for data
  if (!nrow(hbp_agb)) {
    stop(glue::glue("Table from scaleHerbMass() output 'hbp_agb' has no data."))
  }




  ### Prepare data for downstream calculations ####

  #   Filter input data using user-supplied 'plotSubset' argument; insurance step since scaleHerbMass() output should already only be produced for user-selected option
  if (plotSubset %in% c("distributed", "tower")) {

    hbp_agb <- hbp_agb %>%
      dplyr::filter(.data$plotType == plotSubset)

  }

  #   Create 'site x year' variable for identifying all records at sites where a subset of plots are cropped
  hbp_agb <- hbp_agb %>%
    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"),
                  .before = "plotID")



  ### Separate records into grazed, cropped, and standard 'siteID x year' data frames; each requires custom logic to calculate ANPP at the site level

  ##  Step 1: Identify records in all 'siteID x year' combinations that supported grazing
  grazedSiteYearDF <- hbp_agb %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::filter(any(.data$exclosure == "Y")) %>%
    dplyr::ungroup()


  ##  Step 2: Identify records for all 'siteID x year' combinations that contained crops in a plot at any point
  #   Isolate records with mass for a crop of any kind
  temp <- hbp_agb %>%
    dplyr::filter(rowSums(dplyr::across("Barley_gm2":"Wheat_gm2")) > 0)

  #   Filter to all records from crop 'site x year' combos
  cropSiteYearDF <- hbp_agb %>%
    dplyr::filter(.data$siteYear %in% temp$siteYear)

  rm(temp)


  ##  Step 3: Identify all "standard" clips - i.e., no grazing, no crops at any point in a 'siteID x year'
  stdSiteYearDF <- hbp_agb %>%
    dplyr::filter(!.data$siteYear %in% grazedSiteYearDF$siteYear,
                  !.data$siteYear %in% cropSiteYearDF$siteYear)






  ### Standard sites: Calculate plot-level ANPP for 'site x year' combos with no grazing or crops ####
  #--> Sites with multiple Tower plot eventIDs take greatest mass per herbGroup across eventIDs.
  #--> SRER is a special case where mass from multiple eventIDs is summed because species composition during spring green-up does not overlap with species present during monsoon clip.

  if (nrow(stdSiteYearDF)) {

    ### Identify 'site x year' combos with a single Tower plot eventID
    temp <- stdSiteYearDF %>%
      dplyr::filter(.data$plotType == "tower") %>%
      dplyr::group_by(.data$domainID,
                      .data$siteYear) %>%
      dplyr::summarise(events = paste(unique(.data$eventID), collapse = ", "),
                       count = length(unique(.data$eventID))) %>%
      dplyr::filter(count == 1)


    if (nrow(temp)) {

    ##  Process sites with one Tower plot eventID in a 'site x year'

    stdSingleEventProdDF <- stdSiteYearDF %>%
      #   Get records for all plots in 'site x year' combos with a single eventID
      dplyr::filter(.data$siteYear %in% temp$siteYear) %>%

      #   Calculate plot-level ANPP --> averages subplots within 40m x 40m Tower plots
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID,
                      .data$nlcdClass,
                      .data$plotType,
                      .data$plotSize,
                      .data$plotManagement) %>%
      dplyr::summarise(collectDate = as.Date(ifelse(all(is.na(.data$collectDate)), NA, min(.data$collectDate))),
                       TotalMass_gm2 = round(mean(.data$TotalMass_gm2, na.rm = TRUE),
                                             digits = 2),
                       CoolSeasonGram_gm2 = round(mean(.data$CoolSeasonGram_gm2, na.rm = TRUE),
                                                  digits = 2),
                       Forbs_gm2 = round(mean(.data$Forbs_gm2, na.rm = TRUE),
                                         digits = 2),
                       NFixing_gm2 = round(mean(.data$NFixing_gm2, na.rm = TRUE),
                                           digits = 2),
                       WarmSeasonGram_gm2 = round(mean(.data$WarmSeasonGram_gm2, na.rm = TRUE),
                                                  digits = 2),
                       WoodyPlants_gm2 = round(mean(.data$WoodyPlants_gm2, na.rm = TRUE),
                                               digits = 2))

    } else {

      stdSingleEventProdDF <- NULL

    } #   End nrow(temp) conditional checking for site-years with single Tower plot eventID


    ### Process sites with multiple Tower plot eventIDs in a 'site x year' (but not SRER)
    #--> group by siteID, year, herbGroup, and eventID, and pick combo with greatest mass for each herbGroup, then sum herbGroups from selected eventIDs to get total production for each plot.

    ##  Identify 'site x year' combos with multiple Tower plot eventIDs; remove SRER for separate processing
    temp <- stdSiteYearDF %>%
      dplyr::filter(!.data$siteYear %in% temp$siteYear,
                    .data$siteID != "SRER")


    if (nrow(temp)) {

      ##  Isolate and process Distributed plots; these are clipped 1X/year and require no special processing --> data already represent plot-level ANPP. Select columns similar to above in 'stdSingleEventProdDF'
      tempDist <- stdSiteYearDF %>%
        dplyr::filter(.data$siteYear %in% temp$siteYear,
                      .data$plotType == "distributed") %>%
        dplyr::select("domainID",
                      "siteID",
                      "year",
                      "plotID",
                      "nlcdClass",
                      "plotType",
                      "plotSize",
                      "plotManagement",
                      "collectDate",
                      "TotalMass_gm2",
                      "CoolSeasonGram_gm2",
                      "Forbs_gm2",
                      "NFixing_gm2",
                      "WarmSeasonGram_gm2",
                      "WoodyPlants_gm2")


      ##  Process Tower plots clipped > 1X/year
      tempTower <- stdSiteYearDF %>%
        dplyr::filter(.data$siteYear %in% temp$siteYear,
                      .data$plotType == "tower")


      if (nrow(tempTower)) {

        #   Pivot data to long format to enable grouping by herbGroup
        tempTower <- tempTower %>%

          #   Remove crop columns as these are not populated for these site-year combos based on upstream filtering
          dplyr::select(-"TotalMass_gm2",
                        -c("Barley_gm2":"Wheat_gm2")) %>%
          dplyr::rename_with(~ stringr::str_remove(., "_gm2"),
                             .cols = c("CoolSeasonGram_gm2":"WoodyPlants_gm2")) %>%
          tidyr::pivot_longer(cols = c("CoolSeasonGram":"WoodyPlants"),
                              names_to = "herbGroup",
                              values_to = "agb_gm2")

        #   Identify the 'site x year x herbGroup x eventID' combo with greatest productivity; the mean is across all clipIDs
        towerMax <- tempTower %>%
          dplyr::group_by(.data$domainID,
                          .data$siteYear,
                          .data$herbGroup,
                          .data$eventID) %>%
          dplyr::summarise(count = n(),
                           herbMass = round(mean(.data$agb_gm2, na.rm = TRUE),
                                            digits = 2),
                           .groups = "keep") %>%
          dplyr::filter(herbMass == max(.data$herbMass)) %>%
          dplyr::mutate(herbEvent = paste(.data$siteYear, .data$herbGroup, .data$eventID,
                                          sep = "-"),
                        .before = "domainID")

        #   Retain records associated with max 'site x year x herbGroup x eventID' combos, pivot wider, then calculate plot-level ANPP
        tempTower <- tempTower %>%
          dplyr::mutate(herbEvent = paste(.data$siteYear, .data$herbGroup, .data$eventID,
                                          sep = "-"),
                        .before = "domainID") %>%
          dplyr::filter(.data$herbEvent %in% towerMax$herbEvent) %>%
          dplyr::select(-"herbEvent",
                        -"eventID",
                        -"clipID",
                        -"collectDate",
                        -"sampleID") %>%
          tidyr::pivot_wider(names_from = "herbGroup",
                             names_glue = "{herbGroup}_gm2",
                             values_from = "agb_gm2") %>%
          dplyr::mutate(dplyr::across("CoolSeasonGram_gm2":"WoodyPlants_gm2", ~tidyr::replace_na(., 0))) %>%
          dplyr::mutate(TotalMass_gm2 = rowSums(dplyr::across("CoolSeasonGram_gm2":"WoodyPlants_gm2"), na.rm = TRUE),
                        .before = "CoolSeasonGram_gm2") %>%

          #   Calculate mean for 'site x year x plotID' combo to account for potential of 2 subplots per large Tower plot
          dplyr::group_by(.data$domainID,
                          .data$siteID,
                          .data$year,
                          .data$plotID,
                          .data$nlcdClass,
                          .data$plotType,
                          .data$plotSize,
                          .data$plotManagement) %>%
          dplyr::summarise(TotalMass_gm2 = round(mean(.data$TotalMass_gm2, na.rm = TRUE),
                                                 digits = 2),
                           CoolSeasonGram_gm2 = round(mean(.data$CoolSeasonGram_gm2, na.rm = TRUE),
                                                      digits = 2),
                           Forbs_gm2 = round(mean(.data$Forbs_gm2, na.rm = TRUE),
                                             digits = 2),
                           NFixing_gm2 = round(mean(.data$NFixing_gm2, na.rm = TRUE),
                                               digits = 2),
                           WarmSeasonGram_gm2 = round(mean(.data$WarmSeasonGram_gm2, na.rm = TRUE),
                                                      digits = 2),
                           WoodyPlants_gm2 = round(mean(.data$WoodyPlants_gm2, na.rm = TRUE),
                                                   digits = 2),
                           .groups = "drop")


        ##  Combine Distributed and Tower plot ANPP for ungrazed sites with Tower plots clipped > 1x/year (but not SRER)
        stdMultiEventProdDF <- dplyr::bind_rows(tempDist,
                                                tempTower)

      } else {

        stdMultiEventProdDF <- tempDist

      } # End nrow(tempTower) conditional

    } #   End nrow(temp) conditional for processing standard 'site-year' combos with > 1 Tower plot eventID (not SRER)



    ### Process SRER data: Multiple Tower plot eventIDs, herbGroups summed across eventIDs

    ##  Isolate site x year combos from SRER
    srerDF <- stdSiteYearDF %>%
      dplyr::filter(.data$siteID == "SRER")


    ##  Process SRER Distributed plots; these are clipped 1X/year and require no special processing --> data already represent plot-level ANPP. Select columns similar to above.

    srerDist <- srerDF %>%
      dplyr::filter(.data$plotType == "distributed") %>%
      dplyr::select("domainID",
                    "siteID",
                    "year",
                    "plotID",
                    "nlcdClass",
                    "plotType",
                    "plotSize",
                    "plotManagement",
                    "collectDate",
                    "TotalMass_gm2",
                    "CoolSeasonGram_gm2",
                    "Forbs_gm2",
                    "NFixing_gm2",
                    "WarmSeasonGram_gm2",
                    "WoodyPlants_gm2")


    ##  Process SRER Tower plots
    #--> Group by siteID, year, plotID, subplotID, and herbGroup, then sum to get production by herbGroup; then sum across herbGroups to get total production by plotID.

    if("tower" %in% srerDF$plotType) {

    srerTower <- srerDF %>%
      dplyr::filter(.data$plotType == "tower") %>%

      #   Remove crop columns as these are not populated for these site-year combos based on upstream filtering
      dplyr::select(-"TotalMass_gm2",
                    -c("Barley_gm2":"Wheat_gm2")) %>%
      dplyr::rename_with(~ stringr::str_remove(., "_gm2"),
                         .cols = c("CoolSeasonGram_gm2":"WoodyPlants_gm2")) %>%

      #   Pivot data to long format to enable grouping by herbGroup
      tidyr::pivot_longer(cols = c("CoolSeasonGram":"WoodyPlants"),
                          names_to = "herbGroup",
                          values_to = "agb_gm2") %>%

      #   Sum within herbGroups and across bouts
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID,
                      .data$subplotID,
                      .data$herbGroup,
                      .data$nlcdClass,
                      .data$plotType,
                      .data$plotSize,
                      .data$plotManagement) %>%
      dplyr::summarise(agb_gm2 = sum(.data$agb_gm2, na.rm = TRUE),
                       .groups = "drop") %>%

      #   Use pivot_wider to get all herbGroups on same row per subplot to enable plot-level mean calculation
      tidyr::pivot_wider(names_from = "herbGroup",
                         names_glue = "{herbGroup}_gm2",
                         values_from = "agb_gm2") %>%

      dplyr::mutate(TotalMass_gm2 = rowSums(dplyr::across("CoolSeasonGram_gm2":"WoodyPlants_gm2"), na.rm = TRUE),
                    .before = "CoolSeasonGram_gm2") %>%

      #   Calculate mean for 'site x year x plotID' combo to account for 2 subplots per large Tower plot at SRER
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID,
                      .data$nlcdClass,
                      .data$plotType,
                      .data$plotSize,
                      .data$plotManagement) %>%
      dplyr::summarise(TotalMass_gm2 = round(mean(.data$TotalMass_gm2, na.rm = TRUE),
                                             digits = 2),
                       CoolSeasonGram_gm2 = round(mean(.data$CoolSeasonGram_gm2, na.rm = TRUE),
                                                  digits = 2),
                       Forbs_gm2 = round(mean(.data$Forbs_gm2, na.rm = TRUE),
                                         digits = 2),
                       NFixing_gm2 = round(mean(.data$NFixing_gm2, na.rm = TRUE),
                                           digits = 2),
                       WarmSeasonGram_gm2 = round(mean(.data$WarmSeasonGram_gm2, na.rm = TRUE),
                                                  digits = 2),
                       WoodyPlants_gm2 = round(mean(.data$WoodyPlants_gm2, na.rm = TRUE),
                                               digits = 2),
                       .groups = "drop")

    #   Bind SRER Tower output with Dist output
    srerMultiEventProdDF <- dplyr::bind_rows(srerDist,
                                             srerTower)

    } else {

      srerMultiEventProdDF <- srerDist

    } # End Tower SRER conditional



    ### Combine outputs from standard single Tower event, standard multi Tower event, and SRER
    stdSiteYearDF <- dplyr::bind_rows(stdSingleEventProdDF,
                                      stdMultiEventProdDF,
                                      srerMultiEventProdDF) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotID)



  ### Create a NULL data frame named identically to the above output in the event no "standard" sites exist in input dataset
  } else {

    stdSiteYearDF <- NULL

  } #   End nrow(stdSiteYearDF) standard site processing conditional



  ### Clean up following "standard" site processing
  rm(srerDF, srerDist, srerTower, tempDist, tempTower, towerMax)




  ### Ag sites: Calculate plot-level ANPP for 'site x year' combos with crops in at least one plot ####
  #--> Identify plots that contain a crop within a site-year, sum production across all bouts within a site-year for these plots. Distributed plots treated the same as Tower plots when cropped.
  #--> For plots with no crops in a site-year, identify bout with greatest mass across all herbGroups as plot-level productivity.

  if (nrow(cropSiteYearDF)) {

    ### Parse plots into cropped vs cropless in a given site-year
    cropSiteYearDF <- cropSiteYearDF %>%
      dplyr::mutate(plotSiteYear = paste(.data$siteYear, .data$plotID,
                                         sep = "-"),
                    .after = "siteYear")

    #   Isolate records with mass for a crop of any kind
    temp <- cropSiteYearDF %>%
      dplyr::filter(rowSums(dplyr::across("Barley_gm2":"Wheat_gm2")) > 0)

    cropPlots <- cropSiteYearDF %>%
      dplyr::filter(.data$plotSiteYear %in% temp$plotSiteYear)

    #   Isolate records for cropless plots
    croplessPlots <- cropSiteYearDF %>%
      dplyr::filter(!.data$plotSiteYear %in% temp$plotSiteYear)



    ### Process crop plots to obtain plot-level annual production
    #--> Group by plot-site-year and herbGroup and sum, average across subplotIDs, and sum across herbGroups to get total production

    if (nrow(cropPlots)) {

      #   Pivot to long format to enable grouping by herbGroup; remove "TotalMass" column, will be re-calculated later
      cropPlots <- cropPlots %>%
        dplyr::select(-"TotalMass_gm2") %>%
        dplyr::rename_with(~ stringr::str_remove(., "_gm2"),
                           .cols = c("CoolSeasonGram_gm2":"Wheat_gm2")) %>%
        tidyr::pivot_longer(cols = c("CoolSeasonGram":"Wheat"),
                            names_to = "herbGroup",
                            values_to = "agb_gm2") %>%

        #   Group by site, year, plotID, subplotID, and herbGroup and sum across multiple potential eventIDs to get productivity by herbGroup for the entire year at the subplotID level
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotID,
                        .data$subplotID,
                        .data$herbGroup,
                        .data$nlcdClass,
                        .data$plotType,
                        .data$plotSize,
                        .data$plotManagement) %>%
        dplyr::summarise(agb_gm2 = sum(.data$agb_gm2, na.rm = TRUE),
                         .groups = "drop") %>%

        #   Average across multiple potential subplotIDs to get mean plot-level production by herbGroup
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotID,
                        .data$herbGroup,
                        .data$nlcdClass,
                        .data$plotType,
                        .data$plotSize,
                        .data$plotManagement) %>%
        dplyr::summarise(agb_gm2 = mean(.data$agb_gm2, na.rm = TRUE),
                         .groups = "drop") %>%

        #   Use pivot_wider and rowSums to get one row per plot with columns per herbGroup, and calculate plot-level productivity
        tidyr::pivot_wider(names_from = "herbGroup",
                           names_glue = "{herbGroup}_gm2",
                           values_from = "agb_gm2") %>%
        dplyr::relocate("Barley_gm2",
                        "Corn_gm2",
                        "Millet_gm2",
                        "Oat_gm2",
                        "OrchardGrass_gm2",
                        "Rye_gm2",
                        "Sorghum_gm2",
                        "Soybean_gm2",
                        "Sunflower_gm2",
                        "Wheat_gm2",
                        .after = "WoodyPlants_gm2") %>%
        dplyr::mutate(TotalMass_gm2 = rowSums(dplyr::across("CoolSeasonGram_gm2":"Wheat_gm2")),
                      .after = "plotManagement")

    } else {

      cropPlots <- NULL

    } # End nrow(cropPlots) conditional



    ### Process cropless plots at sites with crops to obtain plot-level annual production
    #--> Should be only one eventID per site-year for these plots, but assume there may be multiple bouts in the data to ensure code is robust to data irregularities.
    #--> In a given site x year, identify for each herbGroup which eventID has the greatest productivity; mean herbGroup productivity is calculated across all clipIDs.

    if (nrow(croplessPlots)) {

      #   Remove crop columns as these are not populated for croplessPlots by definition
      croplessPlots <- croplessPlots %>%
        dplyr::select(-"TotalMass_gm2",
                      -c("Barley_gm2":"Wheat_gm2")) %>%

        #   Pivot data to long format to enable grouping by herbGroup
        dplyr::rename_with(~ stringr::str_remove(., "_gm2"),
                           .cols = c("CoolSeasonGram_gm2":"WoodyPlants_gm2")) %>%
        tidyr::pivot_longer(cols = c("CoolSeasonGram":"WoodyPlants"),
                            names_to = "herbGroup",
                            values_to = "agb_gm2")

      #   Identify the 'site x year x plotType x herbGroup x eventID' combo with greatest productivity; the mean is across all clipIDs
      croplessMax <- croplessPlots %>%
        dplyr::group_by(.data$domainID,
                        .data$siteYear,
                        .data$plotType,
                        .data$herbGroup,
                        .data$eventID) %>%
        dplyr::summarise(count = n(),
                         herbMass = round(mean(.data$agb_gm2, na.rm = TRUE),
                                          digits = 2),
                         .groups = "keep") %>%
        dplyr::filter(herbMass == max(.data$herbMass)) %>%

        #   Create concatenated identifier for the plotType-herb-event combos with greatest mass
        dplyr::mutate(herbEvent = paste(.data$siteYear, .data$plotType, .data$herbGroup, .data$eventID,
                                        sep = "-"),
                      .before = "domainID")

      #   Retain records associated with plotType-herb-event combos with greatest mass, average across subplotIDs within plots to get plot-level means by herbGroup, then pivot wider and calculate plot-level ANPP with rowSums
      croplessPlots <- croplessPlots %>%
        dplyr::mutate(herbEvent = paste(.data$siteYear, .data$plotType, .data$herbGroup, .data$eventID,
                                        sep = "-"),
                      .before = "domainID") %>%
        dplyr::filter(.data$herbEvent %in% croplessMax$herbEvent) %>%
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotID,
                        .data$herbGroup,
                        .data$nlcdClass,
                        .data$plotType,
                        .data$plotSize,
                        .data$plotManagement) %>%
        dplyr::summarise(agb_gm2 = mean(.data$agb_gm2, na.rm = TRUE),
                         .groups = "drop") %>%

        #   Use pivot_wider and rowSums to get one row per plot with columns per herbGroup, and calculate plot-level productivity
        tidyr::pivot_wider(names_from = "herbGroup",
                           names_glue = "{herbGroup}_gm2",
                           values_from = "agb_gm2") %>%
        dplyr::mutate(TotalMass_gm2 = rowSums(dplyr::across("CoolSeasonGram_gm2":"WoodyPlants_gm2")),
                      .after = "plotManagement")

    } else {

      croplessPlots <- NULL

    } # End nrow(croplessPlots) conditional



    ### Combine results from cropped and cropless plots
    cropSiteYearDF <- dplyr::bind_rows(cropPlots,
                                       croplessPlots) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotID) %>%

      # Replace NAs for cropless plots in crop columns with zero
      dplyr::mutate(dplyr::across("CoolSeasonGram_gm2":"Wheat_gm2", ~tidyr::replace_na(., 0)))

  } else {

    cropSiteYearDF <- NULL

  } # End nrow(cropSiteYearDF) conditional



  ### Clean up following "crop" site processing
  rm(croplessMax, croplessPlots, cropLong, cropPlots)



  ### Grazed sites: Calculate site-level ANPP for 'site x year' combos with grazing in at least one plot ####
  #--> For plots with exclosure == "Y" at any point in a site-year, treat as a grazed plot; this means that when a clipID under a damaged exclosure is not clipped, the "ambient" clip is still used to estimate grazing consumption.
  #--> Treat plots with no exclosure at any point in a site-year as ungrazed, and take the eventID with the greatest mean mass across all plots as the eventID that equals ANPP.

  if (nrow(grazedSiteYearDF)) {

    #   Remove crop columns as not relevant to grazed sites
    grazedSiteYearDF <- grazedSiteYearDF %>%
      dplyr::select(-c("Barley_gm2":"Wheat_gm2"))


    ##  Identify grazed plots
    grazedPlots <- grazedSiteYearDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID,
                      .data$nlcdClass,
                      .data$plotType,
                      .data$plotSize,
                      .data$plotManagement) %>%
      dplyr::filter(any(.data$exclosure == "Y")) %>%
      dplyr::ungroup() %>%

      #   Create plotSiteYear variable to enable parsing plots into grazed vs ungrazed
      dplyr::mutate(plotSiteYear = paste(.data$siteID, .data$year, .data$plotID,
                                         sep = "-"),
                    .after = "siteYear")


    ##  Identify ungrazed plots at sites managed for grazing
    grazelessPlots <- grazedSiteYearDF %>%
      dplyr::mutate(plotSiteYear = paste(.data$siteID, .data$year, .data$plotID,
                                         sep = "-"),
                    .after = "siteYear") %>%
      dplyr::filter(!.data$plotSiteYear %in% grazedPlots$plotSiteYear)



    ### Process grazed plots for ANPP

    ##  Identify latest eventID for each 'site' x 'year' combination; these are used to estimate ambient biomass at last sampling eventID of the year.
    grazedLatestEvents <- grazedPlots %>%
      dplyr::filter(.data$plotType == "tower") %>%
      dplyr::distinct(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$eventID) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year) %>%
      dplyr::arrange(.data$eventID) %>%
      dplyr::slice_tail() %>%
      dplyr::mutate(finalSiteYearEvent = paste(.data$siteID, .data$year, .data$eventID,
                                               sep = "-"),
                    .after = "year")


    ##  Determine final ambient biomass for latest events
    grazedFinalMass <- grazedPlots %>%
      dplyr::mutate(finalSiteYearEvent = paste(.data$siteID, .data$year, .data$eventID,
                                               sep = "-"),
                    .after = "year") %>%
      dplyr::filter(.data$finalSiteYearEvent %in% grazedLatestEvents$finalSiteYearEvent,
                    .data$exclosure == "N") %>%
      dplyr::select(-c("CoolSeasonGram_gm2":"WoodyPlants_gm2")) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotID,
                     .data$subplotID) %>%

      #   Generate means and SD for final ambient biomass
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$eventID,
                      .data$plotType,
                      .data$plotSize,
                      .data$plotManagement) %>%
      dplyr::summarise(finalClipCount = dplyr::n(),
                       finalAGBMean_gm2 = round(mean(.data$TotalMass_gm2, na.rm = TRUE),
                                                digits = 2),
                       finalAGBSD_gm2 = round(stats::sd(.data$TotalMass_gm2, na.rm = TRUE),
                                              digits = 2),
                       .groups = "drop") %>%
      dplyr::rename("finalEventID" = "eventID")



    ### Calculate consumption as difference between exclosure == "Y" and exclosure == "N" for all other eventIDs
    #--> Begin again here: Use logic in chunk beginning line 825












  } else {

    grazedSiteYearDF <- NULL

  } # End nrow(grazedSiteYearDF) conditional























  ### Standard sites: Calculate site-level productivity and SD by 'year' and 'herbGroup' ####

  #   Filter to "standard" plot data: Sites with no grazing, Distributed plots at grazed sites, Tower plots at grazed sites but that are not grazed. Also remove rows for herbGroups with "NA" mass (mostly crops).
  standardFinalMass <- hbp_agb_long %>%
    dplyr::filter(!.data$siteID %in% grazed_sites | (.data$siteID %in% grazed_sites & .data$plotType == "distributed") |
                    (.data$siteID %in% grazed_sites & .data$plotType == "tower" & !.data$plotYear %in% grazedPlotYears),
                  !is.na(.data$agb_gm2))

  #   Identify latest eventID for each 'site' x 'year' x 'plotType' combination
  standardLatestEvents <- standardFinalMass %>%
    dplyr::distinct(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$plotType,
                    .data$eventID) %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$year,
                    .data$plotType) %>%
    dplyr::arrange(.data$eventID) %>%
    dplyr::slice_tail()

  #   Further filter to latest "standard" eventID --> should be redundant with filtering above unless Tower plot was clipped more than once and never had an exclosure in it for entire 'plot' x 'year' combination
  test <- standardFinalMass %>%
    dplyr::filter(!.data$eventID %in% standardLatestEvents$eventID)

  standardFinalMass <- standardFinalMass %>%
    dplyr::filter(.data$eventID %in% standardLatestEvents$eventID)
  #--> This doesn't quite work right for plots that don't have exclosure but are sampled for all the grazed bouts anyway (happened at SJER in 2017)...


  #   Create site-level ANPP estimates for "standard" sites/plots
  if (nrow(standardFinalMass) > 0) {

    herb_ANPP_site <- standardFinalMass %>%
      dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotType,
                      .data$eventID,
                      .data$herbGroup) %>%
      dplyr::summarise(herbClipCount = dplyr::n(),
                       herbANPP_gm2yr = round(mean(.data$agb_gm2, na.rm = TRUE),
                                              digits = 2),
                       herbANPPSD_gm2yr = round(stats::sd(.data$agb_gm2, na.rm = TRUE),
                                                digits = 2))

  } else {

    herb_ANPP_site <- data.frame()

  }









  ### Grazed sites: Estimate consumption to add to finalStandingMass ####

    ### Calculate consumption productivity component using exclosure data
    #   Calculate mean exclosure == "Y" and exclosure == "N" mass across clip samples by eventID
    exclosure <- hbp_agb_long %>%
      dplyr::filter(.data$herbGroup == "AllHerbaceousPlants",
                    .data$siteID %in% grazed_sites,
                    .data$plotType == "tower",
                    !is.na(.data$agb_gm2),
                    .data$plotYear %in% grazedPlotYears) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$eventID,
                      .data$peak,
                      .data$herbGroup,
                      .data$exclosure) %>%
      dplyr::summarise("clipCount" = dplyr::n(),
                       "agbMean_gm2" = round(mean(.data$agb_gm2, na.rm = TRUE),
                                             digits = 2),
                       "agbSD_gm2" = round(stats::sd(.data$agb_gm2, na.rm = TRUE),
                                           digits = 2),
                       .groups = "drop") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$eventID)

    #   Calculate consumption mean and SD per eventID
    #--> Uncertainties combined according to: https://www.mathbench.umd.edu/modules/statistical-tests_t-tests/page06.htm

    eventConsum <- exclosure %>%
      tidyr::pivot_wider(names_from = "exclosure",
                         values_from = c("clipCount" , "agbMean_gm2", "agbSD_gm2"),
                         names_prefix = "excl") %>%
      dplyr::mutate("consumClipCount" = .data$clipCount_exclN + .data$clipCount_exclY,
                    "consumMean_gm2" = round(.data$agbMean_gm2_exclY - .data$agbMean_gm2_exclN,
                                             digits = 2),
                    "consumSD_gm2" = round(sqrt((.data$agbSD_gm2_exclN^2 / .data$clipCount_exclN) +
                                                  (.data$agbSD_gm2_exclY^2 / .data$clipCount_exclY)),
                                           digits = 2),
                    "consumSD2_N" = round(.data$consumSD_gm2^2 / .data$consumClipCount,
                                          digits = 2))


    ##  Sum consumption for all events per site and year
    siteConsum <- eventConsum %>%
      dplyr::filter(!is.na(.data$consumMean_gm2)) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$herbGroup) %>%
      dplyr::summarise("consumEventCount" = dplyr::n(),
                       "consumClipCount" = sum(.data$consumClipCount),
                       "consumption_gm2" = round(sum(.data$consumMean_gm2, na.rm = TRUE),
                                                 digits = 2),
                       "consumptionSD_gm2" = round(sqrt(sum(.data$consumSD2_N, na.rm = TRUE)),
                                                   digits = 2),
                       .groups = "drop")


    ##  Join with 'grazedFinalMass' to calculate herbaceous ANPP at grazed sites
    herb_ANPP_grazed <- dplyr::left_join(grazedFinalMass,
                                         siteConsum,
                                         by = c("domainID", "siteID", "year", "herbGroup")) %>%
      dplyr::mutate(herbClipCount = .data$finalClipCount + .data$consumClipCount,
                    herbANPP_gm2yr = rowSums(dplyr::across(c("finalAGBMean_gm2", "consumption_gm2")),
                                             na.rm = TRUE),
                    herbANPPSD_gm2yr = round(sqrt((.data$finalAGBSD_gm2^2 / .data$finalClipCount) +
                                                    (.data$consumptionSD_gm2^2 / .data$consumClipCount)),
                                             digits = 2),
                    .after = "herbGroup")


  } else {

    herb_ANPP_grazed <- data.frame()

  } # end nrow(grazedFinalMass) conditional



  ### Site-level output: Bind 'standard' and 'grazed' data ####

  herb_ANPP_site <- dplyr::bind_rows(herb_ANPP_site,
                                     herb_ANPP_grazed) %>%
    dplyr::relocate("finalEventID",
                    .after = "eventID")

  #   Add columns with "Mg/ha/y" units for ANPP and SD and arrange
  herb_ANPP_site <- herb_ANPP_site %>%
    dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                          digits = 2),
                  herbANPPSD_Mghayr = round(.data$herbANPPSD_gm2yr * 10000 * 0.000001,
                                            digits = 2),
                  .after = "herbClipCount") %>%
    dplyr::arrange(.data$domainID,
                   .data$siteID,
                   .data$year,
                   .data$plotType)



  ### Plot-level output: Finalize dataframe ####
  #   Create plot-level output for herbGroup = "AllHerbaceousPlants"
  herb_ANPP_plot <- standardFinalMass %>%
    dplyr::filter(.data$herbGroup == "AllHerbaceousPlants") %>%
    dplyr::rename("herbANPP_gm2yr" = "agb_gm2") %>%
    dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                          digits = 2),
                  .after = "exclosure")

  #   Create plot-level output for all herbGroups (but not "AllHerbaceousPlants")
  herb_ANPP_plot_herbgroup <- standardFinalMass %>%
    dplyr::filter(.data$herbGroup != "AllHerbaceousPlants") %>%
    dplyr::rename("herbANPP_gm2yr" = "agb_gm2") %>%
    dplyr::mutate(herbANPP_Mghayr = round(.data$herbANPP_gm2yr * 10000 * 0.000001,
                                          digits = 2),
                  .after = "exclosure")



  ### Output data
  output <- list(herb_ANPP_site = herb_ANPP_site,
                 herb_ANPP_plot = herb_ANPP_plot,
                 herb_ANPP_plot_herbgroup = herb_ANPP_plot_herbgroup)

  return(output)
}
