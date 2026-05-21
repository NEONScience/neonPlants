#' @title Estimate herbaceous ANPP (Above-ground Net Primary Productivity) at NEON sites
#'
#' @author
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Estimate above-ground herbaceous productivity at NEON sites using Herbaceous Clip Harvest data tables (DP1.10023.001).
#'
#' @details Input data should be a list of herbaceous biomass (DP1.10023.001) data frames downloaded via the neonUtilities::loadByProduct() function. The companion scaleHerbMass() function is called internally, and outputs from this function are used to estimate herbaceous productivity.
#'
#' At sites where herbaceous plants are not subject to grazing management and exclosures are not in use, and crops are not planted at any point during the year, the estimate of herbaceous ANPP is the sum of the standing biomass clipped from the bout with the greatest biomass for each herbGroup. For example, if there is an early-season and late-season bout at a site, and Cool Season Graminoids have greatest mass early-season and Warm Season Graminoids have greatest mass late-season, and both of these herbGroups have mass in each bout, the productivity from these two groups would be the sum of early-season Cool Season Graminoid and late-season Warm Season Graminoid biomass. The exception is the Domain 14 Santa Rita Experimental Range site, where biomass from all bouts and all herbGroups is summed to estimate productivity because early-season and late-season biomass is derived from non-overlapping species pools.
#'
#' Where grazing management occurs in Tower plots and exclosures have been established, consumption is calculated for each Tower plot sampling event as average biomass within exclosures minus average biomass outside of exclosures; the consumption estimate is therefore inherently a "site-level" estimate rather than a "plot-level" estimate. Site-level consumption from each sampling bout is then summed across all bouts and added to the standing biomass of the last bout of the season to generate site-level estimates of herbaceous ANPP. For grazed sites, the Science Design only supports calculating consumption at the site level. The clip harvests within exclosures are not close enough to ambient clip harvests to support plot-level consumption estimates. Where biomass production is spatially heterogeneous and stocking rates are low, consumption estimates can be negative.
#'
#' At sites where crops are planted, productivity in cropped plots is determined on a plot-by-plot basis as the sum of all biomass from every bout within a given 'site x year' combination; this approach sums the productivity from multiple potential plantings throughout the season. Productivity from "wild-type" plots at cropped sites that are not planted with a crop in a given site-year is estimated using the same rubric described above for sites not subject to grazing and with no crops.
#'
#' @param inputDataList An R list object produced by the neonUtilities::loadByProduct() function for the NEON Herbaceous Clip Harvest data product. [list]
#'
#' @param plotSubset The options are the default "all" (all Tower and Distributed plots), "tower" (all plots in the Tower airshed but no Distributed plots), and "distributed" (all Distributed plots, which are sampled on a 5-year interval and are spatially representative of the NLCD classes at a site, and no Tower plots). [character]
#'
#' @return A list that includes productivity summary outputs and additional productivity details from grazed sites. Output tables include:
#'   * herb_ANPP_site - Summarizes herbaceous ANPP for each site x year combination ("Mg/ha/yr" and "g/m2/yr").
#'   * herb_ANPP_plot - Summarizes herbaceous ANPP for all herbaceous plants for each plot x year combination ("Mg/ha/yr" and "g/m2/yr"). Plot-level summaries are not returned for grazed Tower plots, for the reason outlined in the 'details' section above.
#'   * herb_ANPP_grazed_extra - Provides summary information about each of the components required to calculate herbaceous ANPP from Tower plots at grazed sites - i.e., total estimated consumption across all bouts and final standing biomass from grazed Tower plots, as well as the mean productivity contributed from ungrazed Tower plots at grazed sites.
#'   * herb_grazed_consumption - Detailed per bout mass data from exclosure = "Y" and exclosure = "N" clip harvests ("g/m2/yr"), and derived bout-level consumption data for each grazed site ("g/m2/yr"). These data are useful to understand how consumption estimates and total herbaceous ANPP at grazed sites was derived.
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Herbaceous clip harvest data
#' HbpDat <- neonUtilities::loadByProduct(
#' dpID = "DP1.10023.001",
#' site = c("HARV", "CPER")
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

  #   Remove records with NA mass values; these are output from scaleHerbMass when targetTaxaPresent == "Y" in hbp_perbout but hbp_massdata records are missing
  hbp_agb <- hbp_agb %>%
    dplyr::filter(!is.na(.data$TotalMass_gm2))

  #   Create 'site x year' variable for identifying all records with a given site-year combination
  hbp_agb <- hbp_agb %>%
    dplyr::mutate(siteYear = paste(.data$siteID, .data$year, sep = "-"),
                  .before = "plotID")

  #   Standardize column names for productivity output
  hbp_agb <- hbp_agb %>%
    dplyr::rename("totalProd_gm2yr" = "TotalMass_gm2") %>%
    dplyr::rename_with(~ paste0(.x, "yr"), .cols = "CoolSeasonGram_gm2":"Wheat_gm2")



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
    dplyr::filter(rowSums(dplyr::across("Barley_gm2yr":"Wheat_gm2yr")) > 0)

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
                       count = length(unique(.data$eventID)),
                       .groups = "drop") %>%
      dplyr::filter(count == 1)


    if (nrow(temp)) {

    ##  Process sites with one Tower plot eventID in a 'site x year'

    stdSingleEventProdDF <- stdSiteYearDF %>%
      #   Get records for both Distributed and Tower plots in 'site x year' combos with a single eventID
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
                       clipCount = n(),
                       totalProd_gm2yr = round(mean(.data$totalProd_gm2yr, na.rm = TRUE),
                                             digits = 2),
                       CoolSeasonGram_gm2yr = round(mean(.data$CoolSeasonGram_gm2yr, na.rm = TRUE),
                                                  digits = 2),
                       Forbs_gm2yr = round(mean(.data$Forbs_gm2yr, na.rm = TRUE),
                                         digits = 2),
                       NFixing_gm2yr = round(mean(.data$NFixing_gm2yr, na.rm = TRUE),
                                           digits = 2),
                       WarmSeasonGram_gm2yr = round(mean(.data$WarmSeasonGram_gm2yr, na.rm = TRUE),
                                                  digits = 2),
                       WoodyPlants_gm2yr = round(mean(.data$WoodyPlants_gm2yr, na.rm = TRUE),
                                               digits = 2),
                       .groups = "drop")

    } else {

      #   Create empty data frame that can be used with bind_rows() after other "standard" site-years are processed
      stdSingleEventProdDF <- tibble::tibble(domainID = character(0))

    } #   End nrow(temp) conditional checking for site-years with single Tower plot eventID



    ### Process sites with multiple Tower plot eventIDs in a 'site x year' (but not SRER)
    #--> group by siteID, year, herbGroup, and eventID, and pick combo with greatest mass for each herbGroup, then sum herbGroups from selected eventIDs to get total production for each plot.

    ##  Identify 'site x year' combos with multiple Tower plot eventIDs; remove SRER for separate processing
    temp <- stdSiteYearDF %>%
      dplyr::filter(!.data$siteYear %in% temp$siteYear,
                    .data$siteID != "SRER")


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
                    c("totalProd_gm2yr":"WoodyPlants_gm2yr")) %>%
      dplyr::mutate(clipCount = 1,
                    .after = "collectDate")


    ##  Process Tower plots clipped > 1X/year
    tempTower <- stdSiteYearDF %>%
      dplyr::filter(.data$siteYear %in% temp$siteYear,
                    .data$plotType == "tower")

    #   Conditionally process Tower plots
    if (nrow(tempTower)) {

      #   Pivot data to long format to enable grouping by herbGroup. Remove 'TotalMass_gm2' as this is not a true herbGroup; remove crop columns as these are not populated for these site-year combos based on upstream filtering.
      tempTower <- tempTower %>%
        dplyr::select(-"totalProd_gm2yr",
                      -c("Barley_gm2yr":"Wheat_gm2yr")) %>%
        dplyr::rename_with(~ stringr::str_remove(., "_gm2yr"),
                           .cols = c("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr")) %>%
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
                         .groups = "drop") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteYear,
                        .data$herbGroup) %>%
        dplyr::filter(herbMass == max(.data$herbMass)) %>%
        dplyr::slice_max(order_by = .data$count,
                         n = 1) %>%
        dplyr::mutate(herbEvent = paste(.data$siteYear, .data$herbGroup, .data$eventID,
                                        sep = "-"),
                      .before = "domainID") %>%
        dplyr::ungroup()

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
                           names_glue = "{herbGroup}_gm2yr",
                           values_from = "agb_gm2") %>%
        dplyr::mutate(dplyr::across("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr", ~tidyr::replace_na(., 0))) %>%
        dplyr::mutate(totalProd_gm2yr = rowSums(dplyr::across("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr"), na.rm = TRUE),
                      .before = "CoolSeasonGram_gm2yr") %>%

        #   Calculate mean for 'site x year x plotID' combo to account for potential of 2 subplots per large Tower plot
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotID,
                        .data$nlcdClass,
                        .data$plotType,
                        .data$plotSize,
                        .data$plotManagement) %>%
        dplyr::summarise(clipCount = n(),
                         totalProd_gm2yr = round(mean(.data$totalProd_gm2yr, na.rm = TRUE),
                                                 digits = 2),
                         CoolSeasonGram_gm2yr = round(mean(.data$CoolSeasonGram_gm2yr, na.rm = TRUE),
                                                      digits = 2),
                         Forbs_gm2yr = round(mean(.data$Forbs_gm2yr, na.rm = TRUE),
                                             digits = 2),
                         NFixing_gm2yr = round(mean(.data$NFixing_gm2yr, na.rm = TRUE),
                                               digits = 2),
                         WarmSeasonGram_gm2yr = round(mean(.data$WarmSeasonGram_gm2yr, na.rm = TRUE),
                                                      digits = 2),
                         WoodyPlants_gm2yr = round(mean(.data$WoodyPlants_gm2yr, na.rm = TRUE),
                                                   digits = 2),
                         .groups = "drop")


      ##  Combine Distributed and Tower plot ANPP for ungrazed sites with Tower plots clipped > 1x/year (but not SRER)
      stdMultiEventProdDF <- dplyr::bind_rows(tempDist,
                                              tempTower)

    } else {

      stdMultiEventProdDF <- tempDist

    } # End nrow(tempTower) conditional for processing standard 'site-year' combos with > 1 Tower plot eventID (not SRER)



    ### Process SRER data: Multiple Tower plot eventIDs, herbGroups summed across eventIDs

    ##  Isolate site x year combos from SRER
    srerDF <- stdSiteYearDF %>%
      dplyr::filter(.data$siteID == "SRER")


    ##  Process SRER Distributed plots; these are clipped 1X/year and require no special processing. Select columns similar to above.
    #--> Note that Distributed plots are not clipped early-season so plot-level ANPP may be underestimated due to some loss of early-season biomass by the time the plots are clipped during peak green.

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
                    c("totalProd_gm2yr":"WoodyPlants_gm2yr")) %>%
      dplyr::mutate(clipCount = n(),
                    .after = "collectDate")


    ##  Process SRER Tower plots
    #--> Group by siteID, year, plotID, subplotID, and herbGroup, then sum to get production by herbGroup; then sum across herbGroups to get total production by plotID.
    srerTower <- srerDF %>%
      dplyr::filter(.data$plotType == "tower")

    #   Conditionally process SRER Tower plots
    if(nrow(srerTower)) {

      srerTower <- srerTower %>%

        #   Remove crop columns as these are not populated for these site-year combos based on upstream filtering
        dplyr::select(-"totalProd_gm2yr",
                      -c("Barley_gm2yr":"Wheat_gm2yr")) %>%
        dplyr::rename_with(~ stringr::str_remove(., "_gm2yr"),
                           .cols = c("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr")) %>%

        #   Pivot data to long format to enable grouping by herbGroup
        tidyr::pivot_longer(cols = c("CoolSeasonGram":"WoodyPlants"),
                            names_to = "herbGroup",
                            values_to = "agb_gm2") %>%

        #   Sum each herbGroup and across bouts
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
                           names_glue = "{herbGroup}_gm2yr",
                           values_from = "agb_gm2") %>%

        dplyr::mutate(totalProd_gm2yr = rowSums(dplyr::across("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr"), na.rm = TRUE),
                      .before = "CoolSeasonGram_gm2yr") %>%

        #   Calculate mean for 'site x year x plotID' combo to account for 2 subplots per large Tower plot at SRER
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotID,
                        .data$nlcdClass,
                        .data$plotType,
                        .data$plotSize,
                        .data$plotManagement) %>%
        dplyr::summarise(clipCount = n(),
                         totalProd_gm2yr = round(mean(.data$totalProd_gm2yr, na.rm = TRUE),
                                                 digits = 2),
                         CoolSeasonGram_gm2yr = round(mean(.data$CoolSeasonGram_gm2yr, na.rm = TRUE),
                                                      digits = 2),
                         Forbs_gm2yr = round(mean(.data$Forbs_gm2yr, na.rm = TRUE),
                                             digits = 2),
                         NFixing_gm2yr = round(mean(.data$NFixing_gm2yr, na.rm = TRUE),
                                               digits = 2),
                         WarmSeasonGram_gm2yr = round(mean(.data$WarmSeasonGram_gm2yr, na.rm = TRUE),
                                                      digits = 2),
                         WoodyPlants_gm2yr = round(mean(.data$WoodyPlants_gm2yr, na.rm = TRUE),
                                                   digits = 2),
                         .groups = "drop")

      #   Bind SRER Tower output with Dist output
      srerMultiEventProdDF <- dplyr::bind_rows(srerDist,
                                               srerTower)

    } else {

      srerMultiEventProdDF <- srerDist

    } # End nrow(srerTower) conditional



    ### Combine outputs from standard single Tower event, standard multi Tower event, and SRER
    stdSiteYearDF <- dplyr::bind_rows(stdSingleEventProdDF,
                                      stdMultiEventProdDF,
                                      srerMultiEventProdDF) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotType,
                     .data$plotID)



  ### Create a NULL data frame named identically to the above output in the event no "standard" sites exist in input dataset
  } else {

    stdSiteYearDF <- NULL

  } #   End nrow(stdSiteYearDF) standard site processing conditional






  ### Ag sites: Calculate plot-level ANPP for 'site x year' combos with crops in at least one plot ####
  #--> Identify plots that contain a crop within a site-year, sum production across all bouts within a site-year for these plots. Distributed plots treated the same as Tower plots when cropped.
  #--> For plots with no crops in a site-year, identify bout with greatest mass across all herbGroups as plot-level productivity.

  if (nrow(cropSiteYearDF)) {

    ### Account for sites where Tower plots are grazed and Distributed plots have crops --> filter out grazed Tower plots
    #--> Occurs at LAJA in 2020
    cropSiteYearDF <- cropSiteYearDF %>%
      dplyr::filter(!(.data$plotType == "tower" & .data$siteYear %in% grazedSiteYearDF$siteYear))



    ### Parse plots into cropped vs cropless in a given site-year
    cropSiteYearDF <- cropSiteYearDF %>%
      dplyr::mutate(plotSiteYear = paste(.data$siteYear, .data$plotID,
                                         sep = "-"),
                    .after = "siteYear")

    #   Isolate records with mass for a crop of any kind
    temp <- cropSiteYearDF %>%
      dplyr::filter(rowSums(dplyr::across("Barley_gm2yr":"Wheat_gm2yr")) > 0)

    cropPlots <- cropSiteYearDF %>%
      dplyr::filter(.data$plotSiteYear %in% temp$plotSiteYear)

    #   Isolate records for cropless plots
    croplessPlots <- cropSiteYearDF %>%
      dplyr::filter(!.data$plotSiteYear %in% temp$plotSiteYear)



    ### Process crop plots to obtain plot-level annual production
    #--> Group by plot-site-year and herbGroup and sum, average across subplotIDs, and sum across herbGroups to get total production

    if (nrow(cropPlots)) {

      #   Pivot to long format to enable grouping by herbGroup; remove "TotalMass_gm2" column as not an herbGroup
      cropPlots <- cropPlots %>%
        dplyr::select(-"totalProd_gm2yr") %>%
        dplyr::rename_with(~ stringr::str_remove(., "_gm2yr"),
                           .cols = c("CoolSeasonGram_gm2yr":"Wheat_gm2yr")) %>%
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
        dplyr::summarise(clipCount = n(),
                         agb_gm2 = sum(.data$agb_gm2, na.rm = TRUE),
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
        dplyr::summarise(clipCount = sum(.data$clipCount),
                         agb_gm2 = round(mean(.data$agb_gm2, na.rm = TRUE),
                                         digits = 2),
                         .groups = "drop") %>%

        #   Use pivot_wider and rowSums to get one row per plot with columns per herbGroup, and calculate plot-level productivity
        tidyr::pivot_wider(names_from = "herbGroup",
                           names_glue = "{herbGroup}_gm2yr",
                           values_from = "agb_gm2") %>%
        dplyr::relocate("Barley_gm2yr",
                        "Corn_gm2yr",
                        "Millet_gm2yr",
                        "Oat_gm2yr",
                        "OrchardGrass_gm2yr",
                        "Rye_gm2yr",
                        "Sorghum_gm2yr",
                        "Soybean_gm2yr",
                        "Sunflower_gm2yr",
                        "Wheat_gm2yr",
                        .after = "WoodyPlants_gm2yr") %>%
        #   Replace NAs in columns with zero
        dplyr::mutate(dplyr::across("CoolSeasonGram_gm2yr":"Wheat_gm2yr", ~tidyr::replace_na(., 0))) %>%
        dplyr::mutate(totalProd_gm2yr = round(rowSums(dplyr::across("CoolSeasonGram_gm2yr":"Wheat_gm2yr"), na.rm = TRUE),
                                              digits = 2),
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
        dplyr::select(-"totalProd_gm2yr",
                      -c("Barley_gm2yr":"Wheat_gm2yr")) %>%

        #   Pivot data to long format to enable grouping by herbGroup
        dplyr::rename_with(~ stringr::str_remove(., "_gm2yr"),
                           .cols = c("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr")) %>%
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
                         .groups = "drop") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteYear,
                        .data$plotType,
                        .data$herbGroup) %>%
        dplyr::filter(herbMass == max(.data$herbMass)) %>%
        dplyr::slice_max(order_by = .data$count,
                         n = 1) %>%
        dplyr::ungroup() %>%

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
        dplyr::summarise(clipCount = n(),
                         agb_gm2 = round(mean(.data$agb_gm2, na.rm = TRUE),
                                         digits = 2),
                         .groups = "drop") %>%

        #   Use pivot_wider and rowSums to get one row per plot with columns per herbGroup, and calculate plot-level productivity
        tidyr::pivot_wider(names_from = "herbGroup",
                           names_glue = "{herbGroup}_gm2yr",
                           values_from = "agb_gm2") %>%
        #   Replace NAs for cropless plots in herbGroup columns with zero
        dplyr::mutate(dplyr::across("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr", ~tidyr::replace_na(., 0))) %>%
        dplyr::mutate(totalProd_gm2yr = round(rowSums(dplyr::across("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr"), na.rm = TRUE),
                                              digits = 2),
                      .after = "plotManagement")

    } else {

      croplessPlots <- NULL

    } # End nrow(croplessPlots) conditional



    ### Combine results from cropped and cropless plots
    cropSiteYearDF <- dplyr::bind_rows(cropPlots,
                                       croplessPlots) %>%
      #   Replace NAs for herbGroup columns with zero
      dplyr::mutate(dplyr::across("CoolSeasonGram_gm2yr":"Wheat_gm2yr", ~tidyr::replace_na(., 0))) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotType,
                     .data$plotID)

  } else {

    cropSiteYearDF <- NULL

  } # End nrow(cropSiteYearDF) conditional






  ### Plot-level output: Finalize plot-level productivity table for "standard" and "crop" sites ####
  ##  Conditionally combine plot output from "standard" and "crop" sites
  if (!is.null(stdSiteYearDF) & !is.null(cropSiteYearDF)) {

    herb_ANPP_plot <- dplyr::bind_rows(stdSiteYearDF,
                                       cropSiteYearDF) %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotType,
                     .data$plotID)

  } else if (!is.null(stdSiteYearDF) & is.null(cropSiteYearDF)) {

    herb_ANPP_plot <- stdSiteYearDF %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotType,
                     .data$plotID)

  } else if (is.null(stdSiteYearDF) & !is.null(cropSiteYearDF)) {

    herb_ANPP_plot <- cropSiteYearDF %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year,
                     .data$plotType,
                     .data$plotID)

  } else {

    herb_ANPP_plot <- NULL

  }


  ##  Finalize herb_ANPP_plot data frame
  if (!is.null(herb_ANPP_plot)) {

    herb_ANPP_plot <- herb_ANPP_plot %>%
      #   Replace NAs for cropless plots in crop columns with zero
      dplyr::mutate(dplyr::across("CoolSeasonGram_gm2yr":"Wheat_gm2yr", ~tidyr::replace_na(., 0))) %>%

      #   Standardize column names for output, calculate output in Mg/ha/yr
      dplyr::rename("herbProd_gm2yr" = "totalProd_gm2yr") %>%
      dplyr::mutate(herbProd_Mghayr = round(.data$herbProd_gm2yr * 0.01, digits = 2),
                    .before = "herbProd_gm2yr")

  }







  ### Grazed sites: Calculate site-level ANPP for 'site x year' combos with grazing in at least one plot ####
  #--> For plots with exclosure == "Y" at any point in a site-year, treat as a grazed plot; this means that when a clipID under a damaged exclosure is not clipped, the "ambient" clip is still used to estimate grazing consumption.
  #--> Treat plots with no exclosure at any point in a site-year as ungrazed, and take the eventID with the greatest mean mass across all ungrazed plots as the eventID that equals ANPP; note: grazed plots that never had an exclosure successfully deployed will be considered as ungrazed (known error).

  if (nrow(grazedSiteYearDF)) {

    ### Account for sites where Distributed plots have crops and Tower plots are grazed --> filter out cropped Distributed plots
    #--> Occurs at LAJA in 2020
    grazedSiteYearDF <- grazedSiteYearDF %>%
      dplyr::filter(!(.data$plotType == "distributed" & .data$siteID %in% cropSiteYearDF$siteID &
                       .data$year %in% cropSiteYearDF$year))



    ### Parse plots into grazed (ostensibly all Tower plots) and graze-less
    #--> Graze-less plots are a mix of Tower plots and Distributed plots where none of the Distributed plots are cropped;
    #--> If any Distributed plots are cropped (as at LAJA in 2020), Distributed plots are processed above as cropped.

    #   Remove crop columns as not relevant to grazed sites
    grazedSiteYearDF <- grazedSiteYearDF %>%
      dplyr::select(-c("Barley_gm2yr":"Wheat_gm2yr"))


    ##  Identify grazed plots
    grazedPlots <- grazedSiteYearDF %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotID,
                      .data$plotType,
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
      dplyr::select(-c("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr")) %>%
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
                      .data$plotManagement) %>%
      dplyr::summarise(finalClipCount = dplyr::n(),
                       finalStandingMass_gm2yr = round(mean(.data$totalProd_gm2yr, na.rm = TRUE),
                                                       digits = 2),
                       finalStandingSD_gm2yr = round(stats::sd(.data$totalProd_gm2yr, na.rm = TRUE),
                                                     digits = 2),
                       .groups = "drop") %>%
      dplyr::rename("finalEventID" = "eventID")



    ### Calculate consumption as difference between exclosure == "Y" and exclosure == "N" for all eventIDs
    #--> Treat all clipIDs as independent observations; that is, subplotIDs within plots are NOT averaged first.

    grazedPlots <- grazedPlots %>%

      #   Filter to Tower plots; should be redundant but do anyway
      dplyr::filter(.data$plotType == "tower") %>%

      #   Update exclosure to be "N" when targetTaxaPresent == "N" so these zeroes are properly included in the estimate of ambient biomass.
      dplyr::mutate(exclosure = dplyr::case_when(is.na(.data$exclosure) & .data$targetTaxaPresent == "N" ~ "N",
                                                 TRUE ~ exclosure)) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$eventID,
                      .data$exclosure,
                      .data$plotType,
                      .data$plotManagement) %>%

      #   Determine sample size, mean, and SD for exclosure = "N" and "Y"
      dplyr::summarise(clipCount = dplyr::n(),
                       agbMean_gm2 = round(mean(.data$totalProd_gm2yr, na.rm = TRUE),
                                           digits = 2),
                       agbSD_gm2 = round(stats::sd(.data$totalProd_gm2yr, na.rm = TRUE),
                                         digits = 2),
                       .groups = "drop") %>%

      #   Pivot wider to get exclosure = "N" and "Y" on same row to enable within row consumption estimate for each eventID
      tidyr::pivot_wider(names_from = "exclosure",
                         values_from = c("clipCount" , "agbMean_gm2", "agbSD_gm2"),
                         names_prefix = "excl") %>%

      #   Calculate consumption mean and SD per eventID
      #--> Uncertainties combined according to: https://www.mathbench.umd.edu/modules/statistical-tests_t-tests/page06.htm
      dplyr::mutate(consumClipCount = .data$clipCount_exclN + .data$clipCount_exclY,
                    consumMean_gm2yr = round(.data$agbMean_gm2_exclY - .data$agbMean_gm2_exclN,
                                           digits = 2),
                    consumSD_gm2yr = round(sqrt((.data$agbSD_gm2_exclN^2 / .data$clipCount_exclN) +
                                                (.data$agbSD_gm2_exclY^2 / .data$clipCount_exclY)),
                                         digits = 2),
                    consumSD2_N = round(.data$consumSD_gm2yr^2 / .data$consumClipCount,
                                        digits = 2))


    ##  Set aside detailed consumption data for output
    consumptionDF <- grazedPlots %>%
      dplyr::select(-"consumSD2_N") %>%
      dplyr::mutate(plotManagement = paste(.data$plotManagement, "grazed", sep = ", ")) %>%
      dplyr::relocate("consumClipCount":"consumSD_gm2yr",
                      .after = "plotManagement")


    ##  Summarize consumption data at the site level
    grazedPlots <- grazedPlots %>%

      #   Remove NAs from 'consumMean_gm2' (happens when no exclosure = "Y" records for a bout), then sum consumption across all bouts in a site-year and propagate uncertainty
      dplyr::filter(!is.na(.data$consumMean_gm2yr)) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year,
                      .data$plotType,
                      .data$plotManagement) %>%
      dplyr::summarise(consumEventCount = dplyr::n(),
                       consumClipCount = sum(.data$consumClipCount),
                       consumption_gm2yr = round(sum(.data$consumMean_gm2yr, na.rm = TRUE),
                                               digits = 2),
                       consumptionSD_gm2yr = round(sqrt(sum(.data$consumSD2_N, na.rm = TRUE)),
                                                 digits = 2),
                       .groups = "drop") %>%

      #   Join with 'grazedFinalMass' and add to consumption to get ANPP for grazed plots
      dplyr::left_join(grazedFinalMass %>%
                         dplyr::select(-"plotType",
                                       -"plotManagement"),
                       by = c("domainID", "siteID", "year")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(grazedClipCount = sum(.data$consumClipCount, .data$finalClipCount, na.rm = TRUE),
                    grazedProd_gm2yr = sum(.data$consumption_gm2yr, .data$finalStandingMass_gm2yr, na.rm = TRUE),
                    grazedProdSD_gm2yr = dplyr::case_when(
                      is.na(.data$finalStandingSD_gm2yr) & !is.na(.data$consumptionSD_gm2yr) ~
                        round(.data$consumptionSD_gm2yr, digits = 2),
                      !is.na(.data$finalStandingSD_gm2yr) & is.na(.data$consumptionSD_gm2yr) ~
                        round(.data$finalStandingSD_gm2yr, digits = 2),
                      TRUE ~ round(sqrt((.data$finalStandingSD_gm2yr^2 / .data$finalClipCount) +
                                          (.data$consumptionSD_gm2yr^2 / .data$consumClipCount)),
                                   digits = 2)
                    ),
                    .after = "plotManagement") %>%
      dplyr::ungroup()



    ### Process graze-less plots for ANPP

    if (nrow(grazelessPlots)) {

      ##  Isolate and process Tower plots; assume ungrazed Tower plots may be clipped > 1X/year even though they should not be.
      #--> Ignore Distributed plots for ANPP at grazed sites. Cannot determine whether these plots were grazed, and if they were grazed they do not have exclosures so cannot contribute meaningfully to ANPP estimates.
      #--> Choose eventID with greatest mass to sum for ANPP.

      #   Use only 'totalProd_gm2yr' and ignore herbGroup because some of these plots may have no herbGroup data if they were clipped on the same schedule as the grazed Tower plots and were not sorted to herbGroup AND they never had an exclosure during the site-year.
      grazelessPlots <- grazelessPlots %>%
        dplyr::filter(.data$plotType == "tower") %>%
        dplyr::select(-c("CoolSeasonGram_gm2yr":"WoodyPlants_gm2yr"))

      #   Identify the 'site x year x herbGroup x eventID' combo with greatest productivity; the mean is across all clipIDs to ensure the same sampling scale is combined with the productivity estimate from grazed plots.
      towerMax <- grazelessPlots %>%
        dplyr::group_by(.data$domainID,
                        .data$siteYear,
                        .data$eventID) %>%
        dplyr::summarise(count = n(),
                         herbMass = round(mean(.data$totalProd_gm2yr, na.rm = TRUE),
                                          digits = 2),
                         .groups = "drop") %>%
        dplyr::group_by(.data$domainID,
                        .data$siteYear) %>%
        dplyr::filter(herbMass == max(.data$herbMass)) %>%
        dplyr::slice_max(order_by = .data$count,
                         n = 1) %>%
        dplyr::mutate(maxEvent = paste(.data$siteYear, .data$eventID,
                                       sep = "-"),
                      .before = "domainID")

      #   Retain records from max 'site x year x eventID' combos, then calculate plot-level ANPP.
      #--> Productivity will be underestimated in these plots if they were grazed but an exclosure was never successfully deployed in the plot during the site-year; this reflects a current limitation of the data collected.
      grazelessPlots <- grazelessPlots %>%
        dplyr::mutate(maxEvent = paste(.data$siteYear, .data$eventID,
                                       sep = "-"),
                      .before = "domainID") %>%
        dplyr::filter(.data$maxEvent %in% towerMax$maxEvent) %>%
        dplyr::select(-"maxEvent") %>%

        #   Calculate mean for 'site x year' combo keeping sample count and ANPP at clipID scale similar to grazed plots
        dplyr::group_by(.data$domainID,
                        .data$siteID,
                        .data$year,
                        .data$plotType,
                        .data$plotManagement) %>%
        dplyr::summarise(ungrazedClipCount = n(),
                         ungrazedProd_gm2yr = round(mean(.data$totalProd_gm2yr, na.rm = TRUE),
                                                    digits = 2),
                         ungrazedProdSD_gm2yr = round(stats::sd(.data$totalProd_gm2yr, na.rm = TRUE),
                                                    digits = 2),
                         .groups = "drop")


      ##  Join with grazed plot output and combine productivity and uncertainty at site-year scale
      #--> The finalClipCount is used to weight 'totalProd_gm2yr' because this value represents the number of independent subplotIDs sampled across grazed + ungrazed plots
      grazedSiteYearDF <- dplyr::full_join(grazedPlots,
                                           grazelessPlots,
                                           by = c("domainID", "siteID", "year", "plotType", "plotManagement")) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(totalClipCount = sum(.data$grazedClipCount, .data$ungrazedClipCount, na.rm = TRUE),

                      herbProd_gm2yr = dplyr::case_when(
                        is.na(.data$ungrazedClipCount) ~ .data$grazedProd_gm2yr,
                        TRUE ~ round((.data$finalClipCount / (.data$finalClipCount + .data$ungrazedClipCount)) *
                                       .data$grazedProd_gm2yr +
                                       (.data$ungrazedClipCount / (.data$finalClipCount + .data$ungrazedClipCount))
                                     * .data$ungrazedProd_gm2yr,
                                     digits = 2)),

                      herbProdSD_gm2yr = dplyr::case_when(
                        is.na(.data$ungrazedProdSD_gm2yr) ~ .data$grazedProdSD_gm2yr,
                        TRUE ~ round(sqrt((.data$grazedProdSD_gm2yr^2 / .data$grazedClipCount) +
                                            (.data$ungrazedProdSD_gm2yr^2 / .data$ungrazedClipCount)),
                                     digits = 2)
                      ),

                      .after = "plotManagement") %>%
          dplyr::ungroup()

    } else {

      #   Standardize column names
      grazedSiteYearDF <- grazedPlots %>%
        dplyr::rename("totalClipCount" = "grazedClipCount",
                      "herbProd_gm2yr" = "grazedProd_gm2yr",
                      "herbProdSD_gm2yr" = "grazedProdSD_gm2yr")

    } # End nrow(grazelessPlots) conditional

    #   Add 'herbProd_Mghayr' column and update 'plotManagement' to indicate grazing
    grazedSiteYearDF <- grazedSiteYearDF %>%
      dplyr::mutate(plotManagement = paste(.data$plotManagement, "grazed", sep = ", "),
                    herbProd_Mghayr = round(.data$herbProd_gm2yr * 0.01, digits = 2),
                    herbProdSD_Mghayr = round(.data$herbProdSD_gm2yr * 0.01, digits = 2),
                    .after = "totalClipCount")

  } else {

    grazedSiteYearDF <- NULL

  } # End nrow(grazedSiteYearDF) conditional





  ### Site-level outputs: Create site-level summary ####
  #--> Create "herb_ANPP_grazed_extra" output with consumption, final standing mass, ungrazed plot detail; the "herb_ANPP_site" table just gets a stripped down version of grazing output.

  #   Create site-level outputs for "standard" and "crop" sites
  if (!is.null(herb_ANPP_plot)) {

    herb_ANPP_site <- herb_ANPP_plot %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year) %>%
      dplyr::summarise(plotType = paste(sort(unique(.data$plotType)), collapse = ", "),
                       plotManagement = paste(sort(unique(.data$plotManagement)), collapse = ", "),
                       plotCount = n(),
                       clipCount = sum(.data$clipCount),
                       siteProd_gm2yr = round(mean(.data$herbProd_gm2yr, na.rm = TRUE),
                                              digits = 2),
                       herbProdSD_gm2yr = round(stats::sd(.data$herbProd_gm2yr, na.rm = TRUE),
                                                digits = 2)) %>%
      dplyr::rename("herbProd_gm2yr" = "siteProd_gm2yr") %>%
      dplyr::mutate(herbProd_Mghayr = round(.data$herbProd_gm2yr * 0.01,
                                            digits = 2),
                    herbProdSD_Mghayr = round(.data$herbProdSD_gm2yr * 0.01,
                                              digits = 2),
                    .after = "clipCount") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year)

  } else {

    herb_ANPP_site <- NULL

  }



  ### Conditionally extract summary site-level output from grazed sites
  #--> Extra steps required to combine data from sites with grazed Tower plots and cropped Distributed plots (e.g., LAJA 2020)

  if (!is.null(grazedSiteYearDF)) {

    ##  Generate data frame with plotTypes and plotManagement by site-year
    temp <- dplyr::bind_rows(herb_ANPP_site %>%
                               dplyr::select("domainID":"plotManagement"),
                             grazedSiteYearDF %>%
                               dplyr::select("domainID":"plotManagement")) %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$year) %>%
      dplyr::summarise(plotType = paste(sort(unique(.data$plotType)), collapse = ", "),
                       plotManagement = {
                         vals <- plotManagement %>%
                           stringr::str_split(",") %>%            # split each string into vector
                           purrr::flatten_chr() %>%               # flatten to one vector
                           stringr::str_trim() %>%                # trim whitespace
                           unique() %>%                           # remove duplicates
                           sort()                                 # sort values
                         paste(vals, collapse = ", ")
                       },
                       .groups = "drop")


    ##  Join herb_ANPP_site with grazedSiteYearDF and combine productivity estimates and uncertainties for each site-year
    herb_ANPP_site <- dplyr::full_join(herb_ANPP_site %>%
                                         dplyr::select(-"plotType",
                                                       -"plotManagement"),
                                       grazedSiteYearDF %>%
                                         dplyr::select("domainID":"year",
                                                       "totalClipCount":"herbProdSD_gm2yr",
                                                       "finalClipCount",
                                                       "ungrazedClipCount") %>%
                                         dplyr::rename_with(~ sub("^herb", "grazed", .x), starts_with("herb")) %>%
                                         dplyr::rename("totGrazeClipCount" = "totalClipCount"),
                                       by = c("domainID", "siteID", "year")) %>%
      dplyr::relocate("totGrazeClipCount",
                      .after = "clipCount") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        #   Calculate total clip count
        totClipCount = sum(.data$clipCount, .data$totGrazeClipCount, na.rm = TRUE),

        #   Determine site-level productivity
        totProd_gm2yr = dplyr::case_when(
          is.na(.data$totGrazeClipCount) & !is.na(.data$clipCount) ~ .data$herbProd_gm2yr,
          !is.na(.data$totGrazeClipCount) & is.na(.data$clipCount) ~ .data$grazedProd_gm2yr,
          TRUE ~ (.data$clipCount / sum(.data$clipCount, .data$finalClipCount, .data$ungrazedClipCount, na.rm = TRUE)) *
            .data$herbProd_gm2yr + (sum(.data$finalClipCount, .data$ungrazedClipCount, na.rm = TRUE) /
                                      sum(.data$clipCount, .data$finalClipCount, .data$ungrazedClipCount, na.rm = TRUE)) *
            .data$grazedProd_gm2yr),

        #   Determine combined SD
        totProdSD_gm2yr = dplyr::case_when(
          is.na(.data$totGrazeClipCount) & !is.na(.data$clipCount) ~ .data$herbProdSD_gm2yr,
          !is.na(.data$totGrazeClipCount) & is.na(.data$clipCount) ~ .data$grazedProdSD_gm2yr,
          TRUE ~ sqrt(sum((.data$herbProdSD_gm2yr^2 / .data$clipCount), (.data$grazedProdSD_gm2yr / .data$totGrazeClipCount),
                          na.rm = TRUE)))

      ) %>%

      #   Add Mg/ha/yr output
      dplyr::mutate(totProd_Mghayr = round(.data$totProd_gm2yr * 0.01, digits = 2),
                    totProdSD_Mghayr = round(.data$totProdSD_gm2yr * 0.01, digits = 2),
                    .before = "totProd_gm2yr") %>%

      #   Round output
      dplyr::mutate(totProd_gm2yr = round(.data$totProd_gm2yr, digits = 2),
                    totProdSD_gm2yr = round(.data$totProdSD_gm2yr, digits = 2)) %>%

      dplyr::ungroup() %>%

      #   Finalize columns for output
      dplyr::select(-c("clipCount":"ungrazedClipCount")) %>%
      dplyr::rename("herbProd_Mghayr" = "totProd_Mghayr",
                    "herbProdSD_Mghayr" = "totProdSD_Mghayr",
                    "herbProd_gm2yr" = "totProd_gm2yr",
                    "herbProdSD_gm2yr" = "totProdSD_gm2yr") %>%

      #   Bring back plotType and plotManagement columns and arrange rows
      dplyr::full_join(temp,
                       by = c("domainID", "siteID", "year")) %>%
      dplyr::relocate("plotType":"plotManagement",
                      .after = "year") %>%
      dplyr::arrange(.data$domainID,
                     .data$siteID,
                     .data$year)

  } # End !is.null(grazedSiteYearDF) conditional



  ### Return output data ####
  output <- list(herb_ANPP_plot = herb_ANPP_plot,
                 herb_ANPP_site = herb_ANPP_site,
                 herb_ANPP_grazed_extra = grazedSiteYearDF,
                 herb_grazed_consumption = consumptionDF)

  return(output)
}
