#' @title Estimate above-ground biomass of woody vegetation
#'
#' @author
#' Samuel M Simkin \email{ssimkin@BattelleEcology.org} \cr
#' Courtney Meier \email{cmeier@BattelleEcology.org} \cr
#'
#' @description Allometric equations are used to estimate above-ground biomass for woody individuals reported in the NEON "Vegetation structure" data product (DP1.10098.001). Results are summarized as mass per unit area at scales of the siteID, plotID, and taxonID. Biomass outputs can be used in the neonPlants estimateMass() and estimateWoodProd() functions.
#'
#' Data inputs are "Vegetation structure" data (DP1.10098.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#'
#' @details Input data can be filtered by 'plotSubset' if output for only certain types of plots or sampling intervals is desired. Input data are combined with allometric equation parameters and taxon specific characteristics, and biomass is estimated for each individual using allometric equations. Generalized allometric equations are applied first and are replaced by taxon-specific equations if available. Biomass may be estimated only for "tree" individuals (i.e., woody individuals with DBH ≥ 10 cm) or for "all" growth forms excluding "cactus", "ferns", and "yucca". Biomass is summarized on an areal basis at the hierarchical level of the plot and site.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. It is optional to include the "vst_non-woody" table in the list. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for the 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments.  [list]
#'
#' @param inputIndividual The 'vst_apparentindividual' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputMapTag The 'vst_mappingandtagging' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputPerPlot The 'vst_perplotperyear' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param inputNonWoody (Optional) The 'vst_non-woody' table for the site x month combination(s) of interest
#' (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#'
#' @param plotSubset The options are "all" (all Tower and Distributed plots), the default of "towerAll" (all plots in the Tower airshed but no Distributed plots), "towerAnnualSubset" (the subset of n=5 Tower plots that are sampled annually), and "distributed" (all Distributed plots, which are sampled at 5-yr intervals and are spatially representative of the NLCD classes at a site). [character]
#'
#' @param growthForm Select Vegetation Structure growth forms for biomass estimation. The options are "tree" (sbt, mbt), which enables biomass estimation only for trees with a DBH ≥ 10 cm, and the default of "all", which includes "tree" individuals, and also small trees (smt, sap), shrubs (sis, sms), lianas (lia), palms (ptr, spm), tree ferns (ltf, stf), ocotillo (oco), and xerophyllum (xer). Consult the Vegetation Structure Quick Start Guide and/or the Data Product User Guide for more growth form information. [character]
#'
#' @return A list that includes biomass summary data frames and a helper data frame needed by companion productivity functions - e.g., the estimateWoodProd() function. Output tables include:
#'   * vst_agb_kg - Summarizes above-ground live and dead woody biomass for each individual ("kg").
#'   * vst_plot_w_0s - Summarizes above-ground live and dead woody biomass for each taxonID x growthForm x eventID combination for each plot ("Mg/ha").
#'   * vst_agb_zeros - Helper output for productivity estimation that contains plot x year combinations with biomass of zero.
#'   * vst_site - Summarizes above-ground live and dead woody biomass for each site x year combination in the data. ("Mg/ha").
#'
#' @examples
#' \dontrun{
#' #  Obtain NEON Vegetation structure data
#' vstDF <- neonUtilities::loadByProduct(
#' dpID = "DP1.10098.001",
#' package = "basic",
#' check.size = FALSE
#' )
#'
#' # example with arguments at default values
#' estimateWoodMassOutputs <- estimateWoodMass(inputDataList = vstDF)
#'
#' # example specifying several non-default arguments
#' estimateWoodMassOutputs <- estimateWoodMass(
#' inputDataList = vstDF,
#' plotSubset = "towerAnnualSubset",
#' growthForm = "tree"
#' )
#'
#' }
#'
#' @export estimateWoodMass


estimateWoodMass = function(inputDataList,
                            inputIndividual = NA,
                            inputMapTag = NA,
                            inputNonWoody = NA,
                            inputPerPlot = NA,
                            plotSubset = "towerAll",
                            growthForm = "all") {

  options(dplyr.summarise.inform = FALSE)


  ### Verify user inputs are correct type and contain expected tables ####

  ### Verify user-supplied 'inputDataList' object contains correct data if not missing
  if (!missing(inputDataList)) {

    #   Check that input is a list
    if (!inherits(inputDataList, "list")) {
      stop(glue::glue("Argument 'inputDataList' must be a list object from neonUtilities::loadByProduct();
                     supplied input object is {class(inputDataList)}"))
    }

    #   Check that required tables within list match expected names
    listExpNames <- c("vst_apparentindividual", "vst_mappingandtagging", "vst_perplotperyear")


    #   All expected tables required
    if (length(setdiff(listExpNames, names(inputDataList))) > 0) {
      stop(glue::glue("Required tables missing from 'inputDataList':",
                      '{paste(setdiff(listExpNames, names(inputDataList)), collapse = ", ")}',
                      .sep = " "))
    }

  } else {

    inputDataList <- NULL

  } # end missing conditional



  ### Verify table inputs are NA if 'inputDataList' is supplied
  if (inherits(inputDataList, "list") &
      (!is.logical(inputIndividual) | !is.logical(inputMapTag) | !is.logical(inputPerPlot)  | !is.logical(inputNonWoody) )) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }



  ### Verify 'inputIndividual', 'inputMapTag', and 'inputPerPlot' are data frames if 'inputDataList' is missing
  if (is.null(inputDataList) &
      (!inherits(inputIndividual, "data.frame") | !inherits(inputMapTag, "data.frame")  |
       !inherits(inputPerPlot, "data.frame") | !inherits(inputNonWoody, "data.frame"))) {

    stop("Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
  }


  #   Assign standardized names to input data frames
  if (inherits(inputDataList, "list")) {

    map <- inputDataList$vst_mappingandtagging
    perPlot <- inputDataList$vst_perplotperyear
    appInd <- inputDataList$vst_apparentindividual

    #   Account for optional input of vst_non-woody
    if ("vst_non-woody" %in% names(inputDataList)) {
      nonWoody <- inputDataList$`vst_non-woody`
    } else {
      nonWoody <- NA
    }


  } else {

    map <- inputMapTag
    perPlot <- inputPerPlot
    appInd <- inputIndividual
    nonWoody <- inputNonWoody

  }



  ### Verify input tables contain required columns and data ####

  ### Verify 'vst_mappingandtagging' table contains required data
  #   Check for required columns
  mapExpCols <- c("siteID", "plotID", "individualID", "taxonID")

  if (length(setdiff(mapExpCols, colnames(map))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_mappingandtagging':", '{paste(setdiff(mapExpCols, colnames(map)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(map) == 0) {
    stop(glue::glue("Table 'vst_mappingandtagging' has no data."))
  }


  ### Verify 'vst_perplotperyear' table contains required data
  #   Check for required columns
  plotExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana", "totalSampledAreaFerns", "totalSampledAreaOther")

  if (length(setdiff(plotExpCols, colnames(perPlot))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_perplotperyear':", '{paste(setdiff(plotExpCols, colnames(perPlot)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(perPlot) == 0) {
    stop(glue::glue("Table 'vst_perplotperyear' has no data."))
  }


  ### Verify 'vst_apparentindividual' table contains required data
  #   Check for required columns
  appIndExpCols <- c("plotID", "individualID", "growthForm", "plantStatus", "date", "eventID", "stemDiameter", "basalStemDiameter")

  if (length(setdiff(appIndExpCols, colnames(appInd))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_apparentindividual':", '{paste(setdiff(appIndExpCols, colnames(appInd)), collapse = ", ")}',
                    .sep = " "))
  }

  #   Check for data
  if (nrow(appInd) == 0) {
    stop(glue::glue("Table 'vst_apparentindividual' has no data."))
  }


  ### Verify vst_nonWoody table contains required data
  #   Check for required columns
  nonwoodyExpCols <- c("domainID", "siteID", "plotID", "individualID", "growthForm", "plantStatus", "date", "stemDiameter", "basalStemDiameter", "taxonID", "height", "leafNumber", "meanLeafLength", "meanPetioleLength", "meanBladeLength")

  #  if (class(vst_nonWoody) == "data.frame"){if(length(setdiff(nonwoodyExpCols, colnames(vst_nonWoody))) > 0) {
  if (methods::is(nonWoody, class = "data.frame" )) {

    if (length(setdiff(nonwoodyExpCols, colnames(nonWoody))) > 0) {
      stop(glue::glue("Required columns missing from vst_nonWoody:", '{paste(setdiff(nonwoodyExpCols, colnames(nonWoody), collapse = ", ")}',
                      .sep = " "))
    }
  }

  # Error if invalid growthForm option selected
  if (!growthForm %in% c("all", "tree")) {
    stop("The growthForm argument must be one of: 'all', 'tree'")
  }

  # Error if invalid plotSubset option selected
  if (!plotSubset %in% c("all", "towerAll", "towerAnnualSubset", "distributed")) {
    stop("The plotSubset argument must be one of: 'all', 'towerAll', 'towerAnnualSubset', 'distributed'")
  }

  #   For plotPriority: 50 is highest possible value
  plotPriority <- ifelse(plotSubset == "towerAnnualSubset", 5, 50)

  #   Assign plotType needed in output based on 'plotSubset' argument
  plotType <- unique(dplyr::case_when(plotSubset == "all" ~ c("distributed", "tower"),
                                      plotSubset == "distributed" ~ "distributed",
                                      plotSubset %in% c("towerAll", "towerAnnualSubset") ~ "tower"))


  ##  Read in plot sample area for each growthForm by eventID combo from 'perPlot' table
  #   Extract year from eventID
  perPlot$year <- as.numeric(substr(perPlot$eventID, 10, 13))

  #   Load companion 'priority_plots' dataset to obtain 'specificModuleSamplingPriority' data. Field is used to optionally filter only to plots with priority 1-5 when user-supplied 'plotSubset' == "towerAnnualSubset"
  priority_plots <- priority_plots %>%
    dplyr::select("plotID",
                  "specificModuleSamplingPriority")

  #   Merge to add plot priority data to 'perPlot' data
  perPlot <- merge(perPlot,
                   priority_plots,
                   by = c("plotID"),
                   all.x = TRUE)

  #   Create unique plotID x eventID identifier
  perPlot$plot_eventID <- paste0(perPlot$plotID, "_", perPlot$eventID)

  #   Sort by date before removing duplicates so that if duplicates are from different dates the record from latest date will be retained. Sorting by date and then using fromLast = TRUE should retain the most recent version of duplicates.
  perPlot <- perPlot[order(perPlot$date), ]
  perPlot <- perPlot[!duplicated(perPlot$plot_eventID, fromLast = TRUE), ]

  #   Discard Sampling Impractical other than "OK"
  perPlot_not_SI <- perPlot %>%
    dplyr::filter(.data$samplingImpractical == "OK" | .data$samplingImpractical == "" | is.na(.data$samplingImpractical))

  #   Create list of ALL plot by eventID combos from the vst_perplotperyear tables from vst woody, vst non-herb perennial, or both, regardless of whether they have biomass. Generates a list of all unique combos of plotID and eventID where full sampling should have taken place
  plot_eventID_list <- unique(perPlot_not_SI$plot_eventID)

  #   Retain subset of columns in "perPlot" data; 'dataCollected' needed to identify plots for which biomass cannot be accurately estimated on an areal basis (because not all trees were sampled).
  perPlot <- perPlot %>%
    dplyr::select("plot_eventID",
                  "plotID",
                  "eventID",
                  "year",
                  "nlcdClass",
                  "plotType",
                  "eventType",
                  "dataCollected",
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana",
                  "totalSampledAreaOther")

  #   Identify unique 'plotID' and 'plotType' values in the dataset; table 'plotType_df' used later to add 'plotType' to output data frames
  plotType_df <- inputDataList$vst_perplotperyear %>%
    dplyr::distinct(.data$plotID,
                    .data$plotType)


  ##  Define plantStatus groups to identify standing individuals that are live/dead and individuals absent, missing, or with ambiguous fate
  standingLiveDead <- c("Live",
                        "Live, insect damaged",
                        "Live, disease damaged",
                        "Live, physically damaged",
                        "Live, other damage",
                        "Live, broken bole",
                        "Standing dead",
                        "Dead, broken bole")

  missingDowned <- c("Removed",
                     "No longer qualifies",
                     "Lost, burned",
                     "Lost, herbivory",
                     "Lost, presumed dead",
                     "Lost, fate unknown",
                     "Downed")

  #   Create 'missingDownedDF" table for later output
  missingDownedDF <- data.frame()



  ### Estimate non-woody biomass: Calculate biomass from vst_non-woody table ####

  ### Conditionally generate non-woody biomass estimates from vst_nonWoody table
  if (methods::is(nonWoody, class = "data.frame" )) {

    #   Merge with perplot data to add total sampled areas
    vst_agb_other <- merge(nonWoody,
                           perPlot,
                           by = c("plotID", "eventID"),
                           all.x = TRUE)

    vst_agb_other$agb_source <- "no source"

    #   Create 'agb' = NA column to update as biomass is calculated by growthForm
    vst_agb_other$agb <- NA


    ##  Estimate yucca biomass: White, J.D., K.J. Gutzwiller, W.C. Barrow, L.J. Randall, and P. Swint. 2008. Modeling mechanisms of vegetation change due to fire in a semi-arid ecosystem. Ecological Modelling 214:181-200; divide by 1000 to convert output from "grams" to "kg"
    # vst_agb_other$agb_yucca <- ifelse(vst_agb_other$growthForm == "yucca",
    #                                   round(((0.0022*vst_agb_other$height) + (0.00096*(vst_agb_other$height^2)) + 0.04)/1000,
    #                                         digits = 3),
    #                                   NA)
    #
    # #   Provide yucca biomass allometry reference
    # vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_yucca),
    #                                    "White_et_al_2008_yucca",
    #                                    vst_agb_other$agb_source)
    #
    # vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "White_et_al_2008_yucca",
    #                             vst_agb_other$agb_yucca,
    #                             vst_agb_other$agb)


    ##  Estimate ocotillo biomass: Bobich, E.G., and T.E. Huxman. 2009. Dry mass partitioning and gas exhange for young ocotillos (Fouquieria splendends) in the Sonoran Desert. International Journal of Plant Science 170:283-289. Equations:
    #   log(height_m) = 0.13 + 0.45 * log(total above and below ground biomass in kg)
    #   log(total above and below ground biomass in kg) = (log(height_m) - 0.13)/0.45 = -0.2889 +  (2.2222 * log(height_m))
    #   log(root/shoot) = -0.63 + 0.18 * log(total above and below ground biomass in kg)
    #   aboveground biomass in kg = 1(1+exp(log(root/shoot))) * exp(log(total above and below ground biomass in kg)) = fraction aboveground * total biomass

    #   Estimate total ocotillo mass: aboveground + belowground
    vst_agb_other$tot_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
                                         exp(-0.2889 + 2.2222 * log(vst_agb_other$height)),
                                         NA)

    #   Estimate aboveground ocotillo mass
    vst_agb_other$agb_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
                                         round(1/(exp(-0.63 + 0.18 * log(vst_agb_other$tot_ocotillo)) + 1) *
                                                 vst_agb_other$tot_ocotillo,
                                               digits = 3),
                                         NA)

    #   Remove total ocotillo mass: Belowground estimate not needed
    vst_agb_other$tot_ocotillo <- NULL

    #   Update "agb" column with ocotillo mass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(agb_source = dplyr::case_when(!is.na(.data$agb_ocotillo) ~ "Bobich_and_Huxman_2009",
                                                  TRUE ~ .data$agb_source),
                    agb = dplyr::case_when(!is.na(.data$agb_ocotillo) ~ .data$agb_ocotillo,
                                           TRUE ~ .data$agb))


    ##  Estimate Xerophyllum tenax (bear grass) biomass: Gholz, H.L., C.C. Grier, A.G. Campbell, and A.T. Brown. 1979. Equations for estimating biomass and leaf area of plants in the pacific northwest. Research paper 41. Forest Research Laboratory, School of Forestry at Oregon State University, Corvallis. Divide by 1000 to convert output to "kg".
    vst_agb_other$agb_xer <- ifelse(vst_agb_other$growthForm == "xerophyllum",
                                    round((18.873 + (0.0280*((vst_agb_other$basalStemDiameter^2) *
                                                               vst_agb_other$meanLeafLength)))/1000,
                                          digits = 3),
                                    NA)

    #   Update "agb" column with xerophyllum mass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(agb_source = dplyr::case_when(!is.na(.data$agb_xer) ~ "Gholz_et_al_1979",
                                                  TRUE ~ .data$agb_source),
                    agb = dplyr::case_when(!is.na(.data$agb_xer) ~ .data$agb_xer,
                                           TRUE ~ .data$agb))


    ##  Estimate small palm biomass (primarily Serenoa repens): Gholz, H.L., D.N. Guerin, and W.P. Cropper. 1999. Phenology and productivity of saw palmetto (Serenoa repens) in a north Florida slash pine plantation. Canadian Journal of Forest Research 29:1248-1253.
    #   Use separate equations for rachis/petiole biomass (g) and blade/leaf biomass (g). Add together and multiply by leafNumber, then divide by 1000 to get total biomass (kg).
    #   Alexis et al. 2007 Biogeochemistry add petiole length and blade length together to get rachis biomass.

    vst_agb_other$agb_palm <- ifelse(vst_agb_other$growthForm == "small palm" & !is.na(vst_agb_other$meanPetioleLength) &
                                       !is.na(vst_agb_other$meanBladeLength) & !is.na(vst_agb_other$leafNumber),
                                     round((exp(-10.38 + 2.72 * log(vst_agb_other$meanPetioleLength +
                                                                      vst_agb_other$meanBladeLength)) +
                                              (-13.31 + 0.85 * vst_agb_other$meanBladeLength)) *
                                             vst_agb_other$leafNumber / 1000,
                                           digits = 3),
                                     NA)

    #   Update "agb" column with small palm biomass and provide allometry reference
    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(agb_source = dplyr::case_when(!is.na(.data$agb_palm) ~ "Gholz_et_al_1999",
                                                  TRUE ~ .data$agb_source),
                    agb = dplyr::case_when(!is.na(.data$agb_palm) ~ .data$agb_palm,
                                           TRUE ~ .data$agb))


    ##  Estimate Cibotium biomass (tree fern): Ostertag, R, F Inman-Narahari, S Cordell, CP Giardina, and L Sack. 2014. Forest Structure in low-diversity tropical forests: A study of Hawaiian wet and dry forests. PLOS One. 9:e103268; Cibotium wood density (spg_gcm3) is taken as 0.22, the value for Cibotium glaucum.

    # vst_agb_other$agb_Cibotium <- ifelse(substring(vst_agb_other$scientificName,1,8) == "Cibotium",
    #                                      round(0.2085 * (pi * (vst_agb_other$stemDiameter/2)^2 * vst_agb_other$height * 100 * 0.22/1000),
    #                                            digits = 3),
    #                                      NA)
    #
    # # #   Provide tree fern allometry reference
    # vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_Cibotium),
    #                                    "Ostertag_et_al_2014",
    #                                    vst_agb_other$agb_source)
    #
    # vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Ostertag_et_al_2014",
    #                             vst_agb_other$agb_Cibotium,
    #                             vst_agb_other$agb)

    vst_agb_other <- vst_agb_other %>%
      dplyr::mutate(agb_Cibotium = dplyr::case_when(grepl("Cibotium", .data$scientificName) &
                                                      .data$growthForm == "large tree fern" ~
                                                      round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 *
                                                              0.22/1000,
                                                            digits = 2),
                                                    TRUE ~ NA),
                    agb_source = dplyr::case_when(!is.na(.data$agb_Cibotium) ~ "Asner_et_al_2011",
                                                  TRUE ~ .data$agb_source),
                    agb = dplyr::case_when(!is.na(.data$agb_Cibotium) ~ .data$agb_Cibotium,
                                           TRUE ~ .data$agb))



    ### Clean-up of nonWoody data: Add required fields and remove unneeded fields

    #   Identify unique plot x eventIDs in vst_nonWoody data
    vst_agb_other$plot_eventID <- paste0(vst_agb_other$plotID, "_", vst_agb_other$eventID)
    agb_other_plot_eventID <- unique(vst_agb_other$plot_eventID)

    #   Identify unique years in vst_nonWoody data
    vst_agb_other$year <- as.numeric(substr(vst_agb_other$eventID, 10, 13))


    ##  Collate missing individuals (removed, lost, downed) and those with agb = "NA" for separate output table
    missingDownedDF <- vst_agb_other %>%
      dplyr::filter(.data$plantStatus %in% missingDowned | is.na(.data$agb)) %>%
      dplyr::select("plot_eventID",
                    "eventID",
                    "plotID",
                    "taxonID",
                    "scientificName",
                    "individualID",
                    "plantStatus",
                    "growthForm",
                    "year",
                    "agb") %>%
      dplyr::bind_rows(missingDownedDF)


    ##  Retain standing individuals in the plot with agb != NA that are unambiguously 'alive' or 'dead' according to plantStatus and create simplified 'plantStatus2' variable. Removing NA records avoids misinterpreting as "0" mass in later steps. Cactus and ferns removed because no allometries are applied to these individuals.
    vst_agb_other <- vst_agb_other %>%
      dplyr::filter(.data$plantStatus %in% standingLiveDead,
                    !is.na(.data$agb),
                    !.data$growthForm %in% c("cactus", "fern")) %>%
      dplyr::mutate(plantStatus2 = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "live",
                                                    TRUE ~ "dead"))


    ##  Remove unneeded columns
    vst_agb_other <- vst_agb_other %>%
      dplyr::select(-"uid",
                    -"namedLocation",
                    -"publicationDate",
                    -"stemCount",
                    -"branchCount",
                    -"meanBranchLength",
                    -"identificationReferences",
                    -"identificationQualifier",
                    -"morphospeciesID",
                    -"measuredBy",
                    -"recordedBy",
                    -"subplotID")


    ##  Aggregate vst non-herbaceous perennial (other) biomass data (multiple records associated with multi-stem individuals)
    vst_agb_final_other <- vst_agb_other %>%
      dplyr::group_by(.data$plot_eventID,
                      .data$eventID,
                      .data$siteID,
                      .data$plotID,
                      .data$taxonID,
                      .data$scientificName,
                      .data$individualID,
                      .data$plantStatus2,
                      .data$growthForm,
                      .data$year) %>%
      dplyr::summarise(agb = sum(.data$agb, na.rm = TRUE),
                       .groups = "drop")


    ##  Join to associate records with total sampled areas
    vst_agb_final_other <- merge(vst_agb_final_other,
                                 perPlot,
                                 by = c("plot_eventID", "eventID", "year", "plotID"),
                                 all.x = TRUE)


    ##  Assign total sampled area for each individual based on growthForm
    #--> palm tree and large tree fern individuals sampled throughout plot like trees.
    vst_agb_final_other <- vst_agb_final_other %>%
      dplyr::mutate(sampledArea_m2 = dplyr::case_when(.data$growthForm %in% c("palm tree", "large tree fern") ~
                                                        .data$totalSampledAreaTrees,
                                                      TRUE ~ .data$totalSampledAreaOther))

  } #   end non-woody conditional



  ### Estimate woody biomass: Calculate biomass for individuals in vst_apparentindividual table ####

  ##  Read in taxonID from vst_mappingandtagging table
  #   Retain most recent record from vst_mappingandtagging
  map <- map[order(map$date),]
  map <- map[!duplicated(map$individualID, fromLast = TRUE), ]

  #   Find unique taxonIDs
  taxonID_df <- map %>%
    dplyr::distinct(.data$taxonID,
                    .data$scientificName,
                    .data$family,
                    .data$genus)

  vst_taxonIDs <- sort(taxonID_df$taxonID)

  map <- map %>%
    dplyr::select("individualID",
                  "taxonID")



  ### Prepare and clean vst_apparentindividual data

  ##  Remove apparentIndividual records without necessary perplot data
    appInd$plot_eventID <- paste0(appInd$plotID, "_", appInd$eventID)

    appInd <- appInd %>%
      dplyr::filter(appInd$plot_eventID %in% plot_eventID_list)


  ##  Temporary fix of some eventIDs until fixes are made to portal data
  # appInd$month <- as.numeric(substr(appInd$date, 6, 7))

  # appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2019 &
  #                            as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "WREF",
  #                          "vst_WREF_2019",
  #                          appInd$eventID)
  #--> Portal data fixed as of 2025-06-30

  # appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & as.numeric(substr(appInd$eventID, 10, 13)) == 2016 &
  #                            appInd$siteID == "UKFS",
  #                          "vst_UKFS_2018",
  #                          appInd$eventID)
  #--> Portal data fixed as of 2025-06-30

  # appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & appInd$month >= 7 &
  #                            as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "RMNP" ,
  #                          "vst_RMNP_2018",
  #                          appInd$eventID)
  #--> Portal data fixed as of 2025-06-30

  # appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & appInd$month >= 7 &
  #                            as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "UNDE" ,
  #                          "vst_UNDE_2018",
  #                          appInd$eventID)
  #--> Portal data fixed as of 2025-06-30

  # appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2017 & appInd$month == 12 &
  #                            as.numeric(substr(appInd$eventID, 10, 13)) == 2018 & appInd$siteID == "GUAN" ,
  #                          "vst_GUAN_2017",
  #                          appInd$eventID)
  #--> Portal data fixed as of 2025-06-30
  #appInd$month <- NULL


  ##  Merge vst_apparentindividual table with map and perplot to obtain taxonID field and sampling area fields
  #   Add taxonID to appInd table
  appInd <- merge(appInd,
                  map,
                  by = "individualID",
                  all.x = TRUE)

  #   Add total sampled area fields
  appInd <- merge(appInd,
                  perPlot,
                  by = c("plotID", "eventID", "plot_eventID"),
                  all.x = TRUE)

  #   Create additional identifiers by eventID
  # appInd$plotEvent <- paste(appInd$plotID, appInd$eventID, sep="_") #--> not needed given 'plot_eventID' is identical

  appInd <- dplyr::mutate(.data = appInd,
                          indEvent = paste(.data$individualID, .data$eventID,
                                           sep = "_"),
                          .before = "individualID")

  #   Create list of records that are missing taxonID
  appInd_miss_taxonID <- appInd %>%
    dplyr::filter(is.na(.data$taxonID))

  #   Resolve missing taxonIDs and Betula slash species issue
  appInd$taxonID <- ifelse(is.na(appInd$taxonID),
                           "2PLANT",
                           appInd$taxonID)

  appInd$taxonID <- ifelse(appInd$taxonID == "BEGL/BENA",
                           "BEGL",
                           appInd$taxonID)


  ##  Read in the Chojnacky et al 2014 parameters for each of their 35 defined allometric groups
  parameters <- parameters %>%
    dplyr::select("allometry_ID",
                  "b0",
                  "b1",
                  "minDiameter",
                  "maxDiameter")


  ##  Load wood density, veg type, and other data needed to assign species to Chojnacky allometry groups
  taxon_fields <- taxon_fields
  taxon_fields_list <- unique(taxon_fields$taxonID)


  ##  Load USDA Plants characteristics to get PLANTS.Floristic.Area and Native.Status: Filtered to records that have PLANTS.Floristic.Area, Native.Status, or both
  plantIntTrop <- plantIntTrop

  #   Add tropical floristic area and/or introduced status to taxa derived from vst_mappingandtagging data
  plant_char <- merge(taxonID_df,
                      plantIntTrop,
                      by = "taxonID",
                      all.x = TRUE)


  ##  Programatically assign a Chojnacky allometry_ID based on genus, family, specific gravity, deciduous vs. evergreen, and/or woodland vs. forest habit
  Choj <- merge(taxon_fields,
                plant_char,
                by = "taxonID",
                all = TRUE)

  #   Retain only taxonIDs found in the vst_mappingandtagging data
  Choj <- Choj[Choj$taxonID %in% vst_taxonIDs, ]

  #   Stanardize 'nativeStatus' and 'tropical' LOV elements
  Choj$nativeStatus <- dplyr::if_else(Choj$nativeStatus == "int",
                                      "introduced",
                                      "native",
                                      "native")

  Choj$tropical <- dplyr::if_else(Choj$tropical == "trop",
                                  "tropical",
                                  "temperate",
                                  "temperate")


  ##  Assign Chojnacky allometric equation IDs
  Choj <- Choj %>%
    dplyr::mutate(allometry_ID = dplyr::case_when(

      .data$woodland_vs_forest == "forest" & .data$genus == "Abies" & .data$spg_gcm3 < 0.35 ~ "C1",
      .data$woodland_vs_forest == "forest" & .data$genus == "Abies" & .data$spg_gcm3 >= 0.35 ~ "C2",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" & .data$spg_gcm3 < 0.30 ~ "C3",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" &
        .data$spg_gcm3 >= 0.30 & .data$spg_gcm3 < 0.40 ~ "C4",
      .data$woodland_vs_forest == "forest" & .data$family == "Cupressaceae" & .data$spg_gcm3 >= 0.40 ~ "C5",
      .data$woodland_vs_forest == "forest" & .data$genus == "Larix" ~ "C6",
      .data$woodland_vs_forest == "forest" & .data$genus == "Picea" & .data$spg_gcm3 < 0.35 ~ "C7",
      .data$woodland_vs_forest == "forest" & .data$genus == "Picea" & .data$spg_gcm3 >= 0.35 ~ "C8",
      .data$woodland_vs_forest == "forest" & .data$genus == "Pinus" & .data$spg_gcm3 < 0.45 ~ "C9",
      .data$woodland_vs_forest == "forest" & .data$genus == "Pinus" & .data$spg_gcm3 >= 0.45 ~ "C10",
      .data$woodland_vs_forest == "forest" & .data$genus %in% c("Pseudotsuga", "Taxus") ~ "C11",
      .data$woodland_vs_forest == "forest" & .data$genus == "Tsuga" & .data$spg_gcm3 < 0.40 ~ "C12",
      .data$woodland_vs_forest == "forest" & .data$genus == "Tsuga" & .data$spg_gcm3 >= 0.40 ~ "C13",
      .data$woodland_vs_forest  %in% c("forest", "") & .data$family == "Aceraceae" & .data$spg_gcm3 < 0.50 ~ "H1",
      .data$woodland_vs_forest %in% c("forest", "") & .data$family == "Aceraceae" & .data$spg_gcm3 >= 0.50 ~ "H2",
      .data$family == "Betulaceae" & .data$spg_gcm3 < 0.40 ~ "H3",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.40 & .data$spg_gcm3 < 0.50 ~ "H4",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.50 & .data$spg_gcm3 < 0.60 ~ "H5",
      .data$family == "Betulaceae" & .data$spg_gcm3 >= 0.60 ~ "H6",
      .data$family %in% c("Cornaceae", "Ericaceae", "Lauraceae", "Platanaceae", "Rosaceae", "Ulmaceae") ~ "H7",
      .data$woodland_vs_forest == "forest" & .data$genus == "Carya" ~ "H8",
      .data$woodland_vs_forest == "forest" & .data$family %in% c("Fabaceae", "Juglandaceae") & .data$genus != "Carya" ~ "H9",
      .data$woodland_vs_forest == "forest" & .data$family == "Fagaceae" & .data$decid_vs_ever == "decid" ~ "H10",
      .data$woodland_vs_forest == "forest" & .data$family == "Fagaceae" & .data$decid_vs_ever == "ever" ~ "H11",
      .data$family == "Hamamelidaceae" ~ "H12",
      .data$family %in% c("Hippocastanaceae", "Tiliaceae") ~ "H13",
      .data$family == "Magnoliaceae" ~ "H14",
      .data$family == "Oleaceae" & .data$spg_gcm3 < 0.55 ~ "H15",
      .data$family == "Oleaceae" & .data$spg_gcm3 >= 0.55 ~ "H16",
      .data$family == "Salicaceae" & .data$spg_gcm3 < 0.35 ~ "H17",
      .data$family == "Salicaceae" & .data$spg_gcm3 >= 0.35 ~ "H18",
      .data$woodland_vs_forest == "woodland" & .data$family == "Cupressaceae" ~ "W1",
      .data$woodland_vs_forest == "woodland" & .data$family %in% c("Fabaceae", "Rosaceae") ~ "W2",
      .data$woodland_vs_forest == "woodland" & .data$family == "Fagaceae" ~ "W3",
      .data$woodland_vs_forest == "woodland" & .data$family == "Pinaceae" ~ "W4",
      #   Arbitrarily picked C9 (forest) over C10 (forest spg_gcm3>=0.45) or W4 (woodland)
      .data$taxonID == "PINACE" ~ "C9",
      #   Arbitrarily picked H9 (forest) over W2 (woodland)
      .data$taxonID == "FABACE" ~ "H9",
      TRUE ~ NA

    )) %>%
    dplyr::relocate("allometry_ID",
                    "family",
                    "genus",
                    .before = "taxonID") %>%

    #   Identify taxa not in Chojnacky
    dplyr::mutate(source = ifelse(!is.na(.data$allometry_ID),
                                  "yes_Choj",
                                  "no_Choj")) %>%

    #   Assign default allometry (H7) when taxon not in Chojnacky (H7 is equation incorporating most families)
    dplyr::mutate(allometry_ID = dplyr::case_when(is.na(.data$allometry_ID) ~ "H7",
                                                  TRUE ~ .data$allometry_ID))


  ##  Cache records with taxon not in Chojnacky in new data frame
  no_Choj_allometry <- Choj %>%
    dplyr::filter(.data$source == "no_Choj") %>%
    dplyr::select("allometry_ID",
                  "taxonID",
                  "family",
                  "genus",
                  "spg_gcm3",
                  "woodland_vs_forest",
                  "decid_vs_ever",
                  "tropical",
                  "nativeStatus")


  ##  Reduce 'Choj' to desired columns and merge to associate taxonIDs in data with allometric parameters
  Choj <- Choj %>%
    dplyr::select("allometry_ID",
                  "family",
                  "genus",
                  "taxonID",
                  "spg_gcm3",
                  "scientificName",
                  "nativeStatus",
                  "tropical",
                  "source")

  Choj <- merge(parameters,
                Choj,
                by = "allometry_ID",
                all.y = TRUE)



  ### Calculate biomass using Chojnacky allometric equations
  vst_agb <- merge(appInd,
                   Choj,
                   by = "taxonID",
                   all.x = TRUE)

  #   Manually assign 'tropical' and 'temperate' status for a subset of taxonIDs
  vst_agb <- vst_agb %>%
    dplyr::mutate(tropical = dplyr::case_when(.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("2PLANT", "2PLANT-H", "ANAL12", "BOURR", "BUMI6",
                                                                     "CONVOL", "CROSS", "FABACE", "JACQU", "JACQU2",
                                                                     "COPRO", "HYDRAN") ~ "tropical",
                                              TRUE ~ .data$tropical))

  vst_agb <- vst_agb %>%
    dplyr::mutate(tropical = dplyr::case_when(!.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("AMAR5", "CELTI", "DAWR2", "LIJA", "MEAZ", "OPUNT",
                                                                     "RHUS", "SAMBU", "SMSM", "SYMPL2", "VITIS")
                                              ~ "temperate",
                                              TRUE ~ .data$tropical))

  #   Assign specific gravity data type
  vst_agb$spg_gcm3 <- as.numeric(vst_agb$spg_gcm3)

  #   Assumption: For tropical species, if specific gravity is not known then assume it is 0.5 g/cm3 to permit usage of Chave et al 2014, following precedent of Asner et al 2011
  vst_agb$spg_gcm3 <- dplyr::if_else(is.na(vst_agb$spg_gcm3) & vst_agb$tropical == "tropical",
                                     0.5,
                                     vst_agb$spg_gcm3,
                                     vst_agb$spg_gcm3)

  ##  If growthForm is missing for a record but is available from another instance of the same individualID then populate.
  #-->  There are many dead and downed individuals that are missing growthForm, and without a growthForm and area this leaves out some instances of mortality

  #   Create growthForm look-up table: Retain most recent 'growthForm' by individualID
  growth_form_lookup <- vst_agb %>%
    dplyr::select("individualID",
                  "growthForm",
                  "date") %>%
    dplyr::filter(!is.na(.data$growthForm)) %>%
    dplyr::arrange(.data$individualID,
                   dplyr::desc(.data$date)) %>%
    dplyr::distinct(.data$individualID,
                    .keep_all = TRUE) %>%
    dplyr::select(-"date")

  #   Assign last known growthForm if growthForm is NA
  vst_agb <- dplyr::left_join(vst_agb,
                              growth_form_lookup,
                              by = "individualID",
                              suffix = c("", ".lookup")) %>%
    dplyr::mutate(growthForm = dplyr::if_else(is.na(.data$growthForm),
                                              .data$growthForm.lookup,
                                              .data$growthForm)) %>%
    dplyr::select(-"growthForm.lookup")

  #   Assign growthForm == "unknown" when value is still NA
  vst_agb$growthForm <- dplyr::if_else(is.na(vst_agb$growthForm),
                                       "unknown",
                                       vst_agb$growthForm,
                                       vst_agb$growthForm)

  #   Select columns to remove unneeded data, and add simplified 'plantStatus2' values
  vst_agb <- vst_agb %>%
    dplyr::select("domainID",
                  "siteID",
                  "plotID",
                  "taxonID",
                  "family",
                  "genus",
                  "scientificName",
                  "individualID",
                  "indEvent",
                  "eventID",
                  "date",
                  "growthForm",
                  "nlcdClass",
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana",
                  "plantStatus",
                  "stemDiameter",
                  "height",
                  "measurementHeight",
                  "basalStemDiameter",
                  "basalStemDiameterMsrmntHeight",
                  "maxCrownDiameter",
                  "ninetyCrownDiameter",
                  "allometry_ID",
                  "b0",
                  "b1",
                  "minDiameter",
                  "maxDiameter",
                  "spg_gcm3",
                  "nativeStatus",
                  "tropical") %>%
    dplyr::mutate(plantStatus2 = dplyr::case_when(.data$plantStatus %in% head(standingLiveDead, -2) ~ "Live",
                                                  TRUE ~ "Dead_or_Lost"),
                  .after = "plantStatus")

  #   Correct negative ninetyCrownDiameter: Meaningless and generates NAN warnings in some allometric equations
  vst_agb$ninetyCrownDiameter <- dplyr::if_else(vst_agb$ninetyCrownDiameter < 0,
                                                NA,
                                                vst_agb$ninetyCrownDiameter)


  ##  Combine basal area of shrubs belonging to same individualID and calculate round diameter equivalent
  nonShrub <- vst_agb %>%
    dplyr::filter((.data$growthForm != "single shrub" & .data$growthForm != "small shrub") | is.na(.data$growthForm))

  shrub <- vst_agb %>%
    dplyr::filter(.data$growthForm == "single shrub" | .data$growthForm == "small shrub")

  shrub$basalArea <- pi * ((shrub$basalStemDiameter/2)^2)
  shrub$basalAreaDBH <- pi * ((shrub$stemDiameter/2)^2)

  #   Group multiple stems belonging to same individualID, sum basal area, and take the mean of the other quantitative values
  shrub <- shrub %>%
    dplyr::group_by(.data$domainID,
                    .data$siteID,
                    .data$plotID,
                    .data$taxonID,
                    .data$family,
                    .data$genus,
                    .data$scientificName,
                    .data$individualID,
                    .data$indEvent,
                    .data$eventID,
                    .data$date,
                    .data$growthForm,
                    .data$nlcdClass,
                    .data$totalSampledAreaTrees,
                    .data$totalSampledAreaShrubSapling,
                    .data$totalSampledAreaLiana,
                    .data$plantStatus,
                    .data$plantStatus2,
                    .data$allometry_ID,
                    .data$b0,
                    .data$b1,
                    .data$minDiameter,
                    .data$maxDiameter,
                    .data$spg_gcm3,
                    .data$nativeStatus,
                    .data$tropical) %>%
    dplyr::summarise(basalArea = ifelse(!all(is.na(.data$basalArea)),
                                        round(sum(.data$basalArea, na.rm = TRUE), digits = 2),
                                        NA),
                     basalAreaDBH = ifelse(!all(is.na(.data$basalAreaDBH)),
                                           round(sum(.data$basalAreaDBH, na.rm = TRUE), digits = 2),
                                           NA),
                     height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA),
                     maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                               max(.data$maxCrownDiameter, na.rm = TRUE),
                                               NA),
                     ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                  max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                  NA),
                     measurementHeight = ifelse(!all(is.na(.data$measurementHeight)),
                                                round(mean(.data$measurementHeight, na.rm = TRUE), digits = 0),
                                                NA),
                     basalStemDiameterMsrmntHeight = ifelse(!all(is.na(.data$basalStemDiameterMsrmntHeight)),
                                                round(mean(.data$basalStemDiameterMsrmntHeight, na.rm = TRUE), digits = 0),
                                                NA),
                     .groups = "drop") %>%

    #   Calculate round diameter equivalent of the combined stems belonging to the same individualID
    dplyr::mutate(stemDiameter = round(2 * (sqrt(.data$basalAreaDBH / pi)), digits = 1),
                  basalStemDiameter = round(2 * (sqrt(.data$basalArea / pi)), digits = 1),
                  .before = "basalArea") %>%

    #   Remove unneeded columns
    dplyr::select(-"basalArea",
                  -"basalAreaDBH")


  ##  Bind 'nonShrub' and 'shrub' together into simplified dataframe
  vst_agb <- dplyr::bind_rows(nonShrub,
                              shrub)



  ### Calculate AGB for each VST appInd record using Choj allometry_ID and Choj parameters

  # Assumption: Chojnacky et al 2014 allometric equations are the best first estimate of biomass
  vst_agb$agb<- round(exp(vst_agb$b0 + vst_agb$b1 * log(vst_agb$stemDiameter)),
                      digits = 3)

  #   Define AGB allometry source
  vst_agb$agb_source <- "Chojnacky_et_al_2014"

  #   Assign Chojnacky AGB estimates to specific column; needed to preserve Chojnacky estimates when alternate is used for tropical or introduced species.
  vst_agb$agb_Chojnacky  <- vst_agb$agb

  #   Assumption: When the necessary ancillary variables are available for tropical species, replace the Chojnacky et al 2014 biomass estimates with the Chave et al 2014 biomass estimates.
    # Update tropical species records based on Chave et al 2014 if wood specific gravity is available (or an approximation based on congeners).
    # Instructions on extracting environmental stress value E at http://chave.ups-tlse.fr/pantropical_allometry.html; Chave et al 2014 has pantropical allometric equations for tree biomass that require tree height. If tree height is not available, estimate it using their value E.
  #   Chave et al 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20:3177-3190
  # install.packages("raster"); install.packages("ncdf4"); library("raster"); library("ncdf4")
  # source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
  # coord <- data.frame(siteID = c("GUAN", "LAJA", "PUUM"), longitude = c(-66.8687, -67.07689, -155.31731), latitude = c(17.96955, 18.02126, 19.55309) );  rownames(coord) <- coord$siteID; coord$siteID <- NULL
  # Chave_et_al_2014_E <- retrieve_raster("E",coord,plot=TRUE,format="nc") returns an E of 0.5074847 for GUAN, 0.4440793 for LAJA, and NA for PUUM

  #   Assign Chave et al 2014 "E" values needed for site-specific height estimation when height is missing
  vst_agb$Chave_E <- ifelse(vst_agb$siteID == "GUAN",
                            0.5074847,
                            NA)

  vst_agb$Chave_E <- ifelse(vst_agb$siteID == "LAJA",
                            0.4440793,
                            vst_agb$Chave_E)

  vst_agb$height_eval <- ifelse(is.na(vst_agb$height),
                                0,
                                1)

  #   Estimate AGB for tropical species
  vst_agb$agb_trop <- ifelse(vst_agb$tropical == "tropical" & vst_agb$spg_gcm3 >= 0 & vst_agb$stemDiameter >= 0,
                             round(exp(-1.803 - (0.976 * vst_agb$Chave_E) + (0.976 * log(vst_agb$spg_gcm3)) +
                                         (2.673 * log(vst_agb$stemDiameter)) - (0.0299 * (log(vst_agb$stemDiameter))^2)),
                                   digits = 3),
                             NA)

  #   Overwrite AGB for tropical species when "height" is present
  vst_agb$agb_trop <- dplyr::if_else(vst_agb$tropical == "tropical" & vst_agb$height_eval == 1 & vst_agb$spg_gcm3 >= 0 &
                                       vst_agb$stemDiameter >= 0,
                                     round(0.0673 * (vst_agb$spg_gcm3 * (vst_agb$stemDiameter^2) * vst_agb$height)^0.976,
                                           digits = 3),
                                     vst_agb$agb_trop,
                                     vst_agb$agb_trop)

  #   Remove temporary "height_eval" variable
  vst_agb$height_eval <- NULL

  #   Assign AGB allometry source for tropical species
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_trop),
                               "Chave_et_al_2014",
                               vst_agb$agb_source)

  #   Populate "agb" biomass column with Chave estimates for tropical species, otherwise use Chojnacky
  vst_agb$agb <- ifelse(vst_agb$agb_source == "Chave_et_al_2014",
                        vst_agb$agb_trop,
                        vst_agb$agb)



  ### Apply shrub-specific biomass equations from Conti et al. 2019 to shrub growth forms
  # Note: Conti et al. 2019 assume that multiple stems of same individual have been aggregated and to a single record with basalStemDiameter a round diameter equivalent of all stems
  # This calculation was performed earlier in script on shrub dataframe subset

  #   Estimate shrub biomass: Case when basalStemDiameter is missing and crown diameter and height are available (biomass estimate with most uncertainty)
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) &
                                   !is.na(vst_agb$height)),
                              round(exp(-0.370 + 1.903 * log((vst_agb$maxCrownDiameter+vst_agb$ninetyCrownDiameter)/2) +
                                          0.652 * log(vst_agb$height)) * 1.403,
                                    digits = 3),
                              NA)

  #   Estimate shrub biomass: Improved output when basalStemDiameter is available --> less uncertainty
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                !is.na(vst_agb$basalStemDiameter),
                              round(exp(-2.869 + 2.584 * log(vst_agb$basalStemDiameter)),
                                    digits = 3),
                              vst_agb$agb_shrub)

  #   Estimate shrub biomass: Even better output when basalStemDiameter AND crownDiameter available (compared to basalStemDiameter alone)
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) &
                                   !is.na(vst_agb$basalStemDiameter)),
                              round(exp(-2.057 + 1.741 * log(vst_agb$basalStemDiameter) + 0.945 *
                                          log((vst_agb$maxCrownDiameter + vst_agb$ninetyCrownDiameter)/2)),
                                    digits = 3),
                              vst_agb$agb_shrub)

  #   Estimate shrub biomass: Best output when basalStemDiameter, crownDiameter, AND height are all available
  vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) &
                                   !is.na(vst_agb$basalStemDiameter) & !is.na(vst_agb$height)),
                              round(exp(-2.281 + 1.525 * log(vst_agb$basalStemDiameter) + 0.831 *
                                          log((vst_agb$maxCrownDiameter + vst_agb$ninetyCrownDiameter)/2) + 0.523 *
                                          log(vst_agb$height)),
                                    digits = 3),
                              vst_agb$agb_shrub)

  #   Assign AGB allometry source for shrubs. Citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub),
                               "Conti_et_al_2019",
                               vst_agb$agb_source)

  #   Update AGB column with shrub biomass from Conti
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019",
                                vst_agb$agb_shrub,
                                vst_agb$agb,
                                vst_agb$agb)



  ### Assumption: Where available, species-specific allometric equations are preferable to more generic ones; update AGB estimates for taxa for which species-specific allometric equations exist

  ##  Species: Metrosideros polymorpha (MEPO5) - first estimate AGB for all MEPO5 with a stemDiameter (Litton and Kauffman 2008), then in subsequent steps update the AGB estimate with Selmants et al 2014;
   # approach retains Litton and Kauffman 2008 estimate for those MEPO5 with DBH >= 33 cm and that do not have 'height' recorded. All other individuals have an AGB estimate via Selmants et al 2014.

  #   Allometry for all MEPO5 individuals; citation: Litton and Kauffman 2008. Allometric Models for Predicting Aboveground Biomass in Two Widespread WoodyPlants in Hawaii. BIOTROPICA 40(3): 313-320.
  vst_agb$agb_MEPO5_Litton <- ifelse(vst_agb$taxonID == "MEPO5",
                                     round(0.88 * (vst_agb$stemDiameter^1.86),
                                           digits = 3),
                                     NA)

  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_MEPO5_Litton),
                               "Litton_and_Kauffman_2008",
                               vst_agb$agb_source)

  vst_agb$agb <- ifelse(vst_agb$agb_source == "Litton_and_Kauffman_2008",
                        vst_agb$agb_MEPO5,
                        vst_agb$agb)

  #   Update MEPO5 AGB estimate for individuals with DBH <= 33 cm or > 33 cm AND with 'height' data; citation: Selmants, PC, CM Litton, CP Giardina, and GP Asner. 2014. Global Change Biology 20:2927-2937.
  vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter <= 33,
                              round(0.2085 * (vst_agb$stemDiameter^2.318),
                                    digits = 3),
                              NA)

  vst_agb$agb_MEPO5 <- ifelse(vst_agb$taxonID == "MEPO5" & vst_agb$stemDiameter > 33,
                              round(0.0776 * ((vst_agb$spg_gcm3 * (vst_agb$stemDiameter^2) * vst_agb$height)^0.94),
                                    digits = 3),
                              vst_agb$agb_MEPO5)

  #   Update AGB allometry for MEPO5 that have a new value in "agb_MEPO5" column
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_MEPO5),
                               "Selmants_et_al_2014",
                               vst_agb$agb_source)

  #   Update "agb" column with Selmants et al 2014 estimates
  vst_agb$agb <- ifelse(vst_agb$agb_source == "Selmants_et_al_2014",
                        vst_agb$agb_MEPO5,
                        vst_agb$agb)


  ##  Species: Rhamnus davurica (RHDA); citation: Zhang et al 2012. Sexual dimorphism in reproductive and vegetative allometry for two dioecious Rhamnus plants in north-eastern China. Eur J Forest Res (2012) 131:1287-1296.
   # The taxonID RHDA is the most frequent introduced species in NEON VST dataset, and Zhang et al 2012 have a specific equation for RHDA. There is one equation for males and another for females; here, we take the average because NEON does not record sex of RHDA. Output is divided by 1000 to convert to "kg".
  vst_agb$agb_RHDA <- ifelse(vst_agb$taxonID == "RHDA",
                             round(0.001 * exp(((5.237 + 1.996 * log(vst_agb$stemDiameter)) +
                                                  (5.016 + 2.306 * log(vst_agb$stemDiameter)))/2),
                                   digits = 3),
                             NA)

  #   Update AGB allometry for RHDA individuals
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_RHDA),
                               "Zhang_et_al_2012",
                               vst_agb$agb_source)

  #   Update "agb" column with Zhang et al 2012 estimates
  vst_agb$agb <- ifelse(vst_agb$agb_source == "Zhang_et_al_2012",
                        vst_agb$agb_RHDA,
                        vst_agb$agb)


  ##  Species: Artemisia tridentata (ARTR2); citation: Cleary, M.B., E. Pendall, and B.E. Ewers. 2008. Testing sagebrush allometric relationships across three fire chronosequences in Wyoming, USA. Journal of Arid Environments 72:285-301. Output is divided by 1000 to convert to "kg".

  vst_agb$agb_ARTR2 <- ifelse(vst_agb$taxonID == "ARTR2",
                              round(exp(7.889 + 0.8539 * log(4/3 * pi * (vst_agb$maxCrownDiameter/2) *
                                                               (vst_agb$ninetyCrownDiameter/2)))/1000,
                                    digits = 3),
                              NA)

  #   Update AGB allometry for ARTR2 individuals
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_ARTR2),
                               "Cleary_et_al_2008_ARTR2",
                               vst_agb$agb_source)

  #   Update AGB column with Cleary et al 2008 estimates
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Cleary_et_al_2008_ARTR2",
                                vst_agb$agb_ARTR2,
                                vst_agb$agb,
                                vst_agb$agb)



  ##  Species: Cornus spp; citation: Lutz, J.A., K.A. Schwindt, T.J. Furniss, J.A. Freund, M.E Swanson, K.J. Hogan, G.E. Kenagy, and A.J. Larson. 2014. Community composition and allometry of Leucothoe davisiae, Cornus sericea, and Chrysolepis sempervirens. Canadian Journal of Forest Research 44:677-683
  #   Estimate AGB for individuals with a basalStemDiameter; most emergent shrub stems have basalStemDiameter but a small number are occluded from measurement.
  vst_agb$agb_Cornus <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                 substr(vst_agb$scientificName, 1, 6) == "Cornus",
                               round(exp(3.315 + 2.647 * log(vst_agb$basalStemDiameter))/1000,
                                     digits = 3),
                               NA)

  #   Estimate AGB using stemDiameter for individuals lacking a basalStemDiameter
  vst_agb$agb_Cornus <- dplyr::if_else((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                         substr(vst_agb$scientificName, 1, 6) == "Cornus" & is.na(vst_agb$basalStemDiameter),
                                       round(exp(5.089 + 1.883 * log(vst_agb$stemDiameter))/1000,
                                             digits = 3),
                                       vst_agb$agb_Cornus,
                                       vst_agb$agb_Cornus)

  #   Update AGB allometry for Cornus spp individuals
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_Cornus),
                               "Lutz_et_al_2014",
                               vst_agb$agb_source)

  #   Update AGB column with Lutz et al 2014 estimates
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Lutz_et_al_2014",
                                vst_agb$agb_Cornus,
                                vst_agb$agb,
                                vst_agb$agb)


  ##  Assumption: Allometric equations developed specifically for lianas are better than generic allometric equations used above for trees and shrubs. Citation: Schnitzer, SA, SJ DeWalt, and J Chave. 2006. Censusing and measuring lianas: A quantitative comparison of the common methods. Biotropica 38:581-591.
   # Update AGB for lianas with equations from Schnitzer_et_al_2006 (Chojnacky is not intended for lianas, or for introduced or tropical species, and there are numerous introduced and tropical liana species, see below). Equation for tropical lianas is used for temperate liana species.
  vst_agb$agb_liana <- ifelse(vst_agb$growthForm == "liana",
                              round(exp(-1.484 + 2.657 * log(vst_agb$stemDiameter)),
                                    digits = 3),
                              NA)

  #   Update AGB allometry for lianas
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_liana),
                               "Schnitzer_et_al_2006",
                               vst_agb$agb_source)

  #   Update AGB column with Schnitzer et al 2006 estimates
  vst_agb$agb <- ifelse(vst_agb$agb_source == "Schnitzer_et_al_2006",
                        vst_agb$agb_liana,
                        vst_agb$agb)


  ##  Species: Cibotium spp. (taxonIDs CIBOT, CIGL, and CIME8); citation: Ostertag, R, F Inman-Narahari, S Cordell, CP Giardina, and L Sack. 2014. Forest Structure in low-diversity tropical forests: A study of Hawaiian wet and dry forests. PLOS One. 9:e103268
  vst_agb$agb_Cibotium <- ifelse(substring(vst_agb$scientificName, 1, 8) == "Cibotium",
                                 round(0.2085 * (pi * (vst_agb$stemDiameter/2)^2 * vst_agb$height * 100 * vst_agb$spg_gcm3/1000),
                                       digits = 3),
                                 NA)

  #   Update AGB allometry for Cibotium
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_Cibotium),
                               "Ostertag_et_al_2014",
                               vst_agb$agb_source)

  #   Update AGB column with Ostertag et al 2014 estimates
  vst_agb$agb <- ifelse(vst_agb$agb_source == "Ostertag_et_al_2014",
                        vst_agb$agb_Cibotium,
                        vst_agb$agb)


  ##  Assign quality flag: stemDiameter in Chojnacky range
  vst_agb$agb_in_dia_range <- dplyr::if_else(vst_agb$stemDiameter >= vst_agb$minDiameter & vst_agb$stemDiameter <= vst_agb$maxDiameter &
                                               vst_agb$agb_source == "Chojnacky_et_al_2014",
                                             "yes",
                                             "no",
                                             "")

  vst_agb$agb_in_dia_range <- ifelse(vst_agb$agb_source != "Chojnacky_et_al_2014",
                                     NA,
                                     vst_agb$agb_in_dia_range)


  ##  Assumption: it's better to make assumptions about growthForm based on stemDiameter than to leave out records when scaling to area basis (which requires knowing what the the growthForm is)
  #   Assume growthForm == "single bole tree" if stemDiameter >= 10 cm
  vst_agb$growthForm <- dplyr::if_else(vst_agb$growthForm == "unknown" & vst_agb$stemDiameter >= 10,
                                       "single bole tree",
                                       vst_agb$growthForm,
                                       vst_agb$growthForm)


  ##  Create unique plot x eventID identifier and year variables
  vst_agb$plot_eventID <- paste0(vst_agb$plotID, "_", vst_agb$eventID)

  vst_agb$year <- as.numeric(substr(vst_agb$eventID, 10, 13))

  ##  Retain only those records with unambiguous live or dead plantStatus values
  vst_agb <- vst_agb %>%
    dplyr::filter(!is.na(.data$plantStatus) & .data$plantStatus != "No longer qualifies" & .data$plantStatus != "Removed" &
                    .data$plantStatus != "Lost, fate unknown" & .data$plantStatus != "Lost, tag damaged" &
                    .data$plantStatus != "Lost, herbivory" & .data$plantStatus != "Lost, burned" &
                    .data$plantStatus != "Lost, presumed dead")

  ##  Set aside individuals that are dead but have no mass, for use in productivity function mortality calculations
   vst_deadNoMass <- vst_agb %>%
    dplyr::filter(is.na(.data$agb) & .data$plantStatus2 == "Dead_or_Lost")

   if(nrow(vst_deadNoMass) > 0){
   vst_deadNoMass  <- merge(vst_deadNoMass, perplot, by = c("plot_eventID", "eventID", "plotID", "year","nlcdClass"), all.x = TRUE)

   vst_deadNoMass <- vst_deadNoMass %>% dplyr::select("plot_eventID", "eventID", "siteID", "plotID", "taxonID", "scientificName", "family",
                                               "genus", "individualID", "plantStatus2", "growthForm", "year", "nlcdClass", "plotType", "eventType")

   vst_deadNoMass$sampledAreaM2 <- vst_deadNoMass$agb_kg <- NA
   }

  ##  Aggregate woody biomass data by individualID within 'year'
  #   Remove records with NA biomass estimate to avoid assigning to zero due to group_by() output
  vst_agb <- vst_agb %>%
    dplyr::filter(!is.na(.data$agb))

  #   Aggregate woody biomass data by 'individualID', 'plantStatus', and 'year': Assumes that multiple instances of same individualID are true multiple boles and not accidental duplicates. Output is used for both annual biomass summaries and NPP calculations for specified consecutive years.
  vst_agb_final <- vst_agb %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$eventID,
                    .data$siteID,
                    .data$plotID,
                    .data$taxonID,
                    .data$scientificName,
                    .data$family,
                    .data$genus,
                    .data$individualID,
                    .data$plantStatus2,
                    .data$growthForm,
                    .data$year) %>%
    dplyr::summarise(agb = sum(.data$agb, na.rm = TRUE),
                     .groups = "drop")


  ##  Join AGB data with sampled areas from per plot input: Prep step for plot- and site-level biomass calculations
  vst_agb_final  <- merge(vst_agb_final,
                          perplot,
                          by = c("plot_eventID", "eventID", "plotID", "year"),
                          all.x = TRUE)

  #   Assign total sampled area for trees
  vst_agb_final$sampledAreaM2 <- ifelse(vst_agb_final$growthForm == "single bole tree" | vst_agb_final$growthForm == "multi-bole tree",
                                        vst_agb_final$totalSampledAreaTrees,
                                        NA)

  #   Assign total sampled area for saplings, small trees, shrubs, and small shrubs
  vst_agb_final$sampledAreaM2 <- ifelse(vst_agb_final$growthForm == "single shrub" | vst_agb_final$growthForm == "small shrub" |
                                          vst_agb_final$growthForm == "sapling"  | vst_agb_final$growthForm == "small tree",
                                        vst_agb_final$totalSampledAreaShrubSapling,
                                        vst_agb_final$sampledAreaM2)

  #   Assign total sampled area for lianas
  vst_agb_final$sampledAreaM2 <- ifelse(vst_agb_final$growthForm == "liana",
                                        vst_agb_final$totalSampledAreaLiana,
                                        vst_agb_final$sampledAreaM2)


  ### Combine AGB for vst_apparentindividual (vst_agb_Mg_final) and vst_nonWoody (vst_agb_Mg_other)

  if (methods::is(vst_nonWoody, class = "data.frame" )) {
       vst_agb_final_other$family <- vst_agb_final_other$genus <- NA
       vst_agb_kg <- rbind(vst_agb_final, vst_agb_final_other)
  } else {
    vst_agb_kg <- vst_agb_final
  }

  vst_agb_kg <- vst_agb_kg %>%
    dplyr::select(-"totalSampledAreaTrees",
                  -"totalSampledAreaShrubSapling",
                  -"totalSampledAreaLiana",
                  -"totalSampledAreaOther")

    vst_agb_kg$agb_kg <- vst_agb_kg$agb ; vst_agb_kg$agb <- NULL # add units to field name (rename created devtools binding error)

  #   Filter combined output by user-supplied 'growthForm' argument
  if (growthForm == "other") {

    vst_agb_kg <- vst_agb_kg %>%
      dplyr::filter(.data$growthForm == "tree fern" | .data$growthForm == "small tree fern" | .data$growthForm == "large tree fern"  |
                      .data$growthForm == "palm" | .data$growthForm == "small palm" | .data$growthForm == "palm tree" |
                      .data$growthForm == "ocotillo" | .data$growthForm == "xerophyllum"  | .data$growthForm == "yucca")

  }

  if (growthForm == "shrubLiana") {

    vst_agb_kg <- vst_agb_kg %>%
      dplyr::filter(.data$growthForm == "single shrub" | .data$growthForm == "small shrub" | .data$growthForm == "liana")

  }

  if (growthForm == "tree") {

    vst_agb_kg <- vst_agb_kg %>%
      dplyr::filter(.data$growthForm == "single bole tree" | .data$growthForm == "multi-bole tree")

  }

    #################################################################################
    ############ Scale biomass per area and convert to Mg / ha ######################
  #   Remove records that cannot be scaled to a per area basis
  vst_agb_Mgha <- vst_agb_kg %>%
    dplyr::filter(!is.na(.data$sampledAreaM2) & .data$sampledAreaM2 > 0 )

  #   Create "Mg/ha" biomass estimate for each record; used in downstream plot- and site-level biomass estimation
  vst_agb_Mgha$agb_Mgha <- round(vst_agb_Mgha$agb_kg * 0.001 * (10000/vst_agb_Mgha$sampledAreaM2),
                                  digits = 4)



  ##  Create list of plot x eventIDs from vst_apparentindividaul data; later diff against what is reported in perplot data
  agb_ind_eventID_list <- unique(vst_agb_Mgha$plot_eventID)


  ##  Identify plot by eventID combos that don't have biomass values
  vst_agb_zeros <- base::setdiff(plot_eventID_list, agb_ind_eventID_list)

  vst_agb_zeros <- as.data.frame(vst_agb_zeros)

  vst_agb_zeros <- dplyr::rename(vst_agb_zeros,
                                 plot_eventID = vst_agb_zeros)

  vst_agb_zeros$plotID <- substr(vst_agb_zeros$plot_eventID, 1, 8)

  vst_agb_zeros$siteID <- substr(vst_agb_zeros$plot_eventID, 1, 4)

  vst_agb_zeros$year <- as.numeric(substr(vst_agb_zeros$plot_eventID, 19, 22))

  vst_agb_zeros$eventID <- substr(vst_agb_zeros$plot_eventID, 10, 22)

  vst_agb_zeros <- merge(vst_agb_zeros,
                         plotType_df,
                         by = "plotID",
                         all.x = TRUE)



  ### Generate plot-level biomass summary ####

  #   Sum biomass per unit area for each plot x year x plantStatus2 x nlcdClass x taxonID combo (aggegate across individualIDs)
  vst_plot_summary <- vst_agb_Mgha %>%
    dplyr::group_by(.data$plot_eventID,
                    .data$eventID,
                    .data$siteID,
                    .data$plotID,
                    .data$sampledAreaM2,
                    .data$eventType,
                    .data$plotType,
                    .data$nlcdClass,
                    .data$taxonID,
                    .data$growthForm,
                    .data$plantStatus2,
                    .data$year) %>%
    dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE),
                     .groups = "drop")

  #   Within a given year, transpose live and dead AGB into separate columns
  vst_plot_wide <- tidyr::pivot_wider(vst_plot_summary,
                                      id_cols = c("plot_eventID", "eventID", "siteID", "plotID",  "sampledAreaM2", "eventType",
                                                  "plotType", "nlcdClass", "taxonID", "growthForm", "year"),
                                      names_from = "plantStatus2",
                                      names_glue = "{plantStatus2}_Mgha",
                                      values_from = "agb_Mgha")
  if (!"Dead_or_Lost_Mgha" %in% names(vst_plot_wide)) {
    vst_plot_wide$Dead_or_Lost_Mgha <- NA
  }


  #   Assumption: Replace NAs created during transpose with zeroes; assume both live and dead were sampled in a plot

  vst_plot_wide$Dead_or_Lost_Mgha[is.na(vst_plot_wide$Dead_or_Lost_Mgha)] <- 0
  vst_plot_wide$Live_Mgha[is.na(vst_plot_wide$Live_Mgha)] <- 0


  #   Assign zero AGB values to plots with zero biomass
  vst_agb_zeros_plot <- vst_agb_zeros

  if (nrow(vst_agb_zeros_plot) > 0) {
    perplot_meta_for_missing <- unique(perplot %>% dplyr::select("plotID", "eventID", "eventType", "nlcdClass"))
    vst_agb_zeros_plot <- merge(vst_agb_zeros_plot, perplot_meta_for_missing, by = c("plotID", "eventID"), all.x = T)
    vst_agb_zeros_plot$taxonID <-  vst_agb_zeros_plot$growthForm <- vst_agb_zeros_plot$sampledAreaM2 <- NA # placeholders to allow rbind without errors
    vst_agb_zeros_plot$Dead_or_Lost_Mgha <- vst_agb_zeros_plot$Live_Mgha <- 0
  }

  #   Add rows for plots with zero biomass to plots with AGB
  vst_plot_w_0s <- rbind(vst_plot_wide,
                         vst_agb_zeros_plot)

  #   Add 'specificModuleSamplingPriority' column to output
  vst_plot_w_0s <- merge(vst_plot_w_0s,
                         priority_plots,
                         by = c("plotID"),
                         all.x = TRUE)

  #   Retain AGB estimates for records with values for both live and dead biomass
  vst_plot_w_0s <- vst_plot_w_0s %>%
    dplyr::filter(!is.na(.data$Live_Mgha) & !is.na(.data$Dead_or_Lost_Mgha))


  #   Remove records based on plotType and plotPriority parameters
  if (plotType == "distributed") {

    vst_plot_w_0s <- vst_plot_w_0s %>%
      dplyr::filter(.data$plotType == "distributed")

  }

  if (plotType == "tower") {

    vst_plot_w_0s <- vst_plot_w_0s %>%
      dplyr::filter(.data$plotType == "tower")

  }

  vst_plot_w_0s <- vst_plot_w_0s %>%
    dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)



  ### Generate site-level biomass summary ####

  #   First sum AGB at the plot level - i.e., remove intra-plot granularity from growthForm, taxonID, etc.
  vst_plot_w_0s_sum_taxa <- vst_plot_w_0s

  if(nrow(vst_plot_w_0s_sum_taxa) > 0) {

    vst_plot_w_0s_sum_taxa <- vst_plot_w_0s %>%
      dplyr::group_by(.data$plot_eventID,
                      .data$eventID,
                      .data$siteID,
                      .data$plotID,
                      .data$eventType,
                      .data$sampledAreaM2,
                      .data$plotType,
                      .data$nlcdClass,
                      .data$year) %>%
      dplyr::summarise(Live_Mgha = sum(.data$Live_Mgha, na.rm = TRUE),
                       Dead_or_Lost_Mgha = sum(.data$Dead_or_Lost_Mgha, na.rm = TRUE),
                       .groups = "drop")

  } else {

    vst_plot_w_0s_sum_taxa$Live_Mgha = 0
    vst_plot_w_0s_sum_taxa$Dead_or_Lost_Mgha = 0

  }

  #   Create site-level summary table: mean, sd, n()
  vst_site <- vst_plot_w_0s_sum_taxa %>%
    dplyr::group_by(.data$siteID,
                    .data$year) %>%
    dplyr::summarise(woodPlotNum = dplyr::n(),
                     woodLiveMassMean_Mgha = round(mean(.data$Live_Mgha, na.rm = TRUE),
                                                   digits = 1),
                     woodLiveMassSD_Mgha = round(stats::sd(.data$Live_Mgha, na.rm = TRUE),
                                                 digits = 2),
                     woodDeadMassMean_Mgha = round(mean(.data$Dead_or_Lost_Mgha, na.rm = TRUE),
                                                   digits = 1),
                     woodDeadMassSD_Mgha = round(stats::sd(.data$Dead_or_Lost_Mgha, na.rm = TRUE),
                                                 digits = 2),
                     .groups = "drop")


  #  Remove 'plotType' field since only partially populated, then add back complete 'plotType' and 'domainID'
  vst_plot_w_0s$plotType <- NULL

  vst_plot_w_0s <- merge(vst_plot_w_0s,
                         plotType_df,
                         by = "plotID",
                         all.x = TRUE)

  domainID_df <- unique(appInd %>% dplyr::select("siteID", "domainID"))

  vst_plot_w_0s <- merge(vst_plot_w_0s,
                         domainID_df,
                         by = "siteID",
                         all.x = TRUE)



  ### Bundle and return output
  message("Returning woody biomass output data frames as a list object  ..... ")

  output.list <- list(vst_agb_kg = vst_agb_kg,
                      vst_plot_w_0s = vst_plot_w_0s,
                      vst_agb_zeros = vst_agb_zeros,
                      vst_deadNoMass = vst_deadNoMass,
                      vst_site = vst_site)

  return(output.list)
}
