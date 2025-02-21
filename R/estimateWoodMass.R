#' @title Estimate above-ground biomass of woody vegetation
#'
#' @author Samuel M Simkin \email{ssimkin@battelleecology.org} \cr
#'
#' @description Use allometric equations to estimate above-ground biomass for woody individuals and summarise results as mass per unit area for siteID, plotID, and taxonID. Biomass outputs can be used in the neonPlants estimateMass() function, and the estimateWoodProd() and estimateProd() productivity functions. 
#' 
#' Data inputs are "Vegetation structure" data (DP1.10098.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input tables with an equivalent structure and representing the same site x month combinations.
#' 
#' @details Input data can be filtered by plot type and plot priority. Input data are combined with allometric equation parameters and taxon specific characteristics, and biomass is estimated for each individual using allometric equations. Generalized allometric equations are applied first and are replaced by taxon-specific equations if available. Only the set of growth forms selected via the growthForm parameter are included in outputs. The non-woody "cactus" and "ferns" growthForms are not currently included. Biomass is summarized on an areal basis at the hierarchical level of the plot and site.
#'
#' @param inputDataList A list object comprised of "Vegetation structure" tables (DP1.10098.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided for 'inputIndividual', 'inputMapTag', and 'inputPerPlot' arguments. [list]
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
#' @param growthForm Select which growth forms to analyse. Options are "all", "tree", "shrubLiana", or "other". Consult the Vegetation Structure Quick Start Guide and/or the Data Product User Guide for more growth form information. [character]
#' 
#' @param plotType Optional filter for NEON plot type. Options are "tower" (default) or "all". A subset of 5 Tower plots are sampled annually (those plots with the highest plot priority), and remaining Tower plots are scheduled every 5 years. If "all" is selected, results include data from Distributed plots that are also sampled every 5 years. [character]
#' 
#' @param plotPriority NEON plots have a priority number in the event that not all scheduled plots can be sampled. The lower the number the higher the priority. The default is the 5 highest priority Tower plots that are sampled annually. [integer]
#'
#' @return A list that includes biomass summary data frames and a helper data frame for needed by companion productivity functions - e.g., the estimateProd() function. Output tables include:
#'   * vst_agb_per_ha - Summarizes above-ground live woody biomass for each individual ("Mg/ha").
#'   * vst_plot_w_0s - Summarizes above-ground live and dead woody biomass for each plot ("Mg/ha").
#'   * vst_agb_zeros - Helper output for productivity estimation that contains plot x year combinations with biomass of zero.
#'   * vst_site - Summarizes above-ground live and dead woody biomass for each site x year combination in the data. ("Mg/ha").
#'
#' @examples
#' \dontrun{
#' # Obtain NEON Vegetation structure
#' VstDat <- neonUtilities::loadByProduct(
#' dpID="DP1.10098.001", 
#' package = "basic", 
#' check.size = FALSE
#' )
#' 
#' # example with arguments at default values
#' estimateWoodMassOutputs <- estimateWoodMass(inputDataList = VstDat)
#' 
#' # example specifying several non-default arguments
#' estimateWoodMassOutputs <- estimateWoodMass(
#' inputDataList = VstDat,
#' growthForm = "tree",
#' plotPriority = 4
#' )
#' 
#' }
#' 
#' @export estimateWoodMass


estimateWoodMass = function(inputDataList, 
                            inputIndividual = NA, 
                            inputMapTag = NA, 
                            inputPerPlot = NA, 
                            growthForm = "all",
                            plotType = "tower",
                            plotPriority = 5) {
  
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
    listExpNames <- c("vst_apparentindividual", "vst_mappingandtagging", "vst_perplotperyear", "vst_non-woody")
    
    
    #   All expected tables required when includeDilution == TRUE
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
      (!is.logical(inputIndividual) | !is.logical(inputMapTag) | !is.logical(inputPerPlot))) {
    stop("When 'inputDataList' is supplied all table input arguments must be NA")
  }
  
  ### Verify 'inputIndividual', 'inputMapTag', and 'inputPerPlot' are data frames if 'inputDataList' is missing
  if (is.null(inputDataList) & 
      (!inherits(inputIndividual, "data.frame") | !inherits(inputMapTag, "data.frame")  |
       !inherits(inputPerPlot, "data.frame"))) {
    
    stop("Data frames must be supplied for all table inputs if 'inputDataList' is not provided")
  }
  
  
  #   Assign input independent standardized names
  if (inherits(inputDataList, "list")) {
    
    vst_mappingandtagging <- inputDataList$vst_mappingandtagging
    vst_perplotperyear <- inputDataList$vst_perplotperyear
    vst_apparentindividual <- inputDataList$vst_apparentindividual
    vst_nonWoody <- inputDataList$`vst_non-woody`
    
  } else {
    
    vst_mappingandtagging <- inputMapTag
    vst_perplotperyear <- inputPerPlot
    vst_apparentindividual <- inputIndividual
    vst_nonWoody <- NULL
    
  }
  
  
  
  ### Verify input tables contain required columns and data ####
  
  ### Verify 'vst_mappingandtagging' table contains required data
  #   Check for required columns
  mapExpCols <- c("domainID", "siteID", "plotID", "individualID", "taxonID")
  
  if (length(setdiff(mapExpCols, colnames(vst_mappingandtagging))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_mappingandtagging':", '{paste(setdiff(mapExpCols, colnames(vst_mappingandtagging)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(vst_mappingandtagging) == 0) {
    stop(glue::glue("Table 'vst_mappingandtagging' has no data."))
  }
  
  
  ### Verify 'vst_perplotperyear' table contains required data
  #   Check for required columns
  plotExpCols <- c("domainID", "siteID", "plotID", "plotType", "nlcdClass", "eventID", "totalSampledAreaTrees", "totalSampledAreaShrubSapling", "totalSampledAreaLiana", 
                   "totalSampledAreaFerns", "totalSampledAreaOther")
  
  if (length(setdiff(plotExpCols, colnames(vst_perplotperyear))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_perplotperyear':", '{paste(setdiff(plotExpCols, colnames(vst_perplotperyear)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(vst_perplotperyear) == 0) {
    stop(glue::glue("Table 'vst_perplotperyear' has no data."))
  }
  
  
  ### Verify 'vst_apparentindividual' table contains required data
  #   Check for required columns
  appIndExpCols <- c("domainID", "siteID", "plotID", "individualID", "growthForm", "plantStatus", "date", "eventID", "stemDiameter", "basalStemDiameter")
  
  if (length(setdiff(appIndExpCols, colnames(vst_apparentindividual))) > 0) {
    stop(glue::glue("Required columns missing from 'vst_apparentindividual':", '{paste(setdiff(appIndExpCols, colnames(vst_apparentindividual)), collapse = ", ")}',
                    .sep = " "))
  }
  
  #   Check for data
  if (nrow(vst_apparentindividual) == 0) {
    stop(glue::glue("Table 'vst_apparentindividual' has no data."))
  }
  
  
  ### Verify vst_nonWoody table contains required data
  #   Check for required columns
  nonwoodyExpCols <- c("domainID", "siteID", "plotID", "individualID", "growthForm", "plantStatus", "date", "stemDiameter", "basalStemDiameter", "taxonID", 
                       "height", "leafNumber", "meanLeafLength", "meanPetioleLength", "meanBladeLength")
  
  #  if (class(vst_nonWoody) == "data.frame"){if(length(setdiff(nonwoodyExpCols, colnames(vst_nonWoody))) > 0) {
  if (methods::is(vst_nonWoody, class = "data.frame" )) {
    
    if (length(setdiff(nonwoodyExpCols, colnames(vst_nonWoody))) > 0) {
    stop(glue::glue("Required columns missing from vst_nonWoody:", '{paste(setdiff(nonwoodyExpCols, colnames(vst_non-woody), collapse = ", ")}',
                    .sep = " "))
  }
  }
  
  # Error if invalid growthForm option selected
  if (!growthForm %in% c("all", "tree", "shrubLiana", "other")) {
    stop("The growthForm argument must be one of: 'all', 'tree', 'shrubLiana', or 'other'.")
  }  
  
  # Error if invalid plotType option selected
  if (!plotType %in% c("tower", "all")) {
    stop("The only valid plotType options are 'tower' or 'all'.")
  }  
  
  # Error if invalid plotPriority option selected
  if (plotPriority < 1) {
    stop("The minimum plotPriority value is 1, and plotPriority = 5 is the default that corresponds to the annual Tower subset.")
  }  
  
  
  ##  Read in plot sample area for each growthForm by eventID combo from vst_perplotperyear table
  #   Create working table from vst_perplotperyear input
  perplot <- vst_perplotperyear
  
  #   Extract year from eventID
  perplot$year <- as.numeric(substr(perplot$eventID, 10, 13))
  
  #   Define start and end dates from input year component of eventID; on rare occasions may be before the year component of earliest collect date)
  start_from_input <- as.character(min(as.numeric(substr(perplot$year, 1, 4)))) 
  end_from_input <- as.character(max(as.numeric(substr(perplot$date, 1, 4))))
  
  #   The specificModuleSamplingPriority field is used in the productivity script to optionally filter only to plots with priority 1-5 (the plots that are most likely to have been sampled the year they were scheduled)
  priority_plots <- priority_plots
  priority_plots_all = data.frame() 
  endYear <- as.numeric((as.numeric(end_from_input)) : (as.numeric(start_from_input)) )
  
  #   Identify priority plots in all years
  for (i in 1:length(endYear)) {
    
    priority_plots$year <- endYear[i]
    priority_plots_all <- rbind(priority_plots_all, priority_plots)
    
  }
  
  #   Construct an eventID needed for merging with perplot data
  priority_plots_all$eventID <- paste0("vst_", 
                                       substr(priority_plots_all$plotID, 1, 4),
                                       "_",
                                       priority_plots_all$year)
  
  #   Merge to add plot priority data to perplot data
  perplot <- merge(perplot, 
                   priority_plots_all, 
                   by = c("plotID", "plotType", "eventID", "year"),
                   all.x = TRUE)
  
  #   Create unique plotID x eventID identifier
  perplot$plot_eventID <- paste0(perplot$plotID, "_", perplot$eventID)
  
  #   Sort by date before removing duplicates so that if duplicates are from different dates the record from latest date will be retained. Sorting by date and then using fromLast = TRUE should retain the most recent version of duplicates.
  perplot <- perplot[order(perplot$date), ]
  perplot <- perplot[!duplicated(perplot$plot_eventID, fromLast = TRUE), ]
  
  #   Discard Sampling Impractical other than "OK"
  perplot_not_SI <- perplot %>% 
    dplyr::filter(.data$samplingImpractical == "OK" | .data$samplingImpractical == "" | is.na(.data$samplingImpractical)) %>%
    dplyr::filter(.data$year >= as.numeric(start_from_input) & .data$year <= as.numeric(end_from_input)) 
  
  #   Create list of ALL plot by eventID combos from the vst_perplotperyear tables from vst woody, vst non-herb perennial, or both, regardless of whether they have biomass. Generates a list of all unique combos of plotID and eventID where full sampling should have taken place
  plot_eventID_list <- unique(perplot_not_SI$plot_eventID)
  
  #   Retain subset of columns in "perplot" data
  perplot <- perplot %>% 
    dplyr::select("plot_eventID", 
                  "plotID",
                  "eventID",
                  "year",
                  "nlcdClass",
                  "plotType",
                  "eventType",
                  "dataCollected",
                  "targetTaxaPresent",
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  "totalSampledAreaLiana", 
                  "totalSampledAreaOther")
  
  #   Identify vst_perplotperyear records that are not duplicates
  plotType_df <- inputDataList$vst_perplotperyear 
  
  plotType_df <- plotType_df %>% 
    dplyr::select("plotID", 
                  "plotType")
  
  plotType_df <- plotType_df[!duplicated(plotType_df$plotID), ]
  
  
  
  ### Calculate biomass from vst_non-woody table ####
  
  ### Conditionally generaget non-woody biomass estimates from vst_nonWoody table
  if (methods::is(vst_nonWoody, class = "data.frame" )) {
    
    vst_agb_other <- vst_nonWoody
    
    #   Merge with perplot data to add total sampled areas
    vst_agb_other <- merge(vst_agb_other,
                           perplot,
                           by = c("plotID", "eventID"),
                           all.x = TRUE)
    
    vst_agb_other$agb_source <- "no source"
    
    #   Estimate yucca biomass: White, J.D., K.J. Gutzwiller, W.C. Barrow, L.J. Randall, and P. Swint. 2008. Modeling mechanisms of vegetation change due to fire in a semi-arid ecosystem. Ecological Modelling 214:181-200; divide by 1000 to convert output from "grams" to "kg"
    vst_agb_other$agb_yucca <- ifelse(vst_agb_other$growthForm == "yucca", 
                                      round(((0.0022*vst_agb_other$height) + (0.00096*(vst_agb_other$height^2)) + 0.04)/1000,
                                            digits = 6), 
                                      NA)
    
    #   Provide yucca biomass allometry reference
    vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_yucca), 
                                       "White_et_al_2008_yucca",
                                       vst_agb_other$agb_source)
    
    vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "White_et_al_2008_yucca", 
                                vst_agb_other$agb_yucca, 
                                NA)
    
    ##  Estimate ocotillo biomass: Bobich, E.G., and T.E. Huxman. 2009. Dry mass partitioning and gas exhange for young ocotillos (Fouquieria splendends) in the Sonoran Desert. International Journal of Plant Science 170:283-289. Equations: log(height_m) = 0.13 + 0.45 * log(total above and below ground biomass in kg); log(total above and below ground biomass in kg) = (log(height_m) - 0.13)/0.45 = -0.2889 +  (2.2222 * log(height_m)); log(root/shoot) = -0.63 + 0.18 * log(total above and below ground biomass in kg); aboveground biomass in kg = 1(1+exp(log(root/shoot))) * exp(log(total above and below ground biomass in kg)) = fraction aboveground * total biomass
    
    #   Estimate total ocotillo mass: aboveground + belowground
    vst_agb_other$tot_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo", 
                                         exp(-0.2889 + 2.2222 * log(vst_agb_other$height)),
                                         NA) 
    
    #   Estimate aboveground ocotillo mass
    vst_agb_other$agb_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo", 
                                         round(1/(exp(-0.63 - 0.18 * log(vst_agb_other$tot_ocotillo)) + 1) * vst_agb_other$tot_ocotillo, 
                                               digits = 6), 
                                         NA) 
    
    #   Remove total ocotillo mass: Belowground estimate not needed
    vst_agb_other$tot_ocotillo <- NULL 
    
    #   Provide ocotillo biomass allometry reference
    vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_ocotillo), 
                                       "Bobich_and_Huxman_2009", 
                                       vst_agb_other$agb_source)
    
    vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Bobich_and_Huxman_2009", 
                                vst_agb_other$agb_ocotillo, 
                                vst_agb_other$agb)
    
    #   Estimate xerophyllum tenax (bear grass) biomass: Gholz, H.L., C.C. Grier, A.G. Campbell, and A.T. Brown. 1979. Equations for estimating biomass and leaf area of plants in the pacific northwest. Research paper 41. Forest Research Laboratory, School of Forestry at Oregon State University, Corvallis. Divide by 1000 to convert output to "kg".
    vst_agb_other$agb_Gholz_XETE <- ifelse(vst_agb_other$growthForm == "xerophyllum", 
                                           round((18.873 + (0.0280*((vst_agb_other$basalStemDiameter^2) * vst_agb_other$meanLeafLength)))/1000, 
                                                 digits = 6),
                                           NA) 
    
    #   Provide xerophyllum biomass allometry reference
    vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_Gholz_XETE), 
                                       "Gholz_et_al_1979_XETE", 
                                       vst_agb_other$agb_source)
    
    vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Gholz_et_al_1979_XETE", 
                                vst_agb_other$agb_Gholz_XETE, 
                                vst_agb_other$agb)
    
    #   Estimate small palm biomass (primarily Serenoa repens): Gholz, H.L., D.N. Guerin, and W.P. Cropper. 1999. Phenology and productivity of saw palmetto (Serenoa repens) in a north Florida slash pine plantation. Canadian Journal of Forest Research 29:1248-1253. Use separate equations for rachis/petiole biomass (g) and blade/leaf biomass (g); add together and divide by 1000 to get total biomass (kg).
    vst_agb_other$agb_palm <- ifelse(!is.na(vst_agb_other$meanPetioleLength) & vst_agb_other$growthForm == "small palm",
                                     round((exp(-10.38 + 2.72 * log(vst_agb_other$meanPetioleLength)) + (-13.31 + 0.85 * vst_agb_other$meanBladeLength))/1000, 
                                           digits = 6), 
                                     NA) 
    
    #   Provide small palm biomass allometry reference
    vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_palm), 
                                       "Gholz_et_al_1999", 
                                       vst_agb_other$agb_source)
    
    vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Gholz_et_al_1999", 
                                vst_agb_other$agb_palm, 
                                vst_agb_other$agb)
    
    #   Estimate Cibotium biomass (tree fern): Ostertag, R, F Inman-Narahari, S Cordell, CP Giardina, and L Sack. 2014. Forest Structure in low-diversity tropical forests: A study of Hawaiian wet and dry forests. PLOS One. 9:e103268; Cibotium wood density (spg_gcm3) is taken as 0.22, the value for Cibotium glaucum.
    vst_agb_other$agb_Cibotium <- ifelse(substring(vst_agb_other$scientificName,1,8) == "Cibotium", 
                                         round(0.2085 * (pi * (vst_agb_other$stemDiameter/2)^2 * vst_agb_other$height * 100 * 0.22/1000),
                                               digits = 3),
                                         NA)
    
    #   Provide tree fern allometry reference
    vst_agb_other$agb_source <- ifelse(!is.na(vst_agb_other$agb_Cibotium), 
                                       "Ostertag_et_al_2014", 
                                       vst_agb_other$agb_source)
    
    vst_agb_other$agb <- ifelse(vst_agb_other$agb_source == "Ostertag_et_al_2014", 
                                vst_agb_other$agb_Cibotium, 
                                vst_agb_other$agb)
    
    #   Identify unique plot x eventIDs in vst_nonWoody data
    vst_agb_other$plot_eventID <- paste0(vst_agb_other$plotID, "_", vst_agb_other$eventID)
    agb_other_plot_eventID <- unique(vst_agb_other$plot_eventID)
    
    #   Identify unique years in vst_nonWoody data
    vst_agb_other$year <- as.numeric(substr(vst_agb_other$eventID, 10, 13))
    
    #   Retain individuals that are unambiguously 'alive' or 'dead' according to plantStatus
    vst_agb_other <- vst_agb_other %>% 
      dplyr::filter(!is.na(.data$plantStatus) & .data$plantStatus != "No longer qualifies" & .data$plantStatus != "Removed" & .data$plantStatus != "Lost, fate unknown" & .data$plantStatus != "Lost, tag damaged" & .data$plantStatus != "Lost, herbivory" & .data$plantStatus != "Lost, burned" & .data$plantStatus != "Lost, presumed dead" & .data$plantStatus != "Downed")
    
    #   Simplify plantStatus to "live", "dead_or_lost"
    vst_agb_other$plantStatus2 <- dplyr::if_else(vst_agb_other$plantStatus %in% c("Live", "Live, disease damaged", "Live, insect damaged", "Live,  other damage", "Live, physically damaged", "Live, broken bole"),
                                                 "Live", 
                                                 "Dead_or_Lost",
                                                 "Live")
    
    #   Remove biomass "NA" records to avoid misinterpreting as "0" mass in later steps
    vst_agb_other <- vst_agb_other %>% 
      dplyr::filter(!is.na(.data$agb))
    
    #   Remove unneeded columns
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
    
    
    ##  Aggregate vst non-herbaceous perennial (other) biomass data and express on a Mg/ha basis
    #   Sum multiple records associated with multi-stem individuals
    vst_agb_final_other <- vst_agb_other %>% 
      dplyr::group_by(.data$plot_eventID,
                      .data$eventID,
                      .data$siteID,
                      .data$plotID,
                      .data$taxonID,
                      .data$individualID,
                      .data$plantStatus2,
                      .data$growthForm,
                      .data$year) %>% 
      dplyr::summarise(agb = sum(.data$agb, na.rm = TRUE),
                       .groups = "drop") 
    
    
    ##  Join to associate records with total sampled areas
    vst_agb_final_other <- merge(vst_agb_final_other, 
                                 perplot, 
                                 by = c("plot_eventID", "eventID", "year", "plotID"), 
                                 all.x = TRUE) 
    
    #   Remove records that can't be scaled to area basis
    vst_agb_final_other <- vst_agb_final_other %>% 
      dplyr::filter(!is.na(.data$totalSampledAreaOther))
    
    #   Scale mass to per area basis
    vst_agb_final_other$agb_Mgha <- round(vst_agb_final_other$agb * 0.001 * (10000/vst_agb_final_other$totalSampledAreaOther), 
                                          digits = 5)
    
    #   Remove cactus and fern records for which allometries are lacking
    vst_agb_final_other <- vst_agb_final_other %>% 
      dplyr::filter(.data$growthForm != "cactus" & .data$growthForm != "fern") 
    
    #   Sum biomass per unit area for each plot x individualID x year x plantStatus2 x nlcdClass x taxonID x growthForm combo
    vst_agb_per_ha_other <- vst_agb_final_other %>% 
      dplyr::group_by(.data$plot_eventID, 
                      .data$eventID,
                      .data$siteID,
                      .data$plotID,
                      .data$plotType,
                      .data$nlcdClass,
                      .data$taxonID,
                      .data$growthForm,
                      .data$individualID,
                      .data$plantStatus2,
                      .data$year) %>% 
      dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE),
                       .groups = "drop")
    
  } #   end non-woody conditional
  
  
  
  #### Calculate biomass for vst_apparentindividual table ####
  
  ##  Read in taxonID from vst_mappingandtagging table 
  #   Create working data frame from vst_mappingandtagging table
  map <- vst_mappingandtagging 
  
  #   Retain most recent record from vst_mappingandtagging
  map <- map[order(map$date),]
  map <- map[!duplicated(map$individualID, fromLast = TRUE), ] 
  
  #   Find unique taxonIDs
  taxonID_df <- map %>% 
    dplyr::select("taxonID", 
                  "scientificName",
                  "family",
                  "genus",
                  "taxonRank") %>% 
    unique() 
  
  vst_taxonIDs <- taxonID_df$taxonID
  
  map <- map %>% 
    dplyr::select("individualID",
                  "taxonID")
  
  #   Create working data frame from vst_apparentindividual table
  appInd <- vst_apparentindividual
  
  #   Filter to individuals measured in defined timeframe
  appInd <- appInd %>% 
    dplyr::filter(as.numeric(substr(.data$date,1,4)) >= as.numeric(start_from_input) & 
                    as.numeric(substr(.data$date,1,4)) <= as.numeric(end_from_input))
  
  message("Assembling allometric equation parameters ..... ")
  
  #   Temporary fix of some eventIDs until fixes are made to portal data
  appInd$month <- as.numeric(substr(appInd$date, 6, 7))
  
  appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2019 & as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & 
                             appInd$siteID == "WREF", 
                           "vst_WREF_2019", 
                           appInd$eventID) # fixes 182 records
  
  appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & as.numeric(substr(appInd$eventID, 10, 13)) == 2016 &
                             appInd$siteID == "UKFS", 
                           "vst_UKFS_2018", 
                           appInd$eventID) # fixes 73 records
  
  appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & appInd$month >= 7 & 
                             as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "RMNP" , 
                           "vst_RMNP_2018", 
                           appInd$eventID) # fixes 398 records
  
  appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2018 & appInd$month >= 7 & 
                             as.numeric(substr(appInd$eventID, 10, 13)) == 2017 & appInd$siteID == "UNDE" , 
                           "vst_UNDE_2018", 
                           appInd$eventID) # fixes 300 records
  
  appInd$eventID <- ifelse(as.numeric(substr(appInd$date, 1, 4)) == 2017 & appInd$month == 12 & 
                             as.numeric(substr(appInd$eventID, 10, 13)) == 2018 & appInd$siteID == "GUAN" , 
                           "vst_GUAN_2017", 
                           appInd$eventID) # fixes 115 records
  appInd$month <- NULL
  
  
  ##  Merge vst_apparentindividual table with map and perplot to obtain taxonID field and area fields
  #   Add taxonID to appInd table
  appInd <- merge(appInd, 
                  map, 
                  by = "individualID",
                  all.x = TRUE) 
  
  #   Add total sampled area fields
  appInd <- merge(appInd, 
                  perplot, 
                  by = c("plotID", "eventID"),
                  all.x = TRUE)
  
  #   Create additional identifiers by eventID
  appInd$plotEvent <- paste(appInd$plotID, appInd$eventID, sep="_")
  appInd$indEvent <- paste(appInd$individualID, appInd$eventID, sep="_")
  
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
  
  #   Add tropical floristic area and/or introduced status to taxa in vst_mappingandtagging data
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
  
  
  ##  Assign Chojnacky allometric IDs
  Choj$allometry_ID <- NA
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Abies" & Choj$spg_gcm3 < 0.35, 
                              "C1", 
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Abies" & Choj$spg_gcm3 >= 0.35,
                              "C2",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Cupressaceae" & Choj$spg_gcm3 <0.30, 
                              "C3",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Cupressaceae" & 
                                Choj$spg_gcm3 >= 0.30 & Choj$spg_gcm3 < 0.40, 
                              "C4",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Cupressaceae" & Choj$spg_gcm3 >=0.40, 
                              "C5",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Larix", 
                              "C6",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Picea" & Choj$spg_gcm3 < 0.35, 
                              "C7", 
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Picea" & Choj$spg_gcm3 >= 0.35,
                              "C8",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Pinus" & Choj$spg_gcm3 < 0.45,
                              "C9",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Pinus" & Choj$spg_gcm3 >= 0.45,
                              "C10",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & 
                                (Choj$genus == "Pseudotsuga" | Choj$genus == "Taxus" | Choj$genus == "Pseudotsuga"), 
                              "C11",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Tsuga" & Choj$spg_gcm3 < 0.40, 
                              "C12",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Tsuga" & Choj$spg_gcm3 >= 0.40,
                              "C13",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$family == "Aceraceae" & Choj$spg_gcm3 < 0.50,
                              "H1",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$family == "Aceraceae" & Choj$spg_gcm3 >= 0.50,
                              "H2",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae" & Choj$spg_gcm3 < 0.40,
                              "H3", 
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae" & 
                                Choj$spg_gcm3 >= 0.40 & Choj$spg_gcm3 < 0.50,
                              "H4",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae" & 
                                Choj$spg_gcm3 >= 0.50 & Choj$spg_gcm3 < 0.60,
                              "H5",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae" & Choj$spg_gcm3 >= 0.60, 
                              "H6",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse((Choj$family == "Cornaceae" | Choj$family == "Ericaceae" | Choj$family == "Lauraceae" | 
                                 Choj$family == "Platanaceae" | Choj$family == "Rosaceae" | Choj$family == "Ulmaceae"),
                              "H7",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Carya",
                              "H8",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & (Choj$family == "Fabaceae" | Choj$family == "Juglandaceae") & 
                                Choj$genus != "Carya",
                              "H9",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Fagaceae" & Choj$decid_vs_ever == "decid",
                              "H10",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Fagaceae" & Choj$decid_vs_ever == "ever",
                              "H11",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$family == "Hamamelidaceae",
                              "H12",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse((Choj$family == "Hippocastanaceae" | Choj$family == "Tiliaceae"),
                              "H13",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$family == "Magnoliaceae",
                              "H14",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Oleaceae" & Choj$spg_gcm3 < 0.55,
                              "H15",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Oleaceae" & Choj$spg_gcm3 >= 0.55,
                              "H16",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Salicaceae" & Choj$spg_gcm3 < 0.35,
                              "H17",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Salicaceae" & Choj$spg_gcm3 >= 0.35,
                              "H18",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & Choj$family == "Cupressaceae",
                              "W1",
                              Choj$allometry_ID) 
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & (Choj$family == "Fabaceae" | Choj$family == "Rosaceae"),
                              "W2",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & Choj$family == "Fagaceae",
                              "W3",
                              Choj$allometry_ID)
  
  Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & Choj$family == "Pinaceae",
                              "W4",
                              Choj$allometry_ID)
  
  #   Arbitrarily picked C9 (forest) over C10 (forest spg_gcm3>=0.45) or W4 (woodland)
  Choj$allometry_ID <- ifelse(Choj$taxonID == "PINACE",
                              "C9",
                              Choj$allometry_ID) 
  
  #   Arbitrarily picked H9 (forest) over W2 (woodland)
  Choj$allometry_ID <- ifelse(Choj$taxonID == "FABACE",
                              "H9",
                              Choj$allometry_ID)
  
  #   Identify taxa not in Chojnacky
  Choj$source <- ifelse(!is.na(Choj$allometry_ID), 
                        "yes_ref_in_Choj",
                        "not_ref_in_Choj")
  
  #   Assign default allometryID when taxon is not in Chojnacky and cache those records in new data frame
  Choj$allometry_ID <- ifelse(Choj$source == "not_ref_in_Choj",
                              "H7",
                              Choj$allometry_ID)
  
  no_Choj_allometry <- Choj %>% 
    dplyr::filter(.data$source == "not_ref_in_Choj") %>%
    dplyr::select("allometry_ID",
                  "taxonID",
                  "family",
                  "genus",
                  "spg_gcm3", 
                  "woodland_vs_forest",
                  "decid_vs_ever",
                  "tropical",
                  "nativeStatus")
  
  #   Reduce 'Choj' to desired columns and merge to associate taxonIDs in data with needed allometric parameters
  Choj <- Choj %>% 
    dplyr::select("taxonID",
                  "scientificName",
                  "allometry_ID",
                  "source",
                  "spg_gcm3",
                  "nativeStatus",
                  "tropical",
                  "family")
  
  Choj <- merge(parameters, 
                Choj, 
                by = "allometry_ID", 
                all.y = TRUE)
  
  
  
  ### Calculate biomass using Chojnacky allometric equations
  vst_agb <- merge(appInd, 
                   Choj, 
                   by = "taxonID",
                   all.x = TRUE)
  
  message("Calculating biomass from vst_apparentindividual table ..... ")
  
  #   Note: Code below includes a variety of assumptions (including estimating dbh from basal stem diameter or height) that provide estimates that should be better than missing data values that they replace (usually). Reasoning here is that a biomass value with some positive value and percent error due to assumptions made is an improvement over a value of 0 whenever data needed for allometric equation is missing.
  
  #   Create stemDiameterFlag to flag which stemDiameters are estimates based on other fields such basalStemDiameter
  vst_agb$stemDiameterFlag <- ifelse(is.na(vst_agb$stemDiameter),
                                     "estimate",
                                     "raw")
  
  #   Manually assign 'tropical' status for a subset of taxonIDs
  vst_agb$tropical <- ifelse((vst_agb$taxonID == "2PLANT" | vst_agb$taxonID == "2PLANT-H" | vst_agb$taxonID == "ANAL12" | 
                                vst_agb$taxonID == "BOURR" | vst_agb$taxonID == "BUMI6" | vst_agb$taxonID == "CONVOL" | 
                                vst_agb$taxonID == "CROSS" | vst_agb$taxonID == "FABACE" | vst_agb$taxonID == "JACQU" | 
                                vst_agb$taxonID == "JACQU2" | vst_agb$taxonID == "COPRO" | vst_agb$taxonID == "HYDRAN") &
                               (vst_agb$siteID == "GUAN" | vst_agb$siteID == "PUUM" | vst_agb$siteID == "LAJA"),
                             "tropical",
                             vst_agb$tropical)
  
  #   Manually assign 'temperate' status for a subset of taxonIDs
  vst_agb$tropical <- ifelse((vst_agb$taxonID == "AMAR5" | vst_agb$taxonID == "CELTI" | vst_agb$taxonID == "DAWR2" | 
                                vst_agb$taxonID == "LIJA" | vst_agb$taxonID == "MEAZ" | vst_agb$taxonID == "OPUNT" | 
                                vst_agb$taxonID == "RHUS" | vst_agb$taxonID == "SAMBU" | vst_agb$taxonID == "SMSM" | 
                                vst_agb$taxonID == "SYMPL2" | vst_agb$taxonID == "VITIS") &  
                               (vst_agb$siteID != "GUAN" & vst_agb$siteID != "PUUM" & vst_agb$siteID != "LAJA"),
                             "temperate",
                             vst_agb$tropical)
  
  #   Assign specific gravity data type
  vst_agb$spg_gcm3 <- as.numeric(vst_agb$spg_gcm3)
  
  #   Assumption: For tropical species, if specific gravity is not known then assume it is 0.5 g/cm3 to permit usage of Chave et al 2014, following precedent of Asner et al 2011
  vst_agb$spg_gcm3 <- dplyr::if_else(is.na(vst_agb$spg_gcm3) & vst_agb$tropical == "tropical",
                                     0.5, 
                                     vst_agb$spg_gcm3, 
                                     vst_agb$spg_gcm3)
  
  #   Assign growthForm == "unknown" when value is NA
  vst_agb$growthForm <- dplyr::if_else(is.na(vst_agb$growthForm), 
                                       "unknown", 
                                       vst_agb$growthForm, 
                                       vst_agb$growthForm)
  
  #   Assumption: Missing stemDiameter values can be inferred from basalStemDiameter values and this is desirable since the allometric equations assume diameter at breast height rather than basal diameter
  vst_agb$stemDiameter <- dplyr::if_else(is.na(vst_agb$stemDiameter), 
                                         exp(-0.35031 + 1.03991*log(vst_agb$basalStemDiameter)), 
                                         vst_agb$stemDiameter, 
                                         vst_agb$stemDiameter)
  
  #   Assumption: If actual or estimated stemDiameter values are smaller than the minimum for their growthForm then replace with the minimum for that growthForm because record below the minimum wouldn't have been measured
  vst_agb$stemDiameter <- dplyr::if_else((vst_agb$growthForm == "sapling" | vst_agb$growthForm == "small shrub") & 
                                           vst_agb$stemDiameter < 0.1, 
                                         0.1, 
                                         vst_agb$stemDiameter, 
                                         vst_agb$stemDiameter) 
  
  #   Assumption: Assign stemDiameter == 1 for "single shrub", "small tree" and "tree" growthForms when reported DBH < 1 cm
  vst_agb$stemDiameter <- dplyr::if_else(!(vst_agb$growthForm == "sapling" | vst_agb$growthForm == "small shrub" |
                                             vst_agb$growthForm == "liana") & vst_agb$stemDiameter < 1 & is.na(vst_agb$basalStemDiameter),
                                         1,
                                         vst_agb$stemDiameter, 
                                         vst_agb$stemDiameter) 
  
  #   Assumption: If stemDiameter > basalStemDiameter, the order was likely flipped during data entry
  vst_agb$stemDiameter <- dplyr::if_else((vst_agb$stemDiameter > vst_agb$basalStemDiameter) & 
                                           (vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub"),
                                         vst_agb$basalStemDiameter,
                                         vst_agb$stemDiameter,
                                         vst_agb$stemDiameter)
  
  #   Generate list of records with missing stemDiameter and missing basalStemDiameter
  vst_agb_no_dia <- vst_agb %>% 
    dplyr::filter(is.na(.data$stemDiameter) & is.na(.data$basalStemDiameter))
  
  #   Assumption: For lianas, replace extreme outlier stemDiameter values with a low-end estimate of 1 cm
  vst_agb$stemDiameter <- dplyr::if_else(vst_agb$growthForm =="liana" & vst_agb$stemDiameter > 20, 
                                         1, 
                                         vst_agb$stemDiameter, 
                                         vst_agb$stemDiameter)
  
  #   Select columns to remove unneeded data, and simplify values of 'plantStatus2'
  vst_agb <- vst_agb %>% 
    dplyr::select("taxonID", 
                  "scientificName", 
                  "individualID", 
                  "indEvent", 
                  "eventID", 
                  "date", 
                  "siteID",
                  "plotID", 
                  "growthForm", 
                  "nlcdClass",
                  "totalSampledAreaTrees", 
                  "totalSampledAreaShrubSapling", 
                  "totalSampledAreaLiana",
                  "plantStatus", 
                  "stemDiameter", 
                  "stemDiameterFlag", 
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
                  "tropical",
                  "family")
  
  vst_agb$plantStatus2 <- ifelse(vst_agb$plantStatus %in% c("Live", "Live, disease damaged", "Live, insect damaged", "Live,  other damage", "Live, physically damaged","Live, broken bole"), 
                                 "Live",
                                 "Dead_or_Lost")
  
  
  
  ### Calculate AGB for each VST appInd record using Choj allometry_ID and Choj parameters
  
  # Assumption: Chojnacky et al 2014 allometric equations are the best first estimate of biomass
  vst_agb$agb<- round(exp(vst_agb$b0 + vst_agb$b1 * log(vst_agb$stemDiameter)), 
                      digits = 3) 
  
  #   Define AGB allometry source
  vst_agb$agb_source <- "Chojnacky_et_al_2014"
  
  #   Assign Chojnacky AGB estimates to specific column; needed to preserve Chojnacky estimates when alternate is used for tropical or introduced species.
  vst_agb$agb_Chojnacky  <- vst_agb$agb
  
  #   Assumption: When the necessary ancillary variables are available for tropical species, replace the Chojnacky et al 2014 biomass estimates with the Chave et al 2014 biomass estimates. Update tropical species records based on Chave et al 2014 if wood specific gravity is available (or an approximation based on congeners). Instructions on extracting environmental stress value E at http://chave.ups-tlse.fr/pantropical_allometry.html; Chave et al 2014 has pantropical allometric equations for tree biomass that require tree height. If tree height is not available, estimate it using their value E.
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
  
  #   Estimate shrub biomass: Case when basalStemDiameter is missing and crown diameter and height are available (biomass estimate with most uncertainty)
  suppressWarnings(vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                                 (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) &
                                                    !is.na(vst_agb$height)),
                                               round(exp(-0.370 + 1.903 * log((vst_agb$maxCrownDiameter+vst_agb$ninetyCrownDiameter)/2) +
                                                           0.652 * log(vst_agb$height)) * 1.403, 
                                                     digits = 3),
                                               NA)
                   ) # end suppressWarnings
  
  #   Assign AGB allometry source for shrubs. Citation: Conti, G., L.D. Gorne, S.R. Zeballos, M.L. Lipoma, G. Gatica, E. Kowaljow, J.I. Whitworth-Hulse, A. Cuchietti, M. Poca, S. Pestoni, and P.M. Fernandes. 2019. Developing allometric models to predict the individual aboveground biomass of shrubs worldwide. Global Ecology and Biogeography 28(7):961-975.
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub),
                               "Conti_et_al_2019",
                               vst_agb$agb_source)
  
  #   Update AGB column with shrub biomass from Conti
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019",
                                vst_agb$agb_shrub,
                                vst_agb$agb,
                                vst_agb$agb)
  
  #   Estimate shrub biomass: Improved output when basalStemDiameter is available --> less uncertainty
  suppressWarnings(vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                                 !is.na(vst_agb$basalStemDiameter),
                                               round(exp(-2.869 + 2.584 * log(vst_agb$basalStemDiameter)), 
                                                     digits = 3),
                                               vst_agb$agb_shrub)
                   ) # end suppressWarnings
  
  #   Update AGB allometry for shrubs that now have an "agb_shrub" value
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub),
                               "Conti_et_al_2019",
                               vst_agb$agb_source)
  
  #   Update AGB column with Conti estimates based on basalStemDiameter
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019",
                                vst_agb$agb_shrub,
                                vst_agb$agb, 
                                vst_agb$agb)
  
  #   Estimate shrub biomass: Even better output when basalStemDiameter AND crownDiameter available (compared to basalStemDiameter alone)
  suppressWarnings(vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                                 (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) &
                                                    !is.na(vst_agb$basalStemDiameter)),
                                               round(exp(-2.057 + 1.741 * log(vst_agb$basalStemDiameter) + 0.945 * 
                                                           log((vst_agb$maxCrownDiameter + vst_agb$ninetyCrownDiameter)/2)), 
                                                     digits = 3),
                                               vst_agb$agb_shrub)
                     ) # end suppressWarnings
  
  #   Update AGB allometry for shrubs that now have an "agb_shrub" value
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub),
                               "Conti_et_al_2019",
                               vst_agb$agb_source)
  
  #   Update AGB column with Conti estimates based on basalStemDiameter and crownDiameter
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019", 
                                vst_agb$agb_shrub, 
                                vst_agb$agb,
                                vst_agb$agb)
  
  #   Estimate shrub biomass: Best output when basalStemDiameter, crownDiameter, AND height are all available
  suppressWarnings(vst_agb$agb_shrub <- ifelse((vst_agb$growthForm == "single shrub" | vst_agb$growthForm == "small shrub") &
                                                 (!is.na(vst_agb$maxCrownDiameter) & !is.na(vst_agb$ninetyCrownDiameter) &
                                                    !is.na(vst_agb$basalStemDiameter) & !is.na(vst_agb$height)),
                                               round(exp(-2.281 + 1.525 * log(vst_agb$basalStemDiameter) + 0.831 * 
                                                           log((vst_agb$maxCrownDiameter + vst_agb$ninetyCrownDiameter)/2) + 0.523 * 
                                                           log(vst_agb$height)),
                                                     digits = 3),
                                               vst_agb$agb_shrub)
                   ) # end suppressWarnings
  
  #   Update AGB allometry for shrubs that now have an "agb_shrub" value
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_shrub), 
                               "Conti_et_al_2019", 
                               vst_agb$agb_source)
  
  #   Update AGB column with Conti estimates based on basalStemDiameter, crownDiameter, and height
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Conti_et_al_2019", 
                                vst_agb$agb_shrub, 
                                vst_agb$agb, 
                                vst_agb$agb)
  
  
  
  ### Assumption: Where available, species-specific allometric equations are preferable to more generic ones; update AGB estimates for taxa for which species-specific allometric equations exist
  
  ##  Species: Metrosideros polymorpha (MEPO5) - first estimate AGB for all MEPO5 with a stemDiameter (Litton and Kauffman 2008), then in subsequent steps update the AGB estimate with Selmants et al 2014; approach retains Litton and Kauffman 2008 estimate for those MEPO5 with DBH >= 33 cm and that do not have 'height' recorded. All other individuals have an AGB estimate via Selmants et al 2014.
  
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
  
  
  ##  Species: Rhamnus davurica (RHDA); citation: Zhang et al 2012. Sexual dimorphism in reproductive and vegetative allometry for two dioecious Rhamnus plants in north-eastern China. Eur J Forest Res (2012) 131:1287-1296. The taxonID RHDA is the most frequent introduced species in NEON VST dataset, and Zhang et al 2012 have a specific equation for RHDA. There is one equation for males and another for females; here, we take the average because NEON does not record sex of RHDA. Output is divided by 1000 to convert to "kg".
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
  suppressWarnings(vst_agb$agb_ARTR2 <- ifelse(vst_agb$taxonID == "ARTR2", 
                                               round(exp(7.889 + 0.8539 * log(4/3 * pi * (vst_agb$maxCrownDiameter/2) *
                                                                                (vst_agb$ninetyCrownDiameter/2)))/1000, 
                                                     digits = 3),
                                               NA)
                   ) # end suppressWarnings
  
  #   Update AGB allometry for ARTR2 individuals
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_ARTR2),
                               "Cleary_et_al_2008_ARTR2",
                               vst_agb$agb_source)
  
  #   Update AGB column with Cleary et al 2008 estimates
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Cleary_et_al_2008_ARTR2",
                                vst_agb$agb_ARTR2,
                                vst_agb$agb,
                                vst_agb$agb)
  
   
  ##  Species: Larrea tridentata (LATR2); citation: Huenneke, L.F., D. Clason, and E. Muldavin. 2001. Spatial heterogeneity in Chihuahan Desert vegetation: implications for sampling methods in semi-arid ecosystems. Journal of Arid Environments 47:257-270. Diameters multiplied by 100 to convert from "m" to "cm"; final result divided 1000 to convert output from "g" to "kg". Equation estimated from regression line in Figure 3a.
  vst_agb$agb_LATR2 <- ifelse(vst_agb$taxonID == "LATR2", 
                              round((24.76 + 0.0014 * (4/3 * pi * (vst_agb$maxCrownDiameter * 100) * 
                                                         (vst_agb$ninetyCrownDiameter * 100)))/1000, 
                                    digits = 3),
                              NA)
  
  #   Update AGB allometry for LATR2 individuals
  vst_agb$agb_source <- ifelse(!is.na(vst_agb$agb_LATR2), 
                               "Huenneke_et_al_2001_LATR2",
                               vst_agb$agb_source)
  
  #   Update AGB column with Huenneke et al 2001 estimates
  vst_agb$agb <- dplyr::if_else(vst_agb$agb_source == "Huenneke_et_al_2001_LATR2",
                                vst_agb$agb_LATR2,
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
  
  
  ##  Assumption: Allometric equations developed specifically for lianas are better than generic allometric equations used above for trees and shrubs. Citation: Schnitzer, SA, SJ DeWalt, and J Chave. 2006. Censusing and measuring lianas: A quantitative comparison of the common methods. Biotropica 38:581-591. Update AGB for lianas with equations from Schnitzer_et_al_2006 (Chojnacky is not intended for lianas, or for introduced or tropical species, and there are numerous introduced and tropical liana species, see below). Equation for tropical lianas is used for temperate liana species. 
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
  
  #   Assume growthForm == "small tree" if stemDiameter < 10 cm
  vst_agb$growthForm <- dplyr::if_else(vst_agb$growthForm == "unknown" & vst_agb$stemDiameter < 10,
                                       "small tree",
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
                    .data$plantStatus != "Lost, presumed dead" & .data$plantStatus != "Downed") 
  
  
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
  
  #   Remove records that cannot be scaled to a per area basis
  vst_agb_final <- vst_agb_final %>% 
    dplyr::filter(!is.na(.data$sampledAreaM2))
  
  #   Create "Mg/ha" biomass estimate for each record; used in downstream plot- and site-level biomass estimation
  vst_agb_final$agb_Mgha <- round(vst_agb_final$agb * 0.001 * (10000/vst_agb_final$sampledAreaM2), 
                                  digits = 6)
  
  
  ##  Filter output based on user-supplied 'growthForm' argument
  if(growthForm == "shrubLiana") {
    
    vst_agb_final <- vst_agb_final %>% 
      dplyr::filter(.data$growthForm == "single shrub" | .data$growthForm == "small shrub" | .data$growthForm == "liana")
    
  }
  
  if(growthForm == "tree") {
    
    vst_agb_final <- vst_agb_final %>% 
      dplyr::filter(.data$growthForm == "single bole tree" | .data$growthForm == "multi-bole tree"  | 
                      .data$growthForm == "sapling" | .data$growthForm == "small tree") 
    
    }
  
  
  ##  Summarize estimated biomass per unit area for unique combinations of individualID x plantStatus x year
  vst_agb_per_ha_appInd <- vst_agb_final %>% 
    dplyr::group_by(.data$plot_eventID, 
                    .data$eventID, 
                    .data$siteID, 
                    .data$plotID, 
                    .data$plotType, 
                    .data$nlcdClass, 
                    .data$taxonID, 
                    .data$growthForm, 
                    .data$individualID, 
                    .data$plantStatus2, 
                    .data$year) %>% 
    dplyr::summarise(agb_Mgha = sum(.data$agb_Mgha, na.rm = TRUE),
                     .groups = "drop")
  
  
  
  ### Combine AGB for vst_apparentindividual (vst_agb_per_ha_appInd) and vst_nonWoody (vst_agb_per_ha_other)
  
  if (methods::is(vst_nonWoody, class = "data.frame" )) { 
    vst_agb_per_ha <- rbind(vst_agb_per_ha_appInd, vst_agb_per_ha_other)
  } else {
    vst_agb_per_ha <- vst_agb_per_ha_appInd
  }
  
  #   Filter combined output by user-supplied 'growthForm' argument
  if (growthForm == "other") {
    
    vst_agb_per_ha <- vst_agb_per_ha %>% 
      dplyr::filter(.data$growthForm == "tree fern" | .data$growthForm == "small tree fern" | .data$growthForm == "large tree fern"  |
                      .data$growthForm == "palm" | .data$growthForm == "small palm" | .data$growthForm == "palm tree" | 
                      .data$growthForm == "ocotillo" | .data$growthForm == "xerophyllum"  | .data$growthForm == "yucca")
    
  }
  
  if (growthForm == "shrubLiana") {
    
    vst_agb_per_ha <- vst_agb_per_ha %>% 
      dplyr::filter(.data$growthForm == "single shrub" | .data$growthForm == "small shrub" | .data$growthForm == "liana") 
    
  }
  
  if (growthForm == "tree") {
    
    vst_agb_per_ha <- vst_agb_per_ha %>% 
      dplyr::filter(.data$growthForm == "single bole tree" | .data$growthForm == "multi-bole tree"  | .data$growthForm == "sapling" |
                      .data$growthForm == "small tree")
    
  }
  
  
  ##  Create list of plot x eventIDs from vst_apparentindividaul data; later diff against what is reported in perplot data
  agb_ind_eventID_list <- unique(vst_agb_per_ha$plot_eventID)
  
  
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
  
  #   Sum biomass per unit area for each plot x year x plantStatus2 x nlcdClass combo (aggegate across individualIDs)
  vst_plot_summary <- vst_agb_per_ha %>% 
    dplyr::group_by(.data$plot_eventID, 
                    .data$eventID, 
                    .data$siteID, 
                    .data$plotID, 
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
                                      id_cols = c("plot_eventID", "eventID", "siteID", "plotID",
                                                  "plotType", "nlcdClass", "taxonID", "growthForm", "year"),
                                      names_from = "plantStatus2", 
                                      names_prefix = "agb_Mgha__", 
                                      values_from = "agb_Mgha")
  
  #   Assumption: Replace NAs created during transpose with zeroes; assume both live and dead were sampled in a plot
  vst_plot_wide[is.na(vst_plot_wide)] <- 0
  
  
  #   Assign zero AGB values to plots with zero biomass
  vst_agb_zeros_plot <- vst_agb_zeros
  
  if (nrow(vst_agb_zeros_plot) > 0) {
    vst_agb_zeros_plot$agb_Mgha__Dead_or_Lost <- vst_agb_zeros_plot$agb_Mgha__Live <- 0 
  }
  
  #   Add rows for plots with zero biomass to plots with AGB
  vst_plot_w_0s <- rbind(vst_plot_wide, 
                         vst_agb_zeros_plot)
  
  #   Add 'specificModuleSamplingPriority' column to output
  priority_plots_without_plotType <- priority_plots_all %>% 
    dplyr::select(-"plotType")
  
  vst_plot_w_0s <- merge(vst_plot_w_0s, 
                         priority_plots_without_plotType, 
                         by = c("plotID", "eventID", "year"), 
                         all.x = TRUE)
  
  #   Retain AGB estimates for records with values for both live and dead biomass
  vst_plot_w_0s <- vst_plot_w_0s %>% 
    dplyr::filter(!is.na(.data$agb_Mgha__Live) & !is.na(.data$agb_Mgha__Dead_or_Lost))
  
  #   Retain records within data-prescribed 'year' range
  vst_plot_w_0s <- vst_plot_w_0s %>% 
    dplyr::filter(.data$year >= start_from_input)
  
  
  #   Remove records based on user-supplied 'plotType' and 'plotPriority' arguments
  if (plotType == "tower") {
    
    vst_plot_w_0s <- vst_plot_w_0s %>% 
      dplyr::filter(.data$plotType == "tower") 
    
  }
  
  if (!is.na(plotPriority)) {
    
    vst_plot_w_0s <- vst_plot_w_0s %>% 
      dplyr::filter(.data$specificModuleSamplingPriority <= plotPriority)
    
  }
  
  
  
  ### Generate site-level biomass summary ####
  
  #   First sum AGB at the plot level - i.e., remove intra-plot granularity from growthForm, taxonID, etc.
  vst_plot_w_0s_sum_taxa <- vst_plot_w_0s
  
  if(nrow(vst_plot_w_0s_sum_taxa) > 0) {
    
    vst_plot_w_0s_sum_taxa <- vst_plot_w_0s %>% 
      dplyr::group_by(.data$plot_eventID, 
                      .data$eventID, 
                      .data$siteID, 
                      .data$plotID, 
                      .data$plotType, 
                      .data$nlcdClass, 
                      .data$year) %>% 
      dplyr::summarise(agb_Mgha__Live = sum(.data$agb_Mgha__Live, na.rm = TRUE),
                       agb_Mgha__Dead_or_Lost = sum(.data$agb_Mgha__Dead_or_Lost, na.rm = TRUE),
                       .groups = "drop")
    
  } else {
    
    vst_plot_w_0s_sum_taxa$agb_Mgha__Live = 0
    vst_plot_w_0s_sum_taxa$agb_Mgha__Dead_or_Lost = 0
    
  }
  
  #   Create site-level summary table: mean, sd, n()
  vst_site <- vst_plot_w_0s_sum_taxa %>% 
    dplyr::group_by(.data$siteID, 
                    .data$year) %>% 
    dplyr::summarise(woodPlotNum = dplyr::n(), 
                     woodLiveMassMean_Mgha = round(mean(.data$agb_Mgha__Live, na.rm = TRUE), 
                                                   digits = 3), 
                     woodLiveMassSD_Mgha = round(stats::sd(.data$agb_Mgha__Live, na.rm = TRUE), 
                                                 digits = 3),
                     .groups = "drop")
  
  
  ##  Clean-up: Remove unneeded columns and add back complete plotType and domainID fields
  #   Remove 'year' column from 'priority_plots' data frame
  priority_plots$year <- NULL
  
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
  
  output.list <- list(vst_agb_per_ha = vst_agb_per_ha,
                      vst_plot_w_0s = vst_plot_w_0s,
                      vst_agb_zeros = vst_agb_zeros,
                      vst_site = vst_site)
  
  return(output.list)
}
