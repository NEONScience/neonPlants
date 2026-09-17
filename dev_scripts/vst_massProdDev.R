### Scratchpad for estimateWoodMass and estimateWoodProd dev ####



### Investigate why perPlot input has one more row than wood_ANPP_plot
temp1 <- perPlot %>%
  dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
  dplyr::select("plotEvent") %>%
  dplyr::distinct()

temp2 <- wood_ANPP_plot %>%
  dplyr::mutate(plotEvent = paste(plotID, eventID, sep = "-")) %>%
  dplyr::select("plotEvent") %>%
  dplyr::distinct()

setdiff(temp1$plotEvent, temp2$plotEvent)
#--> ABBY_076-vst_ABBY_2019 missing from function output; has dataCollected = NA, is this a problem?
#--> Seems that this plot x event legitimately has no AI records or NW records --> likely a PPPY error.






### DEV: Problem - "tree" duplicates based on plotID, eventID, individualID cause warnings in calculateTransitions function
#-->  For some that are 'mbt', addition of 'A' will resolve (and one bole is 'live' and other is 'dead')
#-->  For others, dupes are not resolvable, should be filtered out



### DEV: Capture live/dead transitions with row-based approach ####

### Get wood mass estimates
##  Get data frames needed for estimateWoodMass outputs
# map <- vstTestDF$vst_mappingandtagging %>%
#   dplyr::filter(plotID %in% c("ABBY_065", "ABBY_070"))
#
# appInd <- vstTestDF$vst_apparentindividual %>%
#   dplyr::filter(plotID %in% c("ABBY_065", "ABBY_070"))
#
# perPlotMassInput <- vstTestDF$vst_perplotperyear %>%
#   dplyr::filter(plotID %in% c("ABBY_065", "ABBY_070"))


map <- vegkonz$vst_mappingandtagging
appInd <- vegkonz$vst_apparentindividual
perPlotMassInput <- vegkonz$vst_perplotperyear

##  Estimate wood mass of individuals
woodMass <- neonPlants::estimateWoodMass(inputIndividual = appInd,
                                         inputMapTag = map,
                                         inputPerPlot = perPlotMassInput,
                                         growthFormSubset = "tree")

#   Simplify wood mass outputs for dev
agbDF <- woodMass$vst_agb_kg %>%
  dplyr::arrange(plotID,
                 individualID,
                 eventID) %>%
  dplyr::select(plotID, eventID, date, taxonID, individualID, liveDeadStatus, growthForm, agb_kg)


##  Create "complete" perPlot DF: Contains a row for all combinations of plotID x eventID
perPlotTransitionDF <- vstTestDF$vst_perplotperyear %>%
  dplyr::filter(plotID %in% c("ABBY_065", "ABBY_070")) %>%
  dplyr::mutate(treesPresent = dplyr::case_when(treesPresent == "Present - sampled" ~ "Y",
                                                TRUE ~ treesPresent)) %>%
  dplyr::select(domainID, siteID, plotID, eventID, plotType, eventType, dataCollected, targetTaxaPresent, treesPresent, totalSampledAreaTrees) %>%
  tidyr::complete(domainID, siteID, plotID, eventID)


##  Use full_join to associate plot data with individual data
#--> Reveals plots-events with no individuals and individuals with no plot-events
transitionDF <- dplyr::full_join(perPlotTransitionDF,
                                 agbDF %>%
                                   dplyr::select("plotID", "eventID", "individualID"),
                                 by = c("plotID", "eventID"))

#   Identify unique plot-event combinations
plotEventDF <- transitionDF %>%
  distinct(plotID, eventID)

#   Identify unique plot-individual combinations
plotIndivDF <- transitionDF %>%
  dplyr::filter(!is.na(individualID)) %>%
  dplyr::distinct(plotID, individualID)

#   For each individual in a plot, create a row for each plot-event combination
plotEventIndivDF <- plotEventDF %>%
  dplyr::left_join(plotIndivDF,
                   by = "plotID",
                   relationship = "many-to-many")


##  Fill in original plot-level data where they exist, otherwise they are NA
transFilledDF <- plotEventIndivDF %>%
  dplyr::left_join(transitionDF %>%
                     dplyr::select(-"individualID") %>%
                     dplyr::distinct(plotID, eventID, .keep_all = TRUE),
                   by = c("plotID", "eventID"),
                   relationship = "many-to-many")


##  Fill in original individual-level data where they exist, otherwise they are NA
transFilledDF <- transFilledDF %>%
  dplyr::left_join(agbDF,
                   by = c("plotID", "eventID", "individualID"))


##  Add 'year' column back, derived from eventID
transFilledDF <- transFilledDF %>%
  dplyr::mutate(year = as.numeric(stringr::str_extract(eventID, "20[0-9]{2}$")),
                .before = "eventID")



### Determine 'growthInterval' and 'transitionStatus' per individual per eventID pair with sufficient data

##  Retain rows from last time plot or individual was sampled to determine 'transitionStatus' and 'growthInterval'
transFilterDF <- transFilledDF %>%
  dplyr::filter(!is.na(date) | (!is.na(totalSampledAreaTrees) & !is.na(treesPresent))) %>%
  dplyr::relocate("eventID",
                  "individualID",
                  .before = "date")


##  Infer liveDeadStatus for missed eventIDs
#--> An individual can be inferred to be "live"/"dead" at a missed timepoint based on time-series; helps avoid false "recruitment"
transFilterDF <- transFilterDF %>%
  dplyr::group_by(individualID) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::group_modify(~{
    df <- .x
    #   Original liveDeadStatus data as 's0'
    s0 <- df$liveDeadStatus
    #   Working copy of liveDeadStatus as 's'
    s  <- s0
    #   liveDeadStatus rows that are not NA
    nz <- which(!is.na(s))

    if (length(nz) >= 2) {
      for (k in seq_len(length(nz) - 1)) {
        i <- nz[k]
        j <- nz[k + 1]

        if (!is.na(s[i]) && s[i] %in% c("live", "dead") &&
            identical(s[i], s[j]) &&
            all(is.na(s[(i + 1):(j - 1)]))) {

          s[(i + 1):(j - 1)] <- s[i]
        }
      }
    }

    df$liveDeadStatus <- s
    df$statusFlag <- is.na(s0) & !is.na(s)   # only originally-NA rows that got filled
    df
  }) %>%
  dplyr::relocate("individualID",
                  .before = "eventID") %>%
  dplyr::relocate("statusFlag",
                  .after = "liveDeadStatus")


##  Determine 'transitionStatus' relative to last time the plot or individual was sampled
transFilterDF <- transFilterDF %>%
  dplyr::group_by(individualID) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(
    transitionStatus = dplyr::case_when(
      liveDeadStatus == dplyr::lag(liveDeadStatus) ~ "noChange",
      liveDeadStatus == "dead" & dplyr::lag(liveDeadStatus) == "live" ~ "mortality",
      liveDeadStatus == "live" & is.na(dplyr::lag(liveDeadStatus)) & dplyr::row_number() != 1 ~ "recruitment",
      (is.na(liveDeadStatus) & targetTaxaPresent == "N") &
        (dplyr::lag(liveDeadStatus) == "live" & dplyr::lag(targetTaxaPresent) == "Y" & dplyr::lag(treesPresent) == "Y") ~ "mortality",
      (is.na(liveDeadStatus) & targetTaxaPresent == "Y" & treesPresent == "N") &
        (dplyr::lag(liveDeadStatus) == "live" & dplyr::lag(targetTaxaPresent) == "Y" & dplyr::lag(treesPresent) == "Y") ~ "mortality",
      #   Missing only assigned when 'liveDeadStatus' is NA, cannot be inferred, and the plot was sampled
      (is.na(liveDeadStatus) & targetTaxaPresent == "Y" & treesPresent == "Y") & dplyr::lag(liveDeadStatus) == "live" ~ "missing",
      TRUE ~ NA_character_
    ),
    .after = "liveDeadStatus"
  )


##  Approximate missing mass values
#--> Calculated 'growthInterval' data will be based off both inferred and allometrically estimated mass values
transFilterDF <- transFilterDF %>%
  dplyr::group_by(individualID) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(
    estimatedMass = {
      x <- year
      y <- agb_kg
      ok <- !is.na(y)

      if (sum(ok) < 2) {
        #   Return NA if not enough points to interpolate
        rep(NA_real_, length(y))

      } else {
        #   Rule 1 does not extrapolate to NAs outside the data
        round(stats::approx(x = x[ok], y = y[ok], xout = x, rule = 1)$y,
              digits = 2)
      }
    }
  )

#   Conditionally populate 'agb_kg' with missing mass values; create 'massFlag' to record when
transFilterDF <- transFilterDF %>%
  dplyr::mutate(
    massFlag = dplyr::case_when(
      is.na(agb_kg) & !is.na(estimatedMass) ~ TRUE,
      TRUE ~ FALSE
    ),
    agb_kg = dplyr::replace_when(
      agb_kg,
      is.na(agb_kg) & !is.na(estimatedMass) ~ estimatedMass
    )
    ) %>%
  dplyr::relocate("massFlag",
                  .after = "agb_kg") %>%
  dplyr::select(-"estimatedMass")


##  Calculate 'growthInterval' relative to the last time the plot or individual was sampled
#--> Use 'date' from AI to calculate interval if available, otherwise use 'year'
transFilterDF <- transFilterDF %>%
  dplyr::group_by(individualID) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(
    growthInterval = dplyr::case_when(
      #   When available, use 'date' from AI table to calculate 'growthInterval'
      !is.na(date) & !is.na(dplyr::lag(date)) ~ as.numeric(difftime(date, lag(date), units = "days")) / 365.25,
      #   Otherwise use 'year' from PPPY table to calculate 'growthInterval' if mass data are present
      (is.na(date) | is.na(dplyr::lag(date))) & !is.na(agb_kg) & !is.na(dplyr::lag(agb_kg)) ~ year - dplyr::lag(year),
      #   Return NA when biomass data unavailable and 'year' based interval not relevant
      TRUE ~ NA_real_
    ),

    #   Reduce 'growthInterval' to a single digit
    growthInterval = round(growthInterval, digits = 1)
  )



### CL estimateIncrement unused code ######################

# iterating over every record is very inefficient, but I haven't figured out a better way
# since each measurement has to be compared against a prior measurement of varying distance
for(i in unique(biomassTable$individualID)) {

  biomassi <- biomassTable[which(biomassTable$individualID==i),]
  biomassi <- biomassi[order(biomassi$eventYear),]
  # if an individual is dead (or NA) at every time step, ignore
  if(identical(unique(biomassi$liveDeadStatus), "dead") |
     all(unique(biomassi$liveDeadStatus) %in% c(NA, "dead"))) {
    outList[[i]] <- biomassi
    next
  }

  # ordered by year, so can skip first year
  for(j in 2:nrow(biomassi)) {

    # recruitment
    if(identical(biomassi$transitionType[j], "recruitment")) {
      # if biomass estimate is unavailable, NA
      if(is.na(biomassi$agb_kg[j])) {
        biomassi$biomassChange[j] <- NA
      } else {
        # # otherwise calculate biomass at 10cm diameter
        # # essentially making a fake data record for the relevant tree
        # # this doesn't work yet
        # indinit <- apparentindividuals[which(apparentindividuals$individualID==i &
        #                                        apparentindividuals$eventID==paste("vst", apparentindividuals$siteID[1], biomassi$eventYear[j], sep="_")),]
        # indinit$stemDiameter <- 10
        # indinit$taxonID <- biomassi$taxonID[1]
        # indinit$scientificName <- biomassi$scientificName[1]
        # indinit$genus <- biomassi$genus[1]
        # indinit$family <- biomassi$family[1]
        # ind10 <- estimateAllometricWoodyMass(indinit)

        # PLACEHOLDER until we have mass code: assume 20kg starting mass
        biomassi$biomassChange[j] <- biomassi$agb_kg[j] - 20
        # flag for suspiciously large new trees
        growthPerYear <- try(I((as.numeric(biomassi$stemDiameter[j])-10)/biomassi$growthInterval[j]),
                             silent=TRUE)
        if(inherits(growthPerYear, "try-error") |
           is.na(growthPerYear)) {
          biomassi$stemIncrementFlag[j] <- -1
        } else {
          if(growthPerYear>3) {
            biomassi$stemIncrementFlag[j] <- 1
          }
        }
      }
    }

    # mortality
    if(identical(biomassi$transitionType[j], "mortality")) {
      # filter out missing trees if specified
      if(identical(biomassi$t2Missing[j], "missing") &
         identical(mortalityMissing, "filterMissing")) {
        missing <- rbind(missing, biomassi[j,])
        biomassi$biomassChange[j] <- NA
      } else {
        # PLACEHOLDER: assume biomass lost = biomass at last measurement
        # this is ok for 1 year growth intervals, longer intervals need
        # estimation of within interval growth

        # check for missing value for biomass at last measurement
        lastbiomass <- try(biomassi$agb_kg[I(j - biomassi$growthInterval[j])], silent=TRUE)
        if(inherits(lastbiomass, "try-error") |
           is.na(lastbiomass)) {
          biomassi$biomassChange[j] <- NA
        } else {
          biomassi$biomassChange[j] <- -lastbiomass
        }
      }
    }

    # live increment
    if(identical(biomassi$liveDeadStatus[j], "live") &
       !identical(biomassi$transitionType[j], "recruitment")) {
      # check for valid measurement at this time step and previous measurement
      if(is.na(biomassi$agb_kg[j])) {
        biomassi$biomassChange[j] <- NA
      } else {
        lastbiomass <- try(biomassi$agb_kg[I(j - biomassi$growthInterval[j])], silent=TRUE)
        if(inherits(lastbiomass, "try-error") |
           is.na(lastbiomass)) {
          biomassi$biomassChange[j] <- NA
        } else {
          biomassi$biomassChange[j] <- biomassi$agb_kg[j] - lastbiomass
        }
      }
    }
  }

  outList[[i]] <- biomassi

}





### VST test dataset summary ####
#--> Not clear which sites SS ultimately included in test dataset and what types of bouts and plots

perPlot <- VstDat$vst_perplotperyear

summaryPlot <- perPlot %>%
  dplyr::group_by(samplingImpractical,
                  eventType,
                  eventID,
                  targetTaxaPresent,
                  dataCollected) %>%
  dplyr::summarise(count = n())

##  Observations:
#   Dataset was constructed with an eye toward getting all the taxa required to test the various taxon- and functional-group-specific allometric equations --> i.e., sites with lianas, xerophyllum, artemisia, Rhamnus, Cibotium, ocotillo, cactus, etc.
#   Plot count for each test case is very low.
#   Low replicate level within most site-eventID combinations means site-level summary metrics will not be typical.
#   A number of sites have data from only one eventID --> productivity estimates are not possible for these sites.
#   All records are targetTaxaPresent == "Y"
#   Data do not represent all possible transitions for eventType values --> i.e., allTowerPlots to towerSubset, allTowerPlots to distributedAndTowerSubset, distributedAndTowerSubset to towerSubset, etc. May not need all transitions in the test dataset, but function logic should accommodate all cases.
#   Does not currently have records with dataCollected == "partial" (would be useful for productivity development since only tree data will be present for this type of plot); also, the "dendrometerOnly" are all singletons and not the full complement (not a problem).
#   Current test dataset does have all cases of samplingImpractical --> i.e., one record with something not "OK" ("logistical"), one record with NA
#   Path forward: Since it is now clear that CRAN submission is not the immediate priority in terms of the AWP milestone, that package size concerns are not immediate, and that I think the current test dataset is insufficient for development and testing of numerous aspects of the productivity code, next step is to create a robust test dataset for both biomass estimations


##  Test dataset requirements:
#   Sites with taxa that allow testing of all allometries present in estimateWoodMass:
    #--> ocotillo
    #--> xerophyllum
    #--> small palm (S. repens)
    #--> large and small tree fern (Cibotium)
    #--> PNW forest, Eastern hardwood forest; want speciose forests to hit as many allometries as possible
    #--> Tropical site (one of GUAN, LAJA, PUUM), to enable testing Chave allometries
    #--> Shrub-dominated site to test Conti allometry output.
    #--> MEPO5 individuals with DBH < 33 cm and DBH > 33 cm
    #--> Rhamnus davurica (RHDA) individuals
    #--> Cornus individuals
    #--> Liana growthform
    #-->
#   For estimateWoodMass:
    #--> Site-level output: Want all possible eventTypes to ensure 'plotSubset' filtering works correctly.
    #--> Want eventIDs with dataCollected = "allTowerPlots" and "dendrometerOnly" to ensure filtering works correctly.
    #--> Want eventID with targetTaxaPresent == "N" for at least one plotID.
    #--> Want eventID with samplingImpractical != "OK" and is.na(samplingImpractical) == TRUE for at least one plotID.
    #--> Want events with dendrometer bands (future dev).

#   For estimateWoodProd:
    #--> Two eventIDs per site with full complement of plots per eventID.
    #--> Transitions between events should include permutations of eventType - e.g., distributedAndTowerSubset -> towerSubset; towerSubset -> allTowerPlots, etc.
    #--> Include some plotIDs with dataCollected == "dendrometerOnly"
    #--> Include some plotIDs with dataCollected == "partial"
    #--> Include PUUM to ensure large tree ferns ('ltf' growthForm) are included in estimate
    #--> Plot that transitions from treesPresent == "N" to "Y" (plot-level recruitment)
    #--> Plot that transitions from targetTaxaPresent == "Y" to "N" (plot-level mortality)
    #--> New paradigm: > 2 input years --> Need 3 eventIDs for one site, with individuals missing from either t1 or t2 but present in either t2 or t3, respectively.
    #--> Plot that shows zero "tree" productivity: tTP = N or treesPresent = N in two consecutive timepoints --> need to ensure plots like this are properly assigned a zero and not filtered out.



### VST eventIDs to include in test dataset:
#-->  vst_BLAN_2021 (allTowerPlots), vst_BLAN_2022 (towerSubset)
        # RHDA, whole plot recruitment for BLAN_032 (one individual)
        # BLAN_032 from 2020 to 2021 also has plots with treesPresent = "N" in two consecutive eventIDs
#-->  vst_SOAP_2023 (allTowerPlots), vst_SOAP_2024 (distributedAndTowerSubset)
        # Whole plot mortality (plots with tTP = "N" in 2024), one samplingImpractical == "logistical" in 2023
#-->  vst_WREF_2024 (towerSubset), vst_WREF_2025 (allTowerPlots)
        # Xerophyllum, dendrobands, dataCollected = "dendrometerOnly"
#-->  vst_YELL_2020 (towerSubset), vst_YELL_2021 (allTowerPlots)
        # ARTR2 (shrub via Conti), dendrobands, dataCollected = "partial"
#-->  vst_SCBI_2023 (distributedAndTowerSubset), vst_SCBI_2024 (towerSubset)
        # Cornus, lianas, shrubs, trees
#-->  vst_PUUM_2022 (towerSubset), vst_PUUM_2023 (allTowerPlots)
        # MEPO5, Cibotium, 7 SI = "logistical" in 2023
#-->  vst_DSNY_2017 (distributedOnly), vst_DSNY_2022 (distributedAndTowerSubset)
        # Small palms, 5-year interval, 8 SI = "logistical" in 2022.
        # Includes 4 plots with treesPresent = "N" in two consecutive eventIDs (zero "tree" productivity)
#-->  vst_SRER_2016 (allTowerPlots), vst_SRER_2021 (allTowerPlots)
        # Ocotillo, many with TEMP-type individualIDs
#-->  vst_ABBY_2018 (allTowerPlots), vst_ABBY_2019 (distAndTowerSubset), vst_ABBY_2020 (towerSubset)
        # Tree individual in ABBY_070 that was sampled, missed, sampled again; is.na(samplingImpractical) -> TRUE
        # Add all years through from 2018 to 2024 to enable development of row-based transition code.
        # Adding 2016 and 2017 includes plots with treesPresent = "N" in two consecutive eventIDs
#-->  vst_KONZ_2019 (allTowerPlots) inclusive to vst_KONZ_2024 (allTowerPlots)
        # Many missed individuals and plots.



### Prepare test dataset  ####

##  Mapping and tagging: Retrieve all records for sites associated with events above
temp <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                     site = c("BLAN", "SCBI", "DSNY", "KONZ", "YELL", "SRER", "ABBY", "WREF", "SOAP", "PUUM"),
                                     tabl = "vst_mappingandtagging",
                                     release = "LATEST",
                                     include.provisional = TRUE,
                                     check.size = FALSE)

#   Extract table, remove unneeded columns
mtDF <- temp$vst_mappingandtagging %>%
  dplyr::select("domainID",
                "siteID",
                "plotID",
                "date",
                "pointID",
                "stemDistance",
                "stemAzimuth",
                "recordType",
                "individualID",
                "taxonID",
                "scientificName",
                "genus",
                "family",
                "remarks",
                "dataQF")



### Retrieve PPPY, AI, NW tables for all sites above, then filter to required eventIDs

temp <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                     site = c("BLAN", "SCBI", "DSNY", "KONZ", "YELL", "SRER", "ABBY", "WREF", "SOAP", "PUUM"),
                                     release = "LATEST",
                                     include.provisional = TRUE,
                                     check.size = FALSE)

#   Define eventIDs in test dataset
theEvents <- c("vst_BLAN_2020", "vst_BLAN_2021", "vst_BLAN_2022",
               "vst_SCBI_2023", "vst_SCBI_2024",
               "vst_DSNY_2017", "vst_DSNY_2022",
               "vst_KONZ_2019", "vst_KONZ_2020", "vst_KONZ_2021", "vst_KONZ_2022", "vst_KONZ_2023", "vst_KONZ_2024",
               "vst_YELL_2020", "vst_YELL_2021",
               "vst_SRER_2016", "vst_SRER_2021",
               "vst_ABBY_2016", "vst_ABBY_2017", "vst_ABBY_2018", "vst_ABBY_2019", "vst_ABBY_2020", "vst_ABBY_2021", "vst_ABBY_2022",
               "vst_ABBY_2023", "vst_ABBY_2024",
               "vst_WREF_2024", "vst_WREF_2025",
               "vst_SOAP_2023", "vst_SOAP_2024",
               "vst_PUUM_2022", "vst_PUUM_2023")

#   Generate vst_perplotperyear test table
pppyDF <- temp$vst_perplotperyear %>%
  dplyr::filter(eventID %in% theEvents) %>%
  dplyr::select("date",
                "nonwoodyCollectDate",
                "domainID",
                "siteID",
                "plotID",
                "plotType",
                "nlcdClass",
                "samplingImpractical",
                "eventID",
                "eventType",
                "dataCollected",
                "targetTaxaPresent",
                "treesPresent",
                "shrubsPresent",
                "lianasPresent",
                "palmsPresent",
                "treeFernsPresent",
                "totalSampledAreaTrees",
                "totalSampledAreaShrubSapling",
                "totalSampledAreaLiana",
                "totalSampledAreaFerns",
                "totalSampledAreaOther",
                "remarks",
                "dataQF")

#   Generate vst_apparentindividual test table
aiDF <- temp$vst_apparentindividual %>%
  dplyr::filter(eventID %in% theEvents) %>%
  dplyr::select("domainID",
                "siteID",
                "plotID",
                "subplotID",
                "date",
                "eventID",
                "individualID",
                "tempStemID",
                "growthForm",
                "plantStatus",
                "stemDiameter",
                "measurementHeight",
                "changedMeasurementLocation",
                "height",
                "heightQualifier",
                "maxCrownDiameter",
                "ninetyCrownDiameter",
                "basalStemDiameter",
                "basalStemDiameterMsrmntHeight",
                "initialGapMeasurementDate",
                "initialBandStemDiameter",
                "initialDendrometerGap",
                "dendrometerGap",
                "dendrometerCondition",
                "bandStemDiameter",
                "remarks",
                "dataQF")

#   Generate vst_non-woody test table
nwDF <- temp$`vst_non-woody` %>%
  dplyr::filter(eventID %in% theEvents) %>%
  dplyr::select("domainID",
                "siteID",
                "plotID",
                "subplotID",
                "date",
                "eventID",
                "individualID",
                "taxonID",
                "scientificName",
                "growthForm",
                "plantStatus",
                "leafNumber",
                "meanLeafLength",
                "meanPetioleLength",
                "meanBladeLength",
                "basalStemDiameter",
                "meanBasalDiameter",
                "stemDiameter",
                "stemLength",
                "measurementHeight",
                "height",
                "maxCrownDiameter",
                "ninetyCrownDiameter",
                "oldPadCount",
                "newPadCount",
                "stemCount",
                "branchCount",
                "meanBranchLength",
                "remarks")


##  Bundle and save
testData <- list(vst_perplotperyear = pppyDF,
                 vst_mappingandtagging = mtDF,
                 vst_apparentindividual = aiDF,
                 `vst_non-woody` = nwDF)

saveRDS(object = testData,
        file = "vst_testDat.rds")



##  Find plots that have no trees in two consecutive eventIDs (and hence zero "tree" productivity)
WITH ranked_data AS (
  SELECT
  siteid,
  plotid,
  eventid,
  LAG(eventid) OVER (PARTITION BY plotid ORDER BY eventid) AS prev_eventid,
  treespresent,
  LAG(treespresent) OVER (PARTITION BY plotid ORDER BY eventid) AS prev_trees
  FROM vstqaqc.perplotperyear
)
SELECT DISTINCT plotid, eventid, prev_eventid, treespresent, prev_trees
FROM ranked_data
WHERE siteid='DSNY'
AND treespresent = 'N'
AND prev_trees = 'N';

#--> ABBY, BLAN, DSNY, maybe others have examples




##  Find plot-level mortality transitions
WITH events AS (
  SELECT DISTINCT eventid
  FROM vstqaqc.perplotperyear
),
ordered_events AS (
  SELECT eventid,
  LEAD(eventid) OVER (ORDER BY eventid) AS next_eventid
  FROM events
)
SELECT DISTINCT s.plotid,
s.eventid AS eventid_when_present,
s."date"    AS date_when_present,
oe.next_eventid AS eventid_next
FROM ordered_events oe
JOIN vstqaqc.perplotperyear s
ON s.eventid = oe.eventid
AND s.targettaxapresent = 'Y'
JOIN vstqaqc.perplotperyear snext
ON snext.plotid = s.plotid
AND snext.eventid = oe.next_eventid
AND snext.targettaxapresent = 'N'
WHERE oe.next_eventid IS NOT NULL;

#--> looks like vst_SOAP_2023 --> vst_SOAP_2024 has one of these transitions for SOAP_056 and SOAP_059


##  Find plot-level recruitment transitions
WITH events AS (
  SELECT DISTINCT eventid
  FROM vstqaqc.perplotperyear
),
ordered_events AS (
  SELECT eventid,
  LEAD(eventid) OVER (ORDER BY eventid) AS next_eventid
  FROM events
)
SELECT DISTINCT s.plotid,
s.eventid AS eventid_earlier,
s."date"   AS date_earlier,
snext.eventid AS eventid_later,
snext."date"   AS date_later
FROM ordered_events oe
JOIN vstqaqc.perplotperyear s
ON s.eventid = oe.eventid
AND s.treespresent = 'N'
JOIN vstqaqc.perplotperyear snext
ON snext.plotid = s.plotid
AND snext.eventid = oe.next_eventid
AND snext.treespresent IN ('Y', 'Present - sampled')
WHERE oe.next_eventid IS NOT NULL;

#--> BLAN_032 from 2021 -> 2022 --> only 1 single bole tree in 2022, could work
#--> BLAN_057 from 2017 -> 2018 --> 5 trees in 2018, none in 2017 but also only 1 'smt' in 2017 --> strange but could work
#--> HEAL_055 from 2024 -> 2025 --> Nope. 'treespresent' values in perplotperyear likely incorrect.
#--> JERC_056 from 2018 -> 2019 --> No data at all for 2018, over 200 records for 2019 --> 2018 data likely not correct
#--> CLBJ_024 from 2018 -> 2019 --> No tree growth forms in either event; likely a 'treespresent' error
#--> ABBY_068 and 069, others from 2017 -> 2018 --> incorrect treespresent = "N" for all plots in this eventID
#--> YELL_001 and 010 from 2018 -> 2019 --> incorrect treespresent = "Y" in 2019
#--> KONZ plots all seem to have incorrect recording of treespresent

#--> BLAN_032 only example of entire plot having no trees one event and then trees in the next.


##  Find tree individuals in a specific site with presence = TRUE, FALSE, TRUE for three sequential eventIDs:
WITH all_events AS (
  SELECT DISTINCT eventid
  FROM vstqaqc.apparentindividual
  WHERE siteid = 'ABBY'
  AND growthform IN ('multi-bole tree', 'single bole tree')
  ORDER BY eventid
  LIMIT 3
),
events_indexed AS (
  SELECT eventid, ROW_NUMBER() OVER (ORDER BY eventid) AS rn
  FROM all_events
),
inds AS (
  SELECT DISTINCT individualid
  FROM vstqaqc.apparentindividual
  WHERE siteid = 'ABBY'
  AND growthform IN ('multi-bole tree', 'single bole tree')
),
ind_event AS (
  SELECT i.individualid, e.eventid, e.rn
  FROM inds i
  CROSS JOIN events_indexed e
),
presence AS (
  SELECT
  ie.individualid,
  ie.rn,
  CASE WHEN ai.individualid IS NULL THEN FALSE ELSE TRUE END AS present
  FROM ind_event ie
  LEFT JOIN vstqaqc.apparentindividual ai
  ON ai.individualid = ie.individualid
  AND ai.eventid = ie.eventid
  AND ai.siteid = 'ABBY'
  AND growthform IN ('multi-bole tree', 'single bole tree')
)
SELECT individualid
FROM presence
GROUP BY individualid
HAVING
COUNT(*) = 3
AND BOOL_AND((rn = 1 AND present) OR (rn <> 1))
AND BOOL_AND((rn = 2 AND NOT present) OR (rn <> 2))
AND BOOL_AND((rn = 3 AND present) OR (rn <> 3));


#   Chave et al 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Global Change Biology 20:3177-3190
# install.packages("raster"); install.packages("ncdf4"); library("raster"); library("ncdf4")
# source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
# coord <- data.frame(siteID = c("GUAN", "LAJA", "PUUM"), longitude = c(-66.8687, -67.07689, -155.31731), latitude = c(17.96955, 18.02126, 19.55309) );  rownames(coord) <- coord$siteID; coord$siteID <- NULL
# Chave_et_al_2014_E <- retrieve_raster("E",coord,plot=TRUE,format="nc") returns an E of 0.5074847 for GUAN, 0.4440793 for LAJA, and NA for PUUM



