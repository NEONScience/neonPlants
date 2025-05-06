### Goal: Verify that consumption has to be calculated at the site level per bout and then summed across bouts.
###       SS has taken plot level approach want to know whether this gets one to the same place or not.

library(neonUtilities)

hbp <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                    site = "SJER",
                                    startdate = "2022-01",
                                    enddate = "2023-12",
                                    check.size = FALSE,
                                    token = Sys.getenv("NEON_TOKEN"))

boutDF <- hbp$hbp_perbout
massDF <- hbp$hbp_massdata

#   Identify all bouts that are part of a Mediterranean growing season for 2022 as test case --> 2022-12-19 to 2023-06-07
#   Remove qaDryMass == Y values for this exercise
temp <- massDF %>%
  dplyr::left_join(boutDF %>%
                      dplyr::select(eventID, subplotID, clipArea, exclosure, sampleID),
                    by = "sampleID") %>%
  dplyr::relocate(eventID, subplotID, clipArea, exclosure, sampleID,
                  .before = setDate) %>%
  dplyr::mutate(collectDate = as.Date(collectDate)) %>%
  dplyr::filter(collectDate < "2023-06-08" & collectDate > "2022-12-18",
                qaDryMass == "N")

#   Calculate scaledMass via clipArea (g/m2)
temp <- temp %>%
  dplyr::mutate(scaledMass = round(dryMass / clipArea,
                                   digits = 2),
                .after = dryMass)

#   Sum scaledMass by sampleID --> don't care about herbGroups for this exercise, only total mass per sampleID
sampleDF <- temp %>%
  dplyr::group_by(siteID, 
                  plotID,
                  subplotID,
                  eventID,
                  sampleID,
                  collectDate,
                  exclosure) %>%
  dplyr::summarise(mass = sum(scaledMass, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::arrange(eventID,
                 plotID,
                 subplotID,
                 exclosure)

#   Identify and remove duplicates based on plotID, subplotID, eventID, and exclosure
#   Dupes arise maybe due to incorrect recording of "exclosure"?
dupDF <- sampleDF[duplicated(paste(sampleDF$plotID,
                                   sampleDF$subplotID,
                                   sampleDF$eventID,
                                   sampleDF$exclosure,
                                   sep = "-")), ]

sampleDF <- sampleDF %>%
  dplyr::filter(!sampleID %in% dupDF$sampleID)



### Estimate productivity per Science Design approach ####

#   Calculate mean exclosure == "Y" and exclosure == "N" mass by eventID
exclosureDF <- sampleDF %>%
  dplyr::group_by(siteID,
                  eventID,
                  exclosure) %>%
  dplyr::summarise(meanMass = round(mean(mass, na.rm = TRUE),
                                    digits = 2),
                   sampleSize = n(),
                   .groups = "drop")

#   Calculate mean consumption per eventID
eventConDF <- exclosureDF %>%
  dplyr::select(-sampleSize) %>%
  tidyr::pivot_wider(names_from = "exclosure",
                     values_from = "meanMass",
                     names_prefix = "exclosure") %>%
  dplyr::mutate(consumption = exclosureY - exclosureN)

#   Calculate productivity as sum of consumption for all eventIDs plus standing ambient biomass for last bout
prod <- sum(eventConDF$consumption) + dplyr::last(eventConDF$exclosureN)

#--> 272.18 g/m2



### Estimate productivity using paired exclosures within subplotIDs ####

#   Calculate consumption within each subplotID
subplotDF <- sampleDF %>%
  dplyr::select(-sampleID,
                -collectDate) %>%
  tidyr::pivot_wider(names_from = "exclosure",
                     values_from = "mass",
                     names_prefix = "exclosure") %>%
  dplyr::filter(!is.na(exclosureY)) %>%
  dplyr::mutate(consumption = exclosureY - exclosureN)

#   Calculate mean consumption per eventID --> assumption: data within each subplotID are independent
plotEventDF <- subplotDF %>%
  dplyr::group_by(siteID,
                  eventID) %>%
  dplyr::summarise(meanCon = mean(consumption))

#   Calculate mean "ambient" standing biomass for last eventID
lastEvent <- dplyr::last(sort(unique(sampleDF$eventID)))

finalAmbient <- sampleDF %>%
  dplyr::filter(eventID == lastEvent,
                exclosure == "N")

finalAmbient <- round(mean(finalAmbient$mass),
                      digits = 2)

plotProd = sum(plotEventDF$meanCon) + finalAmbient

#--> 267.36 g/m2

