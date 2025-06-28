### Dev code for estimateWoodMass function ####
library(neonUtilities)
library(tidyverse)


VstDat <- readRDS(testthat::test_path("testdata", "VstDat.rds"))

inputDataList <- VstDat

### Removed yucca code ####
# #   Estimate yucca biomass: White, J.D., K.J. Gutzwiller, W.C. Barrow, L.J. Randall, and P. Swint. 2008. Modeling mechanisms of vegetation change due to fire in a semi-arid ecosystem. Ecological Modelling 214:181-200; divide by 1000 to convert output from "grams" to "kg"
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
#                             NA)


### Removed ocotillo code ####
# #   Estimate total ocotillo mass: aboveground + belowground
# vst_agb_other$tot_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
#                                      exp(-0.2889 + 2.2222 * log(vst_agb_other$height)),
#                                      NA)
#
# #   Estimate aboveground ocotillo mass
# vst_agb_other$agb_ocotillo <- ifelse(vst_agb_other$growthForm == "ocotillo",
#                                      round(1/(exp(-0.63 - 0.18 * log(vst_agb_other$tot_ocotillo)) + 1) *
#                                              vst_agb_other$tot_ocotillo,
#                                            digits = 3),
#                                      NA)

ocoDF <- vst_agb_other %>%
  dplyr::filter(growthForm == "ocotillo") %>%
  dplyr::select("individualID",
                "growthForm",
                "taxonID",
                "height",
                "stemCount")

ocoDF <- ocoDF %>%
  dplyr::mutate(logDryMass = (log(height) - 0.13) / 0.45,
                logRS = -0.63 + 0.18 * logDryMass,
                dryMass = exp(logDryMass),
                RS = exp(logRS),
                agb = dryMass / (1 + RS))

#   To derive agb formula:
dryMass = agb + bgb
RS = bgb/agb --> bgb = agb*RS
dryMass = agb + agb*RS
dryMass = agb * (1 + RS)
agb = dryMass / (1 + RS)

test <- vst_agb_other %>%
  dplyr::filter(!is.na(agb_xer))



#   Check palm biomass estimation for 'small palms'
palmDF <- vst_agb_other %>%
  dplyr::filter(growthForm == "small palm") %>%
  dplyr::select("individualID",
                "growthForm",
                "taxonID",
                "height",
                "leafNumber",
                "meanPetioleLength",
                "meanBladeLength")

palmDF <- palmDF %>%
  dplyr::mutate(rachisMass_g = exp(-10.38 + 2.72 * log(meanPetioleLength + meanBladeLength)),
                bladeMass_g = -13.31 + 0.85 * meanBladeLength,
                totalMass_g = leafNumber * (rachisMass_g + bladeMass_g),
                totalMass_kg = round(totalMass_g / 1000,
                                     digits = 3))

#--> Alexis et al 2007 Biogeochemistry use Gholz 1999 equation and sum the petiole + blade for input to rachis biomass equation.

#   Modify palmDF to add NAs for case_when testing
palmDF$leafNumber[1] <- NA
palmDF$meanBladeLength[1] <- NA
palmDF$meanPetioleLength[1] <- NA

palmDF <- palmDF %>%
  dplyr::mutate(check = dplyr::case_when(!dplyr::if_all(c("leafNumber", "meanBladeLength", "meanPetioleLength"), is.na) ~ "something",
                                         TRUE ~ "nothing"))




