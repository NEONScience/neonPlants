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


#   Check current tree fern output vs what I think it should be based on Asner et al 2011
cibDF <- vst_agb_other %>%
  dplyr::filter(growthForm == "large tree fern") %>%
  dplyr::select("individualID",
                "taxonID",
                "stemDiameter",
                "height",
                "stemLength") %>%
  dplyr::mutate(agbOrig = round(0.2085 * (pi * (stemDiameter/2)^2 * height * 100 * 0.22/1000),
                                digits = 3),
                agbCorr = round(pi * (stemDiameter/2)^2 * stemLength * 100 * 0.22/1000,
                                digits = 3))


#   Equation format for Chojnacki 2014 (Table 5)
log(agb) = b0 + b1 * log(stemDiameter)

# Choj$allometry_ID <- NA
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Abies" & Choj$spg_gcm3 < 0.35,
#                             "C1",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Abies" & Choj$spg_gcm3 >= 0.35,
#                             "C2",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Cupressaceae" & Choj$spg_gcm3 <0.30,
#                             "C3",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Cupressaceae" &
#                               Choj$spg_gcm3 >= 0.30 & Choj$spg_gcm3 < 0.40,
#                             "C4",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Cupressaceae" & Choj$spg_gcm3 >=0.40,
#                             "C5",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Larix",
#                             "C6",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Picea" & Choj$spg_gcm3 < 0.35,
#                             "C7",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Picea" & Choj$spg_gcm3 >= 0.35,
#                             "C8",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Pinus" & Choj$spg_gcm3 < 0.45,
#                             "C9",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Pinus" & Choj$spg_gcm3 >= 0.45,
#                             "C10",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" &
#                               (Choj$genus == "Pseudotsuga" | Choj$genus == "Taxus" | Choj$genus == "Pseudotsuga"),
#                             "C11",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Tsuga" & Choj$spg_gcm3 < 0.40,
#                             "C12",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Tsuga" & Choj$spg_gcm3 >= 0.40,
#                             "C13",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$family == "Aceraceae" & Choj$spg_gcm3 < 0.50,
#                             "H1",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$family == "Aceraceae" & Choj$spg_gcm3 >= 0.50,
#                             "H2",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae"
#                             & Choj$spg_gcm3 < 0.40,
#                             "H3",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae" &
#                               Choj$spg_gcm3 >= 0.40 & Choj$spg_gcm3 < 0.50,
#                             "H4",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae" &
#                               Choj$spg_gcm3 >= 0.50 & Choj$spg_gcm3 < 0.60,
#                             "H5",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Betulaceae" &
#                               Choj$spg_gcm3 >= 0.60,
#                             "H6",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((Choj$family == "Cornaceae" | Choj$family == "Ericaceae" | Choj$family == "Lauraceae" |
#                                Choj$family == "Platanaceae" | Choj$family == "Rosaceae" | Choj$family == "Ulmaceae"),
#                             "H7",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$genus == "Carya",
#                             "H8",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & (Choj$family == "Fabaceae" | Choj$family == "Juglandaceae") &
#                               Choj$genus != "Carya",
#                             "H9",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Fagaceae" & Choj$decid_vs_ever == "decid",
#                             "H10",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "forest" & Choj$family == "Fagaceae" & Choj$decid_vs_ever == "ever",
#                             "H11",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$family == "Hamamelidaceae",
#                             "H12",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((Choj$family == "Hippocastanaceae" | Choj$family == "Tiliaceae"),
#                             "H13",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$family == "Magnoliaceae",
#                             "H14",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Oleaceae" &
#                               Choj$spg_gcm3 < 0.55,
#                             "H15",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Oleaceae" &
#                               Choj$spg_gcm3 >= 0.55,
#                             "H16",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Salicaceae" &
#                               Choj$spg_gcm3 < 0.35,
#                             "H17",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse((is.na(Choj$allometry_ID) | Choj$allometry_ID == "") & Choj$family == "Salicaceae" &
#                               Choj$spg_gcm3 >= 0.35,
#                             "H18",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & Choj$family == "Cupressaceae",
#                             "W1",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & (Choj$family == "Fabaceae" | Choj$family == "Rosaceae"),
#                             "W2",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & Choj$family == "Fagaceae",
#                             "W3",
#                             Choj$allometry_ID)
#
# Choj$allometry_ID <- ifelse(Choj$woodland_vs_forest == "woodland" & Choj$family == "Pinaceae",
#                             "W4",
#                             Choj$allometry_ID)
#
# #   Arbitrarily picked C9 (forest) over C10 (forest spg_gcm3>=0.45) or W4 (woodland)
# Choj$allometry_ID <- ifelse(Choj$taxonID == "PINACE",
#                             "C9",
#                             Choj$allometry_ID)
#
# #   Arbitrarily picked H9 (forest) over W2 (woodland)
# Choj$allometry_ID <- ifelse(Choj$taxonID == "FABACE",
#                             "H9",
#                             Choj$allometry_ID)
#


