#' @title Estimate mass of woody individuals with allometric equations
#'
#' @author
#' Courtney L Meier \email{cmeier@battelleecology.org} \cr
#'
#' @description Estimate the biomass of woody individuals via generalized, growth form specific, and species-specific allometric equations.
#'
#' @details This is a helper function for the estimateWoodMass() function.
#'
#' @param appIndTable Table (generated in estimateWoodMass()) containing records for qualifying stems of woody individuals. The typical input is a single table created by joining vst_apparentindividual data with the vst_mappingandtagging table (filtered to most recent records per individualID), to obtain 'taxonID', 'scientificName', 'genus', and 'family' fields. [data.frame]
#'
#' @param growthFormSubset Select Vegetation Structure growth forms for biomass estimation. The options are "tree", which enables biomass estimation only for single- and multi-bole trees with a DBH ≥ 10 cm, and the default of "all", which includes "tree" individuals, and also small trees, single shrubs, small shrubs, and liana individuals. [character]
#'
#' @return A table with biomass estimates:
#'    * For trees, small trees, and lianas: One record for each qualifying bole/stem at each time point it was measured.
#'    * For shrubs, small shrubs, and saplings: One record for each qualifying individual at each time point it was measured. For multi-stem individuals, input records for multiple stems are combined into a single live and dead biomass estimate per individual.
#'
#' @keywords internal


estimateAllometricWoodyMass <- function(appIndTable,
                                        growthFormSubset = NA) {

  aiDF <- appIndTable



  ### Resolve missing taxonIDs, missing scientificName, and Betula slash species
  aiDF <- aiDF %>%
    dplyr::mutate(taxonID = dplyr::case_when(is.na(.data$taxonID) ~ "2PLANT",
                                             .data$taxonID == "BEGL/BENA" ~ "BEGL",
                                             TRUE ~ .data$taxonID)) %>%

    dplyr::mutate(scientificName = dplyr::case_when(.data$taxonID == "2PLANT" & is.na(.data$scientificName) ~ "Unknown plant",
                                                    TRUE ~ .data$scientificName))



  ### Prepare table of allometric parameters by taxonID ####

  ### Distill all distinct taxonomic data in 'aiDF' input data frame
  inputTaxonDF <- aiDF %>%
    dplyr::distinct(.data$taxonID,
                    .data$scientificName,
                    .data$genus,
                    .data$family)



  ### Load external data required to assign Chojnacky parameters to taxonID

  #   Read in the Chojnacky et al 2014 parameters for each of their 35 defined allometric groups
  data("parameters", envir = environment())

  parameters <- parameters %>%
    dplyr::select("allometry_ID",
                  "b0",
                  "b1",
                  "minDiameter",
                  "maxDiameter")

  #   Load wood density, veg type, and other taxon-specific data needed to assign species to Chojnacky allometry groups
  data("taxon_fields", envir = environment())

  #   Load USDA Plants characteristics to get PLANTS.Floristic.Area and Native.Status: Filtered to records that have PLANTS.Floristic.Area, Native.Status, or both
  data("plantIntTrop", envir = environment())



  ### Associate input taxonID data with wood density, USDA native/introduced data, etc., and assign Chojnacky allometryID
  ##  Join all taxonomic data
  inputTaxonDF <- dplyr::left_join(inputTaxonDF,
                                   taxon_fields,
                                   by = "taxonID") %>%
    dplyr::left_join(plantIntTrop,
                     by = "taxonID")


  ##  Stanardize 'nativeStatus' and 'tropical' LOV elements
  inputTaxonDF$nativeStatus <- dplyr::if_else(inputTaxonDF$nativeStatus == "int",
                                              "introduced",
                                              "native",
                                              "native")

  inputTaxonDF$tropical <- dplyr::if_else(inputTaxonDF$tropical == "trop",
                                          "tropical",
                                          "temperate",
                                          "temperate")


  ##  Assign Chojnacky 'allometryID' to taxa in the input data
  inputTaxonDF <- inputTaxonDF %>%

    dplyr::mutate(allometryID = dplyr::case_when(

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
      TRUE ~ NA_character_

    ),
    .before = "taxonID") %>%

    #   Identify taxa not in Chojnacky
    dplyr::mutate(source = ifelse(!is.na(.data$allometryID),
                                  "Chojnacky_etal_2014",
                                  NA_character_)) %>%

    #   Reduce to desired columns
    dplyr::select("allometryID",
                  "taxonID",
                  "family",
                  "genus",
                  "spg_gcm3",
                  "scientificName",
                  "nativeStatus",
                  "tropical",
                  "source")


  ##  Assign Chojnacky parameters to taxa based on allometryID
  inputTaxonDF <- dplyr::left_join(inputTaxonDF,
                                   parameters,
                                   by = c("allometryID" = "allometry_ID"))



  ### Finalize input dataset: Join 'aiDF' with 'inputTaxonDF' and make manual 'tropical' status updates
  ##  Join data frames
  aiDF <- dplyr::left_join(aiDF,
                           inputTaxonDF,
                           by = c("taxonID", "scientificName", "genus", "family"))


  ##  Manually assign 'tropical' and 'temperate' status for a subset of taxonIDs
  aiDF <- aiDF %>%
    dplyr::mutate(tropical = dplyr::case_when(.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("2PLANT", "2PLANT-H", "ANAL12", "BOURR", "BUMI6",
                                                                     "CONVOL", "CROSS", "FABACE", "JACQU", "JACQU2",
                                                                     "COPRO", "HYDRAN")
                                              ~ "tropical",
                                              TRUE ~ .data$tropical))

  aiDF <- aiDF %>%
    dplyr::mutate(tropical = dplyr::case_when(!.data$siteID %in% c("GUAN", "LAJA", "PUUM") &
                                                .data$taxonID %in% c("AMAR5", "CELTI", "DAWR2", "LIJA", "MEAZ", "OPUNT",
                                                                     "RHUS", "SAMBU", "SMSM", "SYMPL2", "VITIS")
                                              ~ "temperate",
                                              TRUE ~ .data$tropical))


  ##  Assumption: For tropical species, if specific gravity unknown assume 0.5 g/cm3 forChave et al 2014 allometry, following Asner et al 2011
  aiDF$spg_gcm3 <- dplyr::if_else(is.na(aiDF$spg_gcm3) & aiDF$tropical == "tropical",
                                  0.5,
                                  aiDF$spg_gcm3,
                                  aiDF$spg_gcm3)



  ### Multi-bole trees data prep: Assign measured height to all boles ####
  #--> Assume that 'height' of individual that is measured for primary bole applies to secondary boles. Secondary 'mbt' boles at PUUM with no 'height' would otherwise have AGB = NA since Chave "E" parameter unavailable for PUUM and "E" is needed to estimate 'height' required by Chave allometry when 'height' is missing.

  nonMbt <- aiDF %>%
    dplyr::filter(.data$growthForm != "multi-bole tree" | is.na(.data$growthForm))

  mbt <- aiDF %>%
    dplyr::filter(.data$growthForm == "multi-bole tree") %>%
    dplyr::mutate(tempIndivID = stringr::str_extract(string = .data$individualID,
                                                     pattern = "^NEON.PLA.D[0-9]{2}.[A-Z]{4}.[0-9]{5}"),
                  .before = "individualID")

  #   Assign height and crown dimensions from primary bole to secondary boles
  heightMBT <- mbt %>%
    dplyr::group_by(.data$tempIndivID) %>%
    dplyr::summarise(height = ifelse(!all(is.na(.data$height)),
                                     max(.data$height, na.rm = TRUE),
                                     NA_real_))

  mbt <- dplyr::left_join(mbt %>%
                            dplyr::select(-"height"),
                          heightMBT,
                          by = "tempIndivID") %>%
    dplyr::relocate("height",
                    .after = "liveDeadStatus") %>%
    dplyr::select(-"tempIndivID")

  #   Re-assemble single data frame with updated 'mbt' data
  aiDF <- dplyr::bind_rows(nonMbt,
                           mbt)

  #   Clean-up
  rm(mbt, nonMbt, heightMBT)



  ### Shrub data prep: Assign height and crown dimensions to all boles, aggregate diameter by liveDeadStatus ####
  #--> For shrubs, height and crown dimensions are measured once per individualID; apply these measurements to all emergent boles so that basalDiameter can be aggregated by liveDeadStatus for each individualID, and separate live and dead biomass estimates per individualID can be generated. Crown dimensions are particularly important for some taxon-specific allometries (e.g., ARTR2).

  ### Conditionally process individuals with shrub/sapling growthForm
  #--> Do not want to require 'crownDiameter' data when only estimating biomass for recruited trees

  if (growthFormSubset == "all") {

    #   Separate shrubs from other growthForms to calculate aggregated basalStemDiameter inputs for Conti allometry
    nonShrub <- aiDF %>%
      dplyr::filter(!.data$growthForm %in% c("single shrub", "small shrub", "sapling") | is.na(.data$growthForm))

    shrub <- aiDF %>%
      dplyr::filter(.data$growthForm %in% c("single shrub", "small shrub", "sapling"))

    #   Determine max height, crownDiameters per individualID
    heightCrownShrub <- shrub %>%
      dplyr::group_by(.data$individualID) %>%
      dplyr::summarise(height = ifelse(!all(is.na(.data$height)),
                                       max(.data$height, na.rm = TRUE),
                                       NA_real_),
                       maxCrownDiameter = ifelse(!all(is.na(.data$maxCrownDiameter)),
                                                 max(.data$maxCrownDiameter, na.rm = TRUE),
                                                 NA_real_),
                       ninetyCrownDiameter = ifelse(!all(is.na(.data$ninetyCrownDiameter)),
                                                    max(.data$ninetyCrownDiameter, na.rm = TRUE),
                                                    NA_real_))

    #   Group multiple stems belonging to same individualID x liveDeadStatus combination, and calculate equivalent stemDiameter and basalStemDiameter.
    shrub <- shrub %>%
      dplyr::group_by(.data$domainID,
                      .data$siteID,
                      .data$plotID,
                      .data$subplotID,
                      .data$taxonID,
                      .data$family,
                      .data$genus,
                      .data$scientificName,
                      .data$individualID,
                      .data$liveDeadStatus,
                      .data$eventID,
                      .data$date,
                      .data$growthForm,
                      .data$spg_gcm3,
                      .data$nativeStatus,
                      .data$tropical,
                      .data$source,
                      .data$allometryID,
                      .data$b0,
                      .data$b1,
                      .data$minDiameter,
                      .data$maxDiameter) %>%

      #   Calculate equivalent stemDiameters and mean measurementHeight values
      dplyr::summarise(stemDiameter = ifelse(!all(is.na(.data$stemDiameter)),
                                             round(sqrt(sum(.data$stemDiameter^2, na.rm = TRUE)),
                                                   digits = 1),
                                             NA_real_),
                       basalStemDiameter = ifelse(!all(is.na(.data$basalStemDiameter)),
                                                  round(sqrt(sum(.data$basalStemDiameter^2, na.rm = TRUE)),
                                                        digits = 1),
                                                  NA_real_),
                       .groups = "drop")

    #  Join with 'heightCrownShrub' to assign crown dimensions based on individualID
    shrub <- dplyr::left_join(shrub,
                              heightCrownShrub,
                              by = "individualID")

    #   Re-assemble single data frame with updated 'shrub' data
    aiDF <- dplyr::bind_rows(nonShrub,
                             shrub)

    #   Clean-up
    rm(nonShrub, shrub, heightCrownShrub)

  } # End 'growthFormSubset' conditional



  ### Chojnacky biomass: Generate AGB estimate using Chojnacky et al 2014 allometries ####
  #--> Returns NA when parameters cannot be assigned

  aiDF <- aiDF %>%
    dplyr::mutate(
      agb_kg = dplyr::case_when(
        !is.na(.data$allometryID) ~ round(exp(.data$b0 + .data$b1 * log(.data$stemDiameter)),
                                          digits = 2),
        TRUE ~ NA_real_
      )
    )



  ### Chave tropical biomass: Generate AGB estimate using Chave et al 2014 equations ####
  #--> Use instead of Chojnacky et al 2014 allometries when individual is "tropical"
  #--> Different equations when 'height' is/is not available in the data

  aiDF <- aiDF %>%

    #   Assign Chave 'E' parameter
    dplyr::mutate(

      Chave_E = dplyr::case_when(.data$siteID == "GUAN" ~ 0.5074847,
                                 .data$siteID == "LAJA" ~ 0.4440793,
                                 TRUE ~ NA_real_),
      .after = "maxDiameter") %>%

    #   Estimate Chave biomass
    dplyr::mutate(

      agb_chave_kg = dplyr::case_when(

        #   Biomass estimate when 'height', 'stemDiameter', and 'spg_gcm3' all present
        .data$tropical == "tropical" & !dplyr::if_any(c("height", "stemDiameter", "spg_gcm3"), is.na) ~
          round(0.0673 * (.data$spg_gcm3 * (.data$stemDiameter^2) * .data$height)^0.976, digits = 2),

        #   Biomass estimate when 'height' missing and 'stemDiameter' and 'spg_gcm3' present
        .data$tropical == "tropical" & is.na(.data$height) & !dplyr::if_any(c("stemDiameter", "spg_gcm3"), is.na) ~
          round(exp(-1.803 - (0.976 * .data$Chave_E) + (0.976 * log(.data$spg_gcm3)) + (2.673 * log(.data$stemDiameter)) -
                      (0.0299 * (log(.data$stemDiameter))^2)),
                digits = 2),

        TRUE ~ NA_real_

      ),

      #   Assign "Chave" as allometry source for tropical species
      source = dplyr::replace_when(.data$source,
                                   !is.na(.data$agb_chave_kg) ~ "Chave_etal_2014"),

      #   Update 'agb_kg' with Chave estimate for tropical species
      agb_kg = dplyr::replace_when(.data$agb_kg,
                                   !is.na(.data$agb_chave_kg) ~ .data$agb_chave_kg)
    ) %>%

    dplyr::select(-"agb_chave_kg")



  ### Conti shrub biomass: Generate shrub and sapling AGB estimates using Conti et al 2019 equations ####
  #--> Different equations depending on availability of 'basalStemDiameter', 'crownDiameter' and 'height' variables.
  #--> Equations applied below in order of least to most uncertainty in the AGB estimate
  #--> Confirmed with G. Conti that geometric mean might be more appropriate but arithmetic mean was used to construct allometries

  if (growthFormSubset == "all") {

    aiDF <- aiDF %>%
      ##  Calculate mean crownDiameter for shrubs from max/ninety crown diameters
      dplyr::mutate(meanCrownDiameter = dplyr::case_when(

        .data$growthForm %in% c("single shrub", "small shrub", "sapling") &
          !is.na(.data$maxCrownDiameter) & !is.na(.data$ninetyCrownDiameter) ~
          round(rowMeans(dplyr::across(c("maxCrownDiameter", "ninetyCrownDiameter")), na.rm = TRUE),
                digits = 1),
        TRUE ~ NA_real_

      ),
      .after = "ninetyCrownDiameter") %>%


      ##  Estimate Conti biomass
      dplyr::mutate(

        agb_conti_kg = dplyr::case_when(

          #   Biomass when 'basalStemDiameter' and 'meanCrownDiameter' available
          .data$growthForm %in% c("single shrub", "small shrub", "sapling") &
            !is.na(.data$basalStemDiameter) & !is.na(.data$meanCrownDiameter) ~
            round(exp(-2.057 + 1.741 * log(.data$basalStemDiameter) + 0.945 * log(.data$meanCrownDiameter)), digits = 2),

          #   Biomass when 'basalStemDiameter' available but not 'meanCrownDiameter'
          .data$growthForm %in% c("single shrub", "small shrub", "sapling") &
            !is.na(.data$basalStemDiameter) & is.na(.data$meanCrownDiameter) ~
            round(exp(-2.869 + 2.584 * log(.data$basalStemDiameter)), digits = 2),

          #   Biomass when 'height' and 'meanCrownDiameter' available but not 'basalStemDiameter'
          #--> Increased uncertainty for live/dead biomass estimates because 'height' and 'crown' derived from entire individual, not just the live/dead components
          .data$growthForm %in% c("single shrub", "small shrub", "sapling") &
            !is.na(.data$height) & !is.na(.data$meanCrownDiameter) ~
            round(exp(-0.370 + 1.903 * log(.data$meanCrownDiameter) + 0.652 * log(.data$height)) * 1.403, digits = 2),

          TRUE ~ NA_real_
        ),

        #   Assign "Conti" as allometry source for shrubs and saplings
        source = dplyr::replace_when(.data$source,
                                     !is.na(.data$agb_conti_kg) ~ "Conti_etal_2019"),

        #   Update 'agb_kg' with Conti estimate for shrub and sapling individuals
        agb_kg = dplyr::replace_when(.data$agb_kg,
                                     !is.na(.data$agb_conti_kg) ~ .data$agb_conti_kg)

      ) %>%

      dplyr::select(-"agb_conti_kg")

  } # End 'growthFormSubset' conditional



  ### MEPO5 biomass: Use species-specific Metrosideros polymorpha equations ####
  #--> Assumption: Species-specific allometry is better than Chojnacky or Chave
  #--> Litton & Kaufman 2008 equations used for individuals with 'stemDiameter' > 33 cm AND no 'height'; otherwise Selmants et al 2014 equations are used.

  aiDF <- aiDF %>%
    dplyr::mutate(

      agb_mepo_kg = dplyr::case_when(

        #   Biomass when 'stemDiameter' > 33 cm and 'height' available
        .data$taxonID == "MEPO5" & !is.na(.data$height) & !is.na(.data$spg_gcm3) & !is.na(.data$stemDiameter) & .data$stemDiameter >= 33 ~
          round(0.0776 * ((.data$spg_gcm3 * (.data$stemDiameter^2) * .data$height)^0.94), digits = 2),

        #   Biomass when 'stemDiameter' > 33 cm and 'height' unavailable
        .data$taxonID == "MEPO5" & is.na(.data$height) & !is.na(.data$stemDiameter) & .data$stemDiameter >= 33 ~
          round(0.88 * (.data$stemDiameter^1.86), digits = 2),

        #   Biomass when 'stemDiameter' <= 33 cm
        .data$taxonID == "MEPO5" & !is.na(.data$stemDiameter) & .data$stemDiameter < 33 ~
          round(0.2085 * (.data$stemDiameter^2.318), digits = 2),

        TRUE ~ NA_real_
      ),

      #   Assign MEPO5 allometry sources
      source = dplyr::replace_when(
        .data$source,
        .data$taxonID == "MEPO5" & !is.na(.data$height) & !is.na(.data$spg_gcm3) & !is.na(.data$stemDiameter) &
          .data$stemDiameter >= 33 ~ "Selmants_etal_2014",
        .data$taxonID == "MEPO5" & is.na(.data$height) & !is.na(.data$stemDiameter) & .data$stemDiameter >= 33 ~ "Litton_Kauffman_2008",
        .data$taxonID == "MEPO5" & !is.na(.data$stemDiameter) & .data$stemDiameter < 33 ~ "Selmants_etal_2014"
      ),

      #   Update 'agb_kg' with MEPO5-specific biomass estimates
      agb_kg = dplyr::replace_when(.data$agb_kg,
                                   !is.na(.data$agb_mepo_kg) ~ .data$agb_mepo_kg)
    ) %>%

    dplyr::select(-"agb_mepo_kg")



  ### RHDA biomass: Use species-specific Rhamnus davurica equations ####
  #--> Zhang etal 2012 present one equation for males and another for females; here, we take the average because NEON does not record sex of RHDA.
  #--> Output multiplied by 0.001 to convert to "kg"

  aiDF <- aiDF %>%
    dplyr::mutate(

      agb_rhda_kg = dplyr::case_when(
        .data$taxonID == "RHDA" & !is.na(.data$stemDiameter) ~
          round(0.001 * ((exp(5.237 + 1.996 * log(.data$stemDiameter)) + exp(5.016 + 2.306 * log(.data$stemDiameter))) / 2), digits = 2),

        TRUE ~ NA_real_
      ),

      #   Assign RHDA allometry source
      source = dplyr::replace_when(.data$source,
                                   !is.na(.data$agb_rhda_kg) ~ "Zhang_etal_2012"),

      #   Update 'agb_kg' with RHDA-specific biomass estimates
      agb_kg = dplyr::replace_when(.data$agb_kg,
                                   !is.na(.data$agb_rhda_kg) ~ .data$agb_rhda_kg)
    ) %>%

    dplyr::select(-"agb_rhda_kg")



  ### Cornus biomass: Use genus-specific Cornus equations ####
  #--> Lutz etal 2014 provide Cornus biomass equations utilizing either 'basalStemDiameter' or 'stemDiameter' as inputs
  #--> Output divided by 1000 to convert to "kg"

  if (growthFormSubset == "all") {

    aiDF <- aiDF %>%
      dplyr::mutate(

        agb_cornus_kg = dplyr::case_when(

          #   Biomass when 'stemDiameter' present
          grepl("Cornus", .data$scientificName) & .data$growthForm == "single shrub" & !is.na(.data$stemDiameter) ~
            round(exp(5.089 + 1.883 * log(.data$stemDiameter)) / 1000, digits = 2),

          #   Biomass when 'basalStemDiameter' present and 'stemDiameter' missing
          grepl("Cornus", .data$scientificName) & .data$growthForm %in% c("single shrub", "small shrub", "sapling")
          & !is.na(.data$basalStemDiameter) & is.na(.data$stemDiameter) ~
            round(exp(3.315 + 2.647 * log(.data$basalStemDiameter)) / 1000, digits = 2),

          TRUE ~ NA_real_
        ),

        #   Assign Cornus allometry source
        source = dplyr::replace_when(.data$source,
                                     !is.na(.data$agb_cornus_kg) ~ "Lutz_etal_2014"),

        #   Update 'agb_kg' with Cornus-specific biomass estimates
        agb_kg = dplyr::replace_when(.data$agb_kg,
                                     !is.na(.data$agb_cornus_kg) ~ .data$agb_cornus_kg)
      ) %>%

      dplyr::select(-"agb_cornus_kg")

  } # End 'growthFormSubset' conditional



  ### Liana biomass: Use liana-specific allometric equation ####
  #--> Schnitzer etal 2006 provide a liana biomass equation for tropical species; this equation is also used here for temperate liana species

  aiDF <- aiDF %>%
    dplyr::mutate(

      agb_liana_kg = dplyr::case_when(
        .data$growthForm == "liana" & !is.na(.data$stemDiameter) ~ round(exp(-1.484 + 2.657 * log(.data$stemDiameter)), digits = 2),

        TRUE ~ NA_real_
      ),

      #   Assign liana allometry source
      source = dplyr::replace_when(.data$source,
                                   !is.na(.data$agb_liana_kg) ~ "Schnitzer_etal_2006"),

      #   Update 'agb_kg' with liana-specific biomass estimates
      agb_kg = dplyr::replace_when(.data$agb_kg,
                                   !is.na(.data$agb_liana_kg) ~ .data$agb_liana_kg)
    ) %>%

    dplyr::select(-"agb_liana_kg")



  ### Source clean-up: Indicate "noAllometry" when biomass cannot be estimated ####
  aiDF <- aiDF %>%
    dplyr::mutate(

      source = dplyr::replace_when(.data$source,
                                   is.na(.data$source) ~ "noAllometry")
    )



  ### Output ####
  return(aiDF)


} # End function
