#' @title Estimate mass of non-woody individuals with allometric equations
#'
#' @author
#' Courtney L Meier \email{cmeier@battelleecology.org} \cr
#'
#' @description Estimate the biomass of non-woody individuals via growthForm-specific, and species-specific allometric equations.
#'
#' @details This is a helper function for the estimateWoodMass() function.
#'
#' @param nonWoodyTable Table (generated in estimateWoodMass()) containing records for qualifying stems of non-woody individuals.
#'
#' @param growthFormSubset Select Vegetation Structure growth forms for biomass estimation. The options are "tree", which enables biomass estimation only for palm trees and large tree ferns with a DBH ≥ 10 cm, and the default of "all", which includes "tree" individuals, and also small palms, small tree ferns, ferns, ocotillo, and xerophyllum individuals. [character]
#'
#' @return A table with biomass estimates:
#'    * For palm trees, tree-type small palms, large tree ferns, and small tree ferns: One record for each qualifying bole/stem at each time point it was measured.
#'    * For shrub-type small palms, ocotillo, and xerophyllum: One record for each qualifying individual at each time point it was measured.
#'
#' @keywords internal
#'

estimateAllometricOtherMass <- function(nonWoodyTable,
                                        growthFormSubset = NA) {

  nwDF <- nonWoodyTable



  ### Create 'agb_kg' for biomass estiamtes and 'source' column to track allometry source reference
  nwDF <- nwDF %>%
    dplyr::mutate(source = NA_character_,
                  agb_kg = NA_real_)



  ### Ocotillo biomass: Use species-specific equations ####
  #--> Equations from Bobich and Huxman 2009, International Journal of Plant Science
  #--> log(basalBranchCount) = 1.11 + 0.42 * log(totalMass_kg)
  #--> log(rootShoot) = -0.63 + 0.18 * log(totalMass_kg)
  #--> totalMass_kg = exp((log(basalBranchCount) - 1.11) / 0.42)
  #--> agb_kg = totalMass_kg / (1 + exp(-0.63) * totalMass_kg ^ 0.18)

  if (growthFormSubset == "all") {

    nwDF <- nwDF %>%
      dplyr::mutate(

        agb_ocoTot_kg = dplyr::case_when(
          .data$growthForm == "ocotillo" & !is.na(.data$stemCount) ~ exp((log(.data$stemCount) - 1.11) / 0.42),
          TRUE ~ NA_real_
        ),

        agb_oco_kg = dplyr::case_when(
          !is.na(.data$agb_ocoTot_kg) ~ round(.data$agb_ocoTot_kg / (1 + exp(-0.63) * .data$agb_ocoTot_kg ^ 0.18), digits = 2),
          TRUE ~ NA_real_
        ),

        #   Assign ocotillo allometry source
        source = dplyr::replace_when(.data$source,
                                     !is.na(.data$agb_oco_kg) ~ "Bobich_Huxman_2009"),

        #   Update 'agb_kg' with ocotillo biomass
        agb_kg = dplyr::replace_when(.data$agb_kg,
                                     !is.na(.data$agb_oco_kg) ~ .data$agb_oco_kg)
      ) %>%

      #   Remove 'agb_ocoTot_kg' and 'agb_oco_kg' columns
      dplyr::select(-"agb_ocoTot_kg",
                    -"agb_oco_kg")

  } # End 'growthFormSubset' conditional



  ### Fern biomass: Use species-specific equations ####
  #--> Equations from Gholz et al 1979; output divided by 1000 to convert to "kg"
  if (growthFormSubset == "all") {

    nwDF <- nwDF %>%
      dplyr::mutate(

        agb_frn_kg = dplyr::case_when(
          .data$growthForm == "fern" & .data$taxonID == "POMU" ~
            round((-2.5695 + 0.0643 * (.data$leafNumber * .data$meanLeafLength)) / 1000, digits = 2),

          .data$growthForm == "fern" & .data$taxonID == "PTAQ" ~
            round((3.1703 + 2.1433 * .data$basalStemDiameter) / 1000, digits = 2),

          TRUE ~ NA_real_
        ),

        #   Assign fern allometry source
        source = dplyr::replace_when(.data$source,
                                     !is.na(.data$agb_frn_kg) ~ "Gholz_etal_1979"),

        #   Update 'agb_kg' with fern biomass
        agb_kg = dplyr::replace_when(.data$agb_kg,
                                     !is.na(.data$agb_frn_kg) ~ .data$agb_frn_kg)
        ) %>%

      #   Remove 'agb_frn_kg' column
      dplyr::select(-"agb_frn_kg")

  }



  ### Xerophyllum biomass: Use species-specific equation ####
  #--> Equation from Gholz etal 1979; output divided by 1000 to convert to "kg"

  if (growthFormSubset == "all") {

    nwDF <- nwDF %>%
      dplyr::mutate(

        agb_xer_kg = dplyr::case_when(
          .data$growthForm == "xerophyllum" & !is.na(.data$basalStemDiameter) & !is.na(.data$meanLeafLength) ~
            round((18.873 + (0.0280 * ((.data$basalStemDiameter^2) * .data$meanLeafLength))) / 1000, digits = 2),
          TRUE ~ NA_real_
        ),

        #   Assign xerophyllum allometry source
        source = dplyr::replace_when(.data$source,
                                     !is.na(.data$agb_xer_kg) ~ "Gholz_etal_1979"),

        #   Update 'agb_kg' with xerophyllum biomass
        agb_kg = dplyr::replace_when(.data$agb_kg,
                                     !is.na(.data$agb_xer_kg) ~ .data$agb_xer_kg)
        ) %>%

      #   Remove 'agb_xer_kg' column
      dplyr::select(-"agb_xer_kg")

  } # End 'growthFormSubset' conditional



  ### Small-palm biomass: Estimate biomass for shrub-type and tree-type individuals ####

  if (growthFormSubset == "all") {

    nwDF <- nwDF %>%
      dplyr::mutate(

        agb_spm_kg = dplyr::case_when(

          #   Shrub-type small palms: Use equation from Abrahamson 2023 for SERE2; also apply this equation to other shrub-type palm species
          .data$taxonID %in% c("SERE2", "SAMI8", "SAET", "SABAL", "LEMO5") & !is.na(.data$leafNumber) &
            !is.na(.data$ninetyCrownDiameter) & .data$ninetyCrownDiameter > 0 ~
            round(exp(0.637 * log(.data$ninetyCrownDiameter * 100) + 2.3 * log(.data$leafNumber) + 0.254) / 1000, digits = 2),

          #   Tree-type small palms: Estimate assuming cylinder shape and assuming 0.45 g/cm3 wood density, which is the mean value for Oil Palm published by Geply etal 2011, Continental J. Applied Sciences
          .data$taxonID %in% c("BUTIA", "COBA3") & !is.na(.data$basalStemDiameter) & !is.na(.data$stemLength) ~
            round(pi * (.data$basalStemDiameter/2)^2 * .data$stemLength * 100 * 0.45/1000, digits = 2),

          TRUE ~ NA_real_
        ),

        #   Assign small palm allometry source
        #--> Not updated for small tree-type palms because no allometries found for short tree-type palms
        source = dplyr::replace_when(
          .data$source,
          .data$taxonID %in% c("SERE2", "SAMI8", "SAET", "SABAL", "LEMO5") & !is.na(.data$agb_spm_kg) ~ "Abrahamson_2023"
        ),

        #   Update 'agb_kg' with small palm biomass
        agb_kg = dplyr::replace_when(.data$agb_kg,
                                     !is.na(.data$agb_spm_kg) ~ .data$agb_spm_kg)
        ) %>%

      #   Remove 'agb_spm_kg' column
      dplyr::select(-"agb_spm_kg")

  } # End 'growthFormSubset' conditional



  ### Palm tree biomass: Estimate assuming cylinder shape ####
  #--> Allometry "source" not updated for tree-type palms because no allometries found for tree-type palms
  #--> Assume 0.45 g/cm3 wood density, which is the mean value for Oil Palm published by Geply etal 2011, Continental J. Applied Sciences

  nwDF <- nwDF %>%
    dplyr::mutate(

      agb_ptr_kg = dplyr::case_when(

        #   stemDiameter and stemLength present
        .data$taxonID %in% c("BUTIA", "COBA3") & !is.na(.data$stemDiameter) & !is.na(.data$stemLength) ~
          round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 * 0.45/1000, digits = 2),

        TRUE ~ NA_real_
      ),

      #   Update 'agb_kg' with palm tree biomass
      agb_kg = dplyr::replace_when(.data$agb_kg,
                                   !is.na(.data$agb_ptr_kg) ~ .data$agb_ptr_kg)
      ) %>%

    #   Remove 'agb_ptr_kg' column
    dplyr::select(-"agb_ptr_kg")



  ### Tree fern biomass: Estimate assuming cylinder shape ####
  #--> Approach used by Asner, GP, RF Hughes, J Mascaro, AL Uowolo, DE Knapp, J Jacobson, T Kennedy-Bowdoin, JK Clark. 2011. High-resolution carbon mapping on the million-hectare Island of Hawaii. Frontiers in Ecology and the Environment.
  #--> Wood densities come from Asner etal 2011

  nwDF <- nwDF %>%
    dplyr::mutate(

      agb_tfn_kg = dplyr::case_when(

        #   Biomass for Cibotium tree ferns with stemDiameter
        grepl("Cibotium", .data$scientificName) & !is.na(.data$stemDiameter) & !is.na(.data$stemLength) ~
          round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 * 0.22/1000, digits = 2),

        #   Biomass for Sadleria tree ferns with stemDiameter
        grepl("Sadleria", .data$scientificName) & !is.na(.data$stemDiameter) & !is.na(.data$stemLength) ~
          round(pi * (.data$stemDiameter/2)^2 * .data$stemLength * 100 * 0.5/1000, digits = 2),

        TRUE ~ NA_real_
      ),

      #   Assign tree fern allometry source
      source = dplyr::replace_when(.data$source,
                                   !is.na(.data$agb_tfn_kg) ~ "Asner_etal_2011"),

      #   Update 'agb_kg' with tree fern biomass
      agb_kg = dplyr::replace_when(.data$agb_kg,
                                   !is.na(.data$agb_tfn_kg) ~ .data$agb_tfn_kg)
    ) %>%

    #   Remove 'agb_tfn_kg' column
    dplyr::select(-"agb_tfn_kg")



  ### Small tree fern biomass: Estimate assuming cylinder shape ####
  #--> Approach used by Asner, GP, RF Hughes, J Mascaro, AL Uowolo, DE Knapp, J Jacobson, T Kennedy-Bowdoin, JK Clark. 2011. High-resolution carbon mapping on the million-hectare Island of Hawaii. Frontiers in Ecology and the Environment.
  #--> Wood densities come from Asner etal 2011

  if (growthFormSubset == "all") {

    nwDF <- nwDF %>%
      dplyr::mutate(

        agb_stf_kg = dplyr::case_when(

          #   Biomass for Cibotium tree ferns with basalStemDiameter
          grepl("Cibotium", .data$scientificName) & !is.na(.data$basalStemDiameter) & !is.na(.data$stemLength) ~
            round(pi * (.data$basalStemDiameter/2)^2 * .data$stemLength * 100 * 0.22/1000, digits = 2),

          #   Biomass for Sadleria tree ferns with basalStemDiameter
          grepl("Sadleria", .data$scientificName) & !is.na(.data$basalStemDiameter) & !is.na(.data$stemLength) ~
            round(pi * (.data$basalStemDiameter/2)^2 * .data$stemLength * 100 * 0.5/1000, digits = 2),

          TRUE ~ NA_real_
        ),

        #   Assign small tree fern allometry source
        source = dplyr::replace_when(.data$source,
                                     !is.na(.data$agb_stf_kg) ~ "Asner_etal_2011"),

        #   Update 'agb_kg' with small tree fern biomass
        agb_kg = dplyr::replace_when(.data$agb_kg,
                                     !is.na(.data$agb_stf_kg) ~ .data$agb_stf_kg)
      ) %>%

      #   Remove 'agb_stf_kg' column
      dplyr::select(-"agb_stf_kg")

  } # End 'growthFormSubset' conditional



  ### Source clean-up: Indicate "noAllometry" when biomass cannot be estimated ####
  nwDF <- nwDF %>%
    dplyr::mutate(

      source = dplyr::replace_when(.data$source,
                        is.na(.data$source) ~ "noAllometry")
    )



  ### Output ####
  return(nwDF)

} # End function
