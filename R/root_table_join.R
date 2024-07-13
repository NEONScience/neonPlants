###################################################################################################
#' @title Join NEON Plant Belowground Biomass Root Mass and Root Chemistry Data Into a Single Table

#' @author Courtney Meier \email{cmeier@battelleecology.org} \cr

#' @description Using NEON Plant Belowground Biomass data (DP1.10067.001) from the NEON Data
#' Portal, or input data tables with an equivalent structure, this function uses the bbc_rootmass,
#' bbc_chemistryPooling, and bbc_rootChemistry tables to generate a single table that contains
#' both mass and chemistry data for each sizeCategory within a given sampleID.
#'
#' @details For table joining to be successful, all input data frames must contain data from the
#' same site x month combination(s).
#'
#' @param input_mass The 'bbc_rootmass' table for the site x month combination(s) of interest.
#' [data.frame]
#' @param input_pool The 'bbc_chemistryPooling' table for the site x month combination(s) of
#' interest. [data.frame]
#' @param input_chem The 'bbc_rootChemistry' table for the site x month combination(s) of
#' interest. [data.frame]

#' @return A table containing both root mass and root chemistry data in the same row for
#' different root sizeCategories within a sampleID.

#' @examples
#' \dontrun{
#'
#' }

#' @export root_table_join

###################################################################################################

root_table_join <- function(input_mass,
                            input_pool,
                            input_chem) {

  ### Verify user-supplied input_mass table contains correct data


}
