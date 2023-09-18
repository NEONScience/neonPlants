#' plant_taxa_NEON
#'
#' This data frame indicates the family, genus, species and synonymous taxonID(s) of a taxonID

#' @format ## `plant_taxa_NEON`
#' A data frame with 50,955 rows and 5 columns:
#' \describe{
#'   \item{taxonID}{plant taxonomic ID used by NEON (typically following USDA PLANTS Symbol)}
#'   \item{acceptedTaxonID}{accepted taxonID shared by synonymous taxonIDs}
#'   \item{family}{plant family that taxonID belongs to}
#'   \item{genus}{genus of the taxonID}
#'   \item{specificEpithet}{specific epithet of the taxonID}
#'   ...
#' }
#' @source NEON internal taxon download for taxon "PLANT"
"plant_taxa_NEON"
