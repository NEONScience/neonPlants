#' taxon_fields
#'
#' This data frame provides the necessary fields to assign a plant taxonID to an appropriate Chojnacky et al. 2015 allometry_ID

#' @format ## `taxon_fields`
#' A data frame with 447 rows and 25 columns:
#' \describe{
#'   \item{taxonID}{plant taxonomic ID}
#'   \item{spg_gcm3}{specific gravity for taxonID}
#'   \item{density_source}{source of specific gravity data}
#'   \item{decid_vs_ever}{is taxonID typically deciduous or evergreen}
#'   \item{decid_vs_ever_source}{source of decidious vs evergreen designation}
#'   \item{woodland_vs_forest}{is taxonID typically found in a woodland or closed forest setting}
#'   \item{woodland_vs_forest_source}{source of woodland vs forest designation}
#'   ...
#' }
#' @source Assembled by NEON staff, with source indicated in density_source, decid_vs_ever_source, and woodland_vs_forest_source fields
"taxon_fields"
