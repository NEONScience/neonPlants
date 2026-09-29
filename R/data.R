#' Dataset Title: Chojnacky parameters
#'
#' Allometric parameters for 35 different woody biomass equations (from Chojnacky et al. 2014, Table 5)
#'
#' @format A data frame with 35 rows and 10 variables:
#' \describe{
#'   \item{allometry_ID}{Allometry ID name developed in Chojnacky et al. 2014}
#'   \item{Group}{High-level group for which allometry parameters are applicable: either conifer, hardwood, or woodland}
#'   \item{subgroup}{Sub-group for which allometry parameters are applicable - e.g., family, wood density range, etc."}
#'   \item{Median.specific.gravity}{specific gravity of wood on green volume to dry-weight basis}
#'   \item{b0}{Chojnacky parameter b0}
#'   \item{b1}{Chojnacky parameter b1}
#'   \item{Diameter}{Type of diameter required for allometric equation, either dbh or drc}
#'   \item{minDiameter}{Minimum diameter for which parameters are applicable}
#'   \item{maxDiameter}{Maximum diameter for which parameters are applicable}
#'   \item{R2.statistic}{R2 fit for Chojnacky equation}
#' }
#' @source \url{https://doi.org/10.1093/forestry/cpt053}
"parameters"

#' Dataset Title: Range and nativity status by taxonID
#'
#' USDA Plants temperate vs tropical status and introduced vs native status for woody taxa
#'
#' @format A data frame with 9326 rows and 3 variables:
#' \describe{
#'   \item{taxonID}{Accepted taxonID code associated with binomial latin species epithet}
#'   \item{nativeStatus}{Indicator of native vs introduced status in North America}
#'   \item{tropical}{Indicator of tropical vs temperate range in North America}
#' }
#' @source \url{https://plants.sc.egov.usda.gov/}
"plantIntTrop"

#' Dataset Title: NEON plot sampling priority
#'
#' The spatial sampling priority for all NEON terrestrial plots
#'
#' @format A data frame with 2005 rows and 3 variables:
#' \describe{
#'   \item{plotID}{The NEON unique plot identifier}
#'   \item{specificModuleSamplingPriority}{The ranked priority in which NEON plots are sampled}
#'   \item{plotType}{The NEON plot type, either Distributed or Tower}
#' }
#' @source \url{https://github.com/NEONScience/NEON-OS-spatial-data/tree/main/TOS/data}
"priority_plots"

#' Dataset Title: Wood traits for NEON taxonIDs
#'
#' Wood density, deciduous vs evergreen habit, and habitat type for NEON taxonIDs
#'
#' @format A data frame with 405 rows and 7 variables:
#' \describe{
#'   \item{taxonID}{Accepted taxonID code associated with binomial latin species epithet}
#'   \item{spg_gcm3}{Wood specific gravity (g/cm3)}
#'   \item{density_source}{Wood specific gravity reference}
#'   \item{decid_vs_ever}{Deciduous vs evergreen growth habit}
#'   \item{decid_vs_ever_source}{Growth habit reference}
#'   \item{woodland_vs_forest}{Typical habitat associated with taxonID}
#'   \item{woodland_vs_forest_source}{Habitat reference}
#' }
#' @source Internal NEON dataset
"taxon_fields"

#' Dataset Title: Function output variables, units, and definitions
#'
#' List of novel variables created by neonPlants functions; does not include variables already defined for existing data products
#'
#' @format A data frame with 215 rows and 5 columns:
#' \describe{
#'   \item{functionName}{Name of neonPlants function}
#'   \item{table}{Name of function output table}
#'   \item{field}{Name of column in output table}
#'   \item{units}{Column units}
#'   \item{description}{Description of column in output table}
#' }
#' @source Internal NEON dataset
"variables"
