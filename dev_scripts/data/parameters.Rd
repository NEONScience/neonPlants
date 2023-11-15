#' parameters
#'
#' Chojnacky et al. 2014 Table 5 provides parameters for 35 different allometric equations

#' @format ## `parameters`
#' A data frame with 35 rows and 10 columns:
#' \describe{
#'   \item{allometryID}{allometryID name developed in Chojnacky et al. 2014}
#'   \item{Group}{Chojnacky group, either conifer, hardwood, or woodland}
#'   \item{subGroup}{Chojnacky description of allometry group"}
#'   \item{Median.specific.gravity}{specific gravity of wood of on green volume to dry-weight basis}
#'   \item{b0}{Chojnacky parameter b0}
#'   \item{b1}{Chojnacky parameter b1}
#'   \item{Diameter}{type of diameter, either dbh or drc}
#'   \item{minDiameter}{minimum diameter for which parameters are applicable}
#'   \item{maxDiameter}{maximum diameter for which parameters are applicable}
#'   \item{R2.statistic}{R2 fit for Chojnacky equation}
#'   ...
#' }
#' @source Chojnacky et al. 2014 Table 5   doi:10.1093/forestry/cpt053
"parameters"
