#' priority_plots
#'
#' The "UniquePlotIDsSamplingModulesPriorityLists" table was obtained from GitHub NEON-OS_spatial_data repo in 2022 and then filtered and selected to relevant records and columns: 
#'                   dplyr::filter(str_detect(specificModule, "vst"))  %>% dplyr::select(c(plotID,specificModuleSamplingPriority, plotType) ) 
#' The specificModuleSamplingPriority field is used in the NEON productivity script to optionally filter only to plots with priority 1-5 (the plots that are most likely to have been sampled the year they were scheduled)

#' @format ## `priority_plots`
#' A data frame with 2,005 rows and 3 columns:
#' \describe{
#'   \item{plotID}{NEON plotID}
#'   \item{specificModuleSamplingPriority}{the sampling priority of a NEON plot, with lower numbers indicating higher priority}
#'   \item{plotType}{NEON plotType, either "tower" or "distributed"}
#'   ...
#' }
#' @source <https://github.com/NEONScience/NEON-OS-spatial-data/blob/main/TOS/archive/UniquePlotIDsSamplingModulesPriorityLists_20220309.xlsx>
"priority_plots"
