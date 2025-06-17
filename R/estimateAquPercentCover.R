#' @title Estimate NEON aquatic plant, bryophyte, lichen, and macroalgae percent cover in wadeable streams
#' 
#' @author Madaline Ritter \email{ritterm1@battelleecology.org} \cr
#' 
#' @description Data inputs are NEON Aquatic Plant, Bryophyte, Lichen, and Macroalgae Point Counts in Wadeable Streams (DP1.20072.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations. The calculateAquPercentCover() function aggregates the occurrence data from the Aquatic Plant Point Count data product to return estimates of percent cover at the scale of each transect.
#' 
#' @details Input data may be provided either as a list generated from the neonUtilities::laodByProduct() function or as individual tables. However, if both list and table inputs are provided at the same time the function will error. 
#' 
#' Percent cover is calculated using the equation from Bowden et al. 2006: 
#' 
#' \deqn{percentCover_i = (N_i / N_t) * 100}
#' 
#' Where:
#' 
#' - \eqn{N_i} is the number of observed points in a transect that match class type “i” (i.e., a particular taxonID or substrate)
#' - \eqn{N_t} is the total number of points observed in the transect
#'
#' Note: This calculation can generate percent cover values >100% if there is vertical stacking of plants.
#'
#' @param inputDataList A list object comprised of Aquatic Plant, Bryophyte, Lichen, and Macroalgae Point Count tables (DP1.20072.001) downloaded using the neonUtilities::loadByProduct() function. If list input is provided, the table input arguments must all be NA; similarly, if list input is missing, table inputs must be provided. [list]
#'
#' @param inputPoint The 'apc_pointTransect' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputPerTax The 'apc_perTaxon' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputTaxProc The 'apc_taxonomyProcessed' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @param inputMorph The 'apc_morphospecies' table for the site x month combination(s) of interest (defaults to NA). If table input is provided, the 'inputDataList' argument must be missing. [data.frame]
#' 
#' @return A data frame that estimates percent cover for each species observed on a given aquatic plant transect. Description of columns can be found in the Data Product User Guide.
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @examples
#' \dontrun{
#' #   Obtain NEON Aquatic Plant Point Count data
#' apc <- neonUtilities::loadByProduct(
#' dpID = "DP1.20072.001",
#' site = "all",
#' startdate = "2018-07",
#' enddate = "2018-08",
#' tabl = "all",
#' check.size = FALSE
#' )
#' 
#' #   Calculate percent cover for downloaded data
#' df <- neonPlants::estimateAquPercentCover(
#' inputDataList = apc,
#' inputPoint = NA,
#' inputPerTax = NA,
#' inputTaxProc = NA,
#' inputMorph = NA
#' )
#' }
#' @export estimateAquPercentCover 




estimateAquPercentCover <- function(inputDataList,
                                inputPoint = NA,
                                inputPerTax = NA,
                                inputTaxProc = NA,
                                inputMorph = NA) {
  
  ### Join taxonomy data ####
  
  if(!missing(inputDataList)){
    
    joinPointCounts <- neonPlants::joinAquPointCount(inputDataList = inputDataList)
    # joinPointCounts <- joinAquPointCount(inputDataList = apc)
    
  
  } else {
    
    joinPointCounts <- neonPlants::joinAquPointCount(inputPoint = inputPoint,
                                  inputPerTax = inputPerTax,
                                  inputTaxProc = inputTaxProc,
                                  inputMorph = inputMorph)
  }
  
  
  ### Calculate Percent Cover ####
  
  #  filter to only observed points:
  # joinPointCounts <- joinPointCounts %>% filter(targetTaxaPresent == "Y")
  
  # Calculate percent cover of plants/macroalgae/etc
  # percent_cover <- joinPointCounts %>%
  #   dplyr::group_by(.data$namedLocation, .data$collectDate) %>%
  #   dplyr::mutate(total_points = dplyr::n()) %>%
  #   # dplyr::filter(targetTaxaPresent == 'Y') %>% 
  #   dplyr::group_by(.data$namedLocation, .data$collectDate, .data$acceptedTaxonID, .data$scientificName) %>%
  #   dplyr::reframe(
  #     tax_count = dplyr::n(),
  #     total_points = dplyr::first(total_points),
  #     percent_cover = round( (100 * tax_count / total_points), 2)
  #   )
  

  # Calculate total points per group (siteID + collectDate)
  total_points_df <- joinPointCounts %>%
    dplyr::group_by(.data$siteID, .data$collectDate) %>%
    dplyr::summarise(total_points = dplyr::n(), .groups = "drop")
  
  # Percent cover by substrate
  cover_substrate <- joinPointCounts %>%
    dplyr::filter(.data$targetTaxaPresent == "N") %>%
    dplyr::group_by(.data$siteID, .data$collectDate, .data$namedLocation, .data$substrate) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(.data$total_points_df, by = c("siteID", "collectDate")) %>%
    dplyr::mutate(
      percent_cover = round(100 * .data$count / .data$total_points, 2),
      type = "substrate",
      substrateOrTaxonID = .data$substrate
    ) %>%
    dplyr::select("siteID", "collectDate", "namedLocation", "substrateOrTaxonID", "percent_cover", "type")
  
  # Percent cover by taxon
  cover_taxon <- joinPointCounts %>%
    dplyr::filter(.data$targetTaxaPresent == "Y") %>%
    dplyr::group_by(.data$siteID, .data$collectDate, .data$namedLocation, .data$acceptedTaxonID, .data$scientificName) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(.data$total_points_df, by = c("siteID", "collectDate")) %>%
    dplyr::mutate(
      percent_cover = round(100 * .data$count / .data$total_points, 2),
      type = "taxon",
      substrateOrTaxonID = .data$acceptedTaxonID
    ) %>%
    dplyr::select("siteID", "collectDate", "namedLocation", "substrateOrTaxonID", "scientificName", "percent_cover", "type")
  
  # Combine results
  cover_substrate$scientificName <- NA_character_
  percent_cover <- dplyr::bind_rows(cover_substrate, cover_taxon)
  
  
  ### Plot Percent Cover by Site/Date ####
  
  # # Create a unique ID for each siteID + collectDate
  # percent_cover$plotID <- paste(percent_cover$siteID, substr(percent_cover$collectDate, 1, 10), sep = "_")
  # 
  # # Generate plots
  # plot_ids <- unique(percent_cover$plotID)
  # 
  # plot_grid <- function(plot_id) {
  #   ggplot(subset(percent_cover, plotID == plot_id),
  #          aes(x = namedLocation, y = percent_cover, fill = substrateOrTaxonID)) +
  #     geom_bar(stat = "identity", position = "stack") +
  #     labs(
  #       title = paste("Percent Cover for", gsub("_", " on ", plot_id)),
  #       x = "Named Location",
  #       y = "Percent Cover",
  #       fill = "Substrate/Taxon"
  #     ) +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # }
  # 
  # plot_list <- lapply(plot_ids, plot_grid)
  # names(plot_list) <- plot_ids
  # 
  # # Print all plots
  # for (i in seq_along(plot_list)) {
  #   if (!is.null(plot_list[[i]])) {
  #     print(plot_list[[i]])
  #   }
  # }
  
  return(percent_cover)
  
} #function closer

  