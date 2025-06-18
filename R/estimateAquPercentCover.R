#' @title Estimate NEON aquatic plant, bryophyte, lichen, and macroalgae percent cover in wadeable streams
#' 
#' @author Madaline Ritter \email{ritterm1@battelleecology.org} \cr
#' 
#' @description Data inputs are NEON Aquatic Plant, Bryophyte, Lichen, and Macroalgae Point Counts in Wadeable Streams (DP1.20072.001) in list format retrieved using the neonUtilities::loadByProduct() function (preferred), data tables downloaded from the NEON Data Portal, or input data tables with an equivalent structure and representing the same site x month combinations. The estimateAquPercentCover() function joins taxonomy information across point count tables and aggregates occurrence data to estimate percent cover at the transect level.
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
#' @param barPlots If TRUE, will produce a list of plots, one for each site/date in the data provided.
#' 
#' @return Two tables are produced containing point count summary data. The first "percentCover" table contains estimated percent cover for each observed species and/or substrate class on aquatic plant transects. This table includes a 'type' column indicating whether the estimate corresponds to a taxonID or a substrate class, and a 'substrateOrTaxonID' column which provides the corresponding identifier. 
#' 
#' The second "transectMetrics" table contains summary information including the length, habitatType, and total number of points sampled at each transect. 
#' 
#' If barPlots = TRUE, a list containing plots of the data will also be produced. 
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
#' inputMorph = NA,
#' barPlots = F
#' )
#' }
#' @export estimateAquPercentCover 




estimateAquPercentCover <- function(inputDataList,
                                    inputPoint = NA,
                                    inputPerTax = NA,
                                    inputTaxProc = NA,
                                    inputMorph = NA,
                                    barPlots = FALSE) {
  
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
  
  ### Remove SI Records ####
  joinPointCounts <- joinPointCounts %>% dplyr::filter(is.na(.data$samplingImpractical))
  
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
  

  # Calculate total number of sampling points per group (siteID + collectDate)
  # total_points_df <- joinPointCounts %>%
  #   dplyr::group_by(.data$siteID, .data$collectDate) %>%
  #   dplyr::summarise(total_points = dplyr::n(), .groups = "drop")
  
  total_points_df <- joinPointCounts %>%
    dplyr::distinct(.data$siteID, .data$namedLocation, .data$collectDate, .data$pointNumber) %>%
    dplyr::group_by(.data$siteID, .data$namedLocation, .data$collectDate) %>%
    dplyr::summarise(total_points = dplyr::n(), .groups = "drop")
  
  
  # Percent cover by substrate
  cover_substrate <- joinPointCounts %>%
    dplyr::filter(.data$targetTaxaPresent == "N") %>%
    dplyr::group_by(.data$siteID, .data$collectDate, .data$namedLocation, .data$substrate) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(total_points_df, by = c("siteID", "namedLocation", "collectDate")) %>%
    dplyr::mutate(
      percent_cover = round(100 * .data$count / .data$total_points, 2),
      type = "substrate",
      substrateOrTaxonID = .data$substrate
    ) %>%
    dplyr::select("siteID", "collectDate", "namedLocation", "type", "substrateOrTaxonID", "percent_cover")
  
  # Percent cover by taxon
  cover_taxon <- joinPointCounts %>%
    dplyr::filter(.data$targetTaxaPresent == "Y") %>%
    dplyr::group_by(.data$siteID, .data$collectDate, .data$namedLocation, .data$acceptedTaxonID) %>% #, .data$scientificName
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(total_points_df, by = c("siteID", "namedLocation", "collectDate")) %>%
    dplyr::mutate(
      percent_cover = round(100 * .data$count / .data$total_points, 2),
      type = "taxon",
      substrateOrTaxonID = .data$acceptedTaxonID
    ) %>%
    dplyr::select("siteID", "collectDate", "namedLocation", "type", "substrateOrTaxonID",  "percent_cover") #"scientificName",
  
  # Combine results
  # cover_substrate$scientificName <- NA_character_
  percent_cover <- dplyr::bind_rows(cover_substrate, cover_taxon) %>% 
    dplyr::arrange(.data$collectDate, .data$namedLocation)
  
  
  
  ### Calculate Transect metrics ####
  transect_metrics <- joinPointCounts %>%
    dplyr::distinct(.data$domainID, .data$siteID, .data$namedLocation, .data$collectDate, .data$boutNumber, .data$habitatType, .data$pointNumber, .data$transectDistance) %>%
    dplyr::group_by(.data$domainID, .data$siteID, .data$namedLocation, .data$collectDate, .data$boutNumber, .data$habitatType) %>%
    dplyr::summarise(transectMax = max(.data$transectDistance),
                     transectMin = min(.data$transectDistance),
                     total_points = dplyr::n(),.groups = "drop") %>%
    dplyr::mutate(transectLength_m = .data$transectMax - .data$transectMin) %>%
    dplyr::select("domainID", "siteID", "namedLocation", "collectDate", "boutNumber", "habitatType", "transectLength_m", "total_points")
  
  
  returnList <- list(percentCover=percent_cover, transectMetrics=transect_metrics)
  
  
  ### Optionally Plot Percent Cover by Site/Date ####
  if(barPlots){
    
    # Create a unique ID for each siteID + collectDate
    percent_cover$boutID <- paste(percent_cover$siteID, substr(percent_cover$collectDate, 1, 10), sep = "_")

    plot_ids <- unique(percent_cover$boutID)
  
    # Create a consistent color palette
    
    # Separate substrate and taxon IDs
    substrate_ids <- unique(cover_substrate$substrateOrTaxonID)
    taxon_ids <- unique(cover_taxon$substrateOrTaxonID)
    
    # Assign greyscale colors to substrates
    greys <- grDevices::gray.colors(length(substrate_ids), start = 0.3, end = 0.8)
    
    # Assign colorful palette to taxa
    taxon_colors <- RColorBrewer::brewer.pal(min(length(taxon_ids), 8), "Set2")
    if (length(taxon_ids) > 8) {
      extra_colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)]
      taxon_colors <- c(taxon_colors, sample(extra_colors, length(taxon_ids) - 8))
    }
    
    # Combine into one palette
    color_palette <- c(stats::setNames(greys, substrate_ids), stats::setNames(taxon_colors, taxon_ids))
    
    
    # Plotting function
    plot_grid <- function(plot_id) {
      ggplot2::ggplot(subset(percent_cover, boutID == plot_id),
             ggplot2::aes(x = namedLocation, y = percent_cover, fill = substrateOrTaxonID)) +
        ggplot2::geom_bar(stat = "identity", position = "stack") +
        ggplot2::scale_fill_manual(values = color_palette) +  # Apply consistent colors
        labs(
          # title = paste("Percent Cover for", gsub("_", " on ", plot_id)),
          title = gsub(" ", " on ", plot_id),
          # x = "Named Location",
          y = "Percent Cover",
          fill = "Substrate/Taxon"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
    
    plot_list <- lapply(plot_ids, plot_grid)
    names(plot_list) <- plot_ids
    
    # Bind df and plots
    returnList$plot_list <- plot_list

    # Print all plots
    # for (i in seq_along(df$plot_list)) {
    #   if (!is.null(df$plot_list[[i]])) {
    #     print(df$plot_list[[i]])
    #   }
    # }
    
  }
  
  return(returnList)
  
} #function closer

  