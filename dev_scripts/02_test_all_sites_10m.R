# # compare neonPlantEcology and neonPlan stacking output
library(tidyverse)

# install.packages("neonPlantEcology")
library("neonPlantEcology")

# load neonPlants
devtools::load_all()

# set our dir
out_dir <- path.expand("dev_scripts/compare_results/")

# get in dir and filenames
data_dir <- path.expand("dev_scripts/RELEASE-2024/")
data_filename_list <- list.files(data_dir)

for(j in 1:length(data_filename_list)){

  # reads in data object j_data_list
  j_data_filename <- data_filename_list[j]
  j_data_list <- readRDS(paste0(data_dir,j_data_filename))

  # stack using npe
  d_npe <- npe_longform(j_data_list, scale = "10m", timescale = "subannual")

  # stack using np
  d_np <- neonPlants::stackPlantPresence(j_data_list, totalSampledAreaFilter = 10) %>%
    filter(!is.na(taxonID))


  # summaries
  d_np_sum <- d_np %>%
    group_by(siteID, plotID, subplotID, eventID) %>%
    summarize(
      rich_np = taxonID %>% unique() %>% length() %>% sum(),
      n_obs_np = n())

  d_npe_sum <- d_npe %>%
    rename(siteID=site) %>%
    mutate(subplotID = gsub("_","_10_",subplotID)) %>%
    group_by(siteID, plotID, subplotID, eventID) %>%
    summarize(
      rich_npe = taxonID %>% unique() %>% length() %>% sum(),
      n_obs_npe = n())

  # compare summaries
  d_compare <- d_np_sum %>%
    full_join(d_npe_sum) %>%
    rowwise() %>%
    mutate(
      rich_var = var(c(rich_np, rich_npe),na.rm = TRUE),
      n_obs_var = var(c(n_obs_np, n_obs_npe),na.rm = TRUE)) %>%
    as.data.frame()

  # filter to recs with discrepancies
  d_diffs <- d_compare %>%
    filter(rich_var > 0)


  if(nrow(d_diffs) == 0){
    message(j_data_filename, ": ",j,"/",length(data_filename_list))
    next}

  for(i in 1:nrow(d_diffs)){
    my_plotID = d_diffs$plotID[i] # "JORN_016"
    my_subplotID = d_diffs$subplotID[i] #"31_10_1"
    my_eventID = d_diffs$eventID[i] #"JORN.1.2022"

    np_recs <- d_np %>%
      filter(
        plotID == my_plotID,
        subplotID == my_subplotID,
        eventID == my_eventID) %>%
      as.data.frame()

    npe_recs <- d_npe %>%
      filter(
        plotID == my_plotID,
        subplotID == gsub("_10_","_",my_subplotID),
        eventID == my_eventID) %>%
      as.data.frame()

    # truth
    j_data_list_10_recs <- j_data_list$div_10m2Data100m2Data %>%
      filter(
        plotID == my_plotID,
        subplotID == my_subplotID,
        eventID == my_eventID) %>%
      as.data.frame()

    j_data_list_1_recs <- j_data_list$div_1m2Data %>%
      filter(
        plotID == my_plotID,
        subplotID == gsub("_10_","_1_",my_subplotID),
        eventID == my_eventID,
        !is.na(taxonID)) %>%
      as.data.frame()

    d_diffs[i,"tax_1m"] <- j_data_list_1_recs$taxonID %>%
      unique() %>% paste(collapse = "|")
    d_diffs[i,"tax_10m"] <- j_data_list_10_recs$taxonID %>%
      unique() %>% paste(collapse = "|")
    d_diffs[i,"tax_np_10m"] <- np_recs$taxonID %>%
      unique() %>% paste(collapse = "|")
    d_diffs[i,"tax_npe_10m"] <- npe_recs$taxonID %>%
      unique() %>% paste(collapse = "|")
    d_diffs[i,"np_subplotID"] <- np_recs$subplotID %>%
      unique() %>% paste(collapse = "|")
    d_diffs[i,"npe_subplotID"] <- npe_recs$subplotID %>%
      unique() %>% paste(collapse = "|")

  }

  j_out_filename <- paste0(out_dir,"d_diffs_10m_",gsub("\\.RDS","",j_data_filename),".csv")
  write_csv(d_diffs, file = j_out_filename)

  message(j_data_filename, ": ",j,"/",length(data_filename_list))
}
