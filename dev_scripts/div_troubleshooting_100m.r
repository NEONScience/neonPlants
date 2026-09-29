# # compare neonPlantEcology and neonPlan stacking output
library(tidyverse)

# install.packages("neonPlantEcology")
library("neonPlantEcology")

# load neonPlants
devtools::load_all()

# reads in data object D14
data("D14")

# stack using npe
d_npe <- npe_longform(D14, scale = "100m", timescale = "subannual")

# stack using np
d_np <- neonPlants::stackPlantPresence(D14, totalSampledAreaFilter = 100) %>%
  filter(!is.na(taxonID))


# summaries
d_np_sum <- d_np %>%
  group_by(siteID, plotID, subplotID, eventID) %>%
  summarize(
    rich_np = taxonID %>% unique() %>% length() %>% sum(),
    n_obs_np = n())

d_npe_sum <- d_npe %>%
  rename(siteID=site) %>%
  mutate(subplotID = paste0(subplotID,"_100")) %>%
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
      paste0(subplotID,"_100") == my_subplotID,
      eventID == my_eventID) %>%
    as.data.frame()

  # truth
  d14_10_100_recs <- D14$div_10m2Data100m2Data %>%
    filter(
      plotID == my_plotID,
      grepl(
        gsub("100","",my_subplotID),
        subplotID),
      eventID == my_eventID) %>%
    as.data.frame()

  d14_1_recs <- D14$div_1m2Data %>%
    filter(
      plotID == my_plotID,
      grepl(
        gsub("100","",my_subplotID),
        subplotID),
      eventID == my_eventID,
      !is.na(taxonID)) %>%
    as.data.frame()

  d_diffs[i,"tax_1m"] <- d14_1_recs$taxonID %>%
    unique() %>% paste(collapse = "|")
  d_diffs[i,"tax_10_100m"] <- d14_10_100_recs$taxonID %>%
    unique() %>% paste(collapse = "|")
  d_diffs[i,"tax_np_100m"] <- np_recs$taxonID %>%
    unique() %>% paste(collapse = "|")
  d_diffs[i,"tax_npe_100m"] <- npe_recs$taxonID %>%
    unique() %>% paste(collapse = "|")
  d_diffs[i,"np_subplotID"] <- np_recs$subplotID %>%
    unique() %>% paste(collapse = "|")
  d_diffs[i,"npe_subplotID"] <- npe_recs$subplotID %>%
    unique() %>% paste(collapse = "|")

}

write_csv(d_diffs, file = "d_diffs_100m.csv")

