# # compare neonPlantEcology and neonPlan stacking output
library(tidyverse)

# install.packages("neonPlantEcology")
library("neonPlantEcology")

# load neonPlants
devtools::load_all()

# reads in data object D14
data("D14")

# stack using npe
d14_npe <- npe_longform(D14, scale = "10m", timescale = "subannual")

# stack using np
d14_np <- neonPlants::stackPlantPresence(D14, totalSampledAreaFilter = 10) %>%
  filter(!is.na(taxonID))

# summaries
d14_np_sum <- d14_np %>%
  group_by(siteID, plotID, subplotID, eventID) %>%
  summarize(
    rich_np = taxonID %>% unique() %>% length() %>% sum(),
    n_obs_np = n())

d14_npe_sum <- d14_npe %>%
  rename(siteID=site) %>%
  mutate(subplotID = gsub("_","_10_",subplotID)) %>%
  group_by(siteID, plotID, subplotID, eventID) %>%
  summarize(
    rich_npe = taxonID %>% unique() %>% length() %>% sum(),
    n_obs_npe = n())

# compare summaries
d14_compare <- d14_np_sum %>%
  full_join(d14_npe_sum) %>%
  rowwise() %>%
  mutate(
    rich_var = var(c(rich_np, rich_npe),na.rm = TRUE),
    n_obs_var = var(c(n_obs_np, n_obs_npe),na.rm = TRUE))

d14_compare_diff_var <- d14_compare %>%
  filter(rich_var > 0)

d14_compare_diff_obs <- d14_compare %>%
  filter(n_obs_var > 0)

d14_compare_NAs <- d14_compare %>%
  rowwise %>%
  filter(anyNA(c(rich_np,n_obs_np,rich_npe,n_obs_npe)))

# plots - are the lists the same?
plot_npe <- d14_npe$plotID %>% unique()
plot_np <- d14_np$plotID %>% unique()
setequal(plot_npe, plot_np)
dplyr::setdiff(plot_npe, plot_np)
dplyr::setdiff(plot_np, plot_npe)

# tax list
tax_npe <- d14_npe$taxonID %>% unique()
tax_np <- d14_np$taxonID %>% unique()
setequal(tax_npe, tax_np)

tax_prob_list <- dplyr::setdiff(tax_npe, tax_np)
dplyr::setdiff(tax_np, tax_npe)

#######################################
# row 1 in compare --
my_plotID = "JORN_003"
my_subplotID = "32_10_2"
my_eventID = "JORN.1.2022"

d14_np %>%
  filter(
    plotID == my_plotID,
    subplotID == my_subplotID,
    eventID == my_eventID) %>%
  as.data.frame()

d14_npe %>%
  filter(
    plotID == my_plotID,
    subplotID == gsub("_10_","_",my_subplotID),
    eventID == my_eventID) %>%
  as.data.frame()

# truth
D14$div_10m2Data100m2Data %>%
  filter(
    plotID == my_plotID,
    subplotID == my_subplotID,
    eventID == my_eventID) %>%
  as.data.frame()

D14$div_1m2Data %>%
  filter(
    plotID == my_plotID,
    subplotID == gsub("_10_","_1_",my_subplotID),
    eventID == my_eventID,
    !is.na(taxonID)) %>%
  as.data.frame()

############################################################################
my_plotID = "JORN_016"
my_subplotID = "31_10_1"
my_eventID = "JORN.1.2022"

d14_np %>%
  filter(
    plotID == my_plotID,
    subplotID == my_subplotID,
    eventID == my_eventID) %>%
  as.data.frame()

d14_npe %>%
  filter(
    plotID == my_plotID,
    subplotID == gsub("_10_","_",my_subplotID),
    eventID == my_eventID) %>%
  as.data.frame()

# truth
D14$div_10m2Data100m2Data %>%
  filter(
    plotID == my_plotID,
    subplotID == my_subplotID,
    eventID == my_eventID) %>%
  as.data.frame()

D14$div_1m2Data %>%
  filter(
    plotID == my_plotID,
    subplotID == gsub("_10_","_1_",my_subplotID),
    eventID == my_eventID,
    !is.na(taxonID)) %>%
  as.data.frame()
