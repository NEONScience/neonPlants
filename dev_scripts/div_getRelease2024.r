# load packages
library(tidyverse)
library(neonUtilities)

install.packages("neonPlantEcology")
library(neonPlantEcology)

# load local dev version of neonPlants
devtools::load_all()

# get info
div_info <- neonUtilities::getProductInfo(
  dpID = "DP1.10058.001",
  token = Sys.getenv("NEON_TOKEN"))

site_list <- div_info$siteCodes$siteCode

for(i in 1:length(site_list)){
  i_site <- site_list[i]
  i_data_list <- list()
  try({
    i_data_list <- neonUtilities::loadByProduct(
      "DP1.10058.001",
      site = i_site,
      release = "RELEASE-2024",
      check.size = FALSE,
      token = Sys.getenv("NEON_TOKEN"))
  })

  if(length(i_data_list) > 0){
    i_filename <- paste0("dev_scripts/RELEASE-2024/div_",i_site,".RDS")
    saveRDS(i_data_list,file = i_filename)
  }
  message(i_site, ":",i,"/",length(site_list))
}
