library(neonUtilities)
library(dplyr)

VstDat_multiple <- loadByProduct(
  dpID ="DP1.10098.001",
  site = c("SJER","PUUM","JERC","SRER","WREF","BLAN","ONAQ","KONZ","MOAB"),
  startdate = "2013-01",
  enddate = "2022-12",
  package = "basic",
  check.size = FALSE,
  release = "RELEASE-2025", 
  token  = Sys.getenv('NEON_PAT')
)

vst_mappingandtagging <- VstDat_multiple$vst_mappingandtagging %>% filter(plotID == "SJER_049" | plotID == "SJER_050"| plotID == "SJER_053"| plotID == "PUUM_036" | plotID == "JERC_054" | plotID == "SRER_047" | plotID == "BLAN_032" | plotID == "BLAN_036" | plotID == "ONAQ_025" | 
                                           plotID == "KONZ_042" | plotID == "KONZ_045" | plotID == "MOAB_044" | plotID == "MOAB_059" | plotID == "WREF_070"| plotID == "WREF_073"| plotID == "WREF_075"| plotID == "WREF_079")
vst_apparentindividual <- VstDat_multiple$vst_apparentindividual %>% filter(((plotID == "SJER_049" | plotID == "SJER_050" | plotID == "SJER_053"| plotID == "PUUM_036" | plotID == "JERC_054" | plotID == "SRER_047" | plotID == "BLAN_032" | plotID == "BLAN_036" | plotID == "ONAQ_025" | 
                                           plotID == "KONZ_042" | plotID == "KONZ_045" | plotID == "MOAB_044" | plotID == "MOAB_059" | plotID == "WREF_070"| plotID == "WREF_073"| plotID == "WREF_075"| plotID == "WREF_079") & (substr(eventID, 10, 13) == 2021 | substr(eventID, 10, 13) == 2022)) )
vst_nonwoody <- VstDat_multiple$`vst_non-woody`
vst_nonwoody <- if(!is.null(vst_nonwoody)) {
  vst_nonwoody <- vst_nonwoody %>% filter(((plotID == "SJER_049" | plotID == "SJER_050"  | plotID == "SJER_053"| plotID == "PUUM_036" | plotID == "JERC_054" | plotID == "SRER_047" | plotID == "BLAN_032" | plotID == "BLAN_036" | plotID == "ONAQ_025" | 
                                    plotID == "KONZ_042" | plotID == "KONZ_045" | plotID == "MOAB_044" | plotID == "MOAB_059" | plotID == "WREF_070"| plotID == "WREF_073"| plotID == "WREF_075"| plotID == "WREF_079") & (substr(eventID, 10, 13) == 2021 | substr(eventID, 10, 13) == 2022) ) | 
                                    (plotID == "WREF_077" & substr(eventID, 10, 13) == 2020))}
vst_perplotperyear <- VstDat_multiple$vst_perplotperyear %>% filter(((plotID == "SJER_049" | plotID == "SJER_053"| plotID == "PUUM_036" | plotID == "JERC_054" | plotID == "SRER_047" | plotID == "BLAN_032" | plotID == "BLAN_036" | plotID == "ONAQ_025" | 
                                    plotID == "KONZ_042" | plotID == "KONZ_045" |  plotID == "MOAB_044" | plotID == "MOAB_059" | plotID == "WREF_070"| plotID == "WREF_073"| plotID == "WREF_075"| plotID == "WREF_079") & (substr(eventID, 10, 13) == 2021 | substr(eventID, 10, 13) == 2022)) | 
                                    ((plotID == "WREF_077") & substr(eventID, 10, 13) == 2020))
vst_perplotperyear$samplingImpractical <- ifelse(vst_perplotperyear$eventID == "vst_WREF_2021" & vst_perplotperyear$plotID == "WREF_079", "logistical", vst_perplotperyear$samplingImpractical) # note: WREF_079 forced to have one year samplingImpractical and so should not appear in productivity output
vst_apparentindividual <- vst_apparentindividual %>% filter(!(individualID == "NEON.PLA.D02.BLAN.14314" & eventID == "vst_BLAN_2022")) # remove the one tree with bole from this eventID

# create minimal vst_MOAB_2022 data such that there are live shrubs (but not trees) in both 2021 and 2022
vst_perplotperyear_MOAB_2022 <- vst_perplotperyear %>% filter(siteID == "MOAB")
vst_perplotperyear_MOAB_2022$eventID <- "vst_MOAB_2022"
vst_perplotperyear <- rbind(vst_perplotperyear, vst_perplotperyear_MOAB_2022)
vst_apparentindividual$eventID <- ifelse(vst_apparentindividual$uid == "832e3efd-15bc-4b98-b29b-c507a9b3175c" | vst_apparentindividual$uid == "ef4f4d8c-fc4c-48ac-9af9-7f77fe825025", "vst_MOAB_2022", vst_apparentindividual$eventID)
vst_apparentindividual$plantStatus <- ifelse(vst_apparentindividual$uid == "832e3efd-15bc-4b98-b29b-c507a9b3175c", "Live", vst_apparentindividual$plantStatus)

# note: WREF_070 is dendrometerOnly and so should not appear in productivity output
# note: for 2020 WREF_075 only vst_non-woody and vst_perplotperyear retained, for purpose of including growthForm Xerophyllum


VstDat_LENO <- loadByProduct(
  dpID ="DP1.10098.001",
  site = c("LENO"),
  startdate = "2013-01",
  enddate = "2023-12",
  package = "basic",
  check.size = FALSE,
  release = "RELEASE-2025", 
  token  = Sys.getenv('NEON_PAT')
)

# for testing plotSubset "towerAll", with sampling more than one year apart
vst_LENO_mappingandtagging <- VstDat_LENO$vst_mappingandtagging %>% filter(plotID == "LENO_070")
vst_LENO_apparentindividual <- VstDat_LENO$vst_apparentindividual %>% filter(plotID == "LENO_070" & (substr(eventID, 10, 13) == 2018 | substr(eventID, 10, 13) == 2023))
vst_LENO_nonwoody <- VstDat_LENO$`vst_non-woody`
vst_LENO_nonwoody <- if(!is.null(vst_nonwoody)) {vst_nonwoody %>% filter(plotID == "LENO_070" & (substr(eventID, 10, 13) == 2018 | substr(eventID, 10, 13) == 2023))}
vst_LENO_perplotperyear <- VstDat_LENO$vst_perplotperyear %>% filter(plotID == "LENO_070" & (substr(eventID, 10, 13) == 2018 | substr(eventID, 10, 13) == 2023))
vst_LENO_perplotperyear$eventType <- ifelse(vst_LENO_perplotperyear$eventID == "vst_LENO_2018", "allTowerPlots", vst_LENO_perplotperyear$eventType)

vst_apparentindividual <- rbind(vst_apparentindividual, vst_LENO_apparentindividual)
vst_mappingandtagging <- rbind(vst_mappingandtagging, vst_LENO_mappingandtagging)
vst_nonwoody <- rbind(vst_nonwoody, vst_LENO_nonwoody)
vst_perplotperyear <- rbind(vst_perplotperyear, vst_LENO_perplotperyear)

vst_apparentindividual <- vst_apparentindividual %>% filter(uid != "fe76dd79-b577-42b8-81b3-6c9ae4ba254a" & uid != "dee5b5df-968c-46b5-bfb2-0d61158de951")
   # the first removes an instance of NEON.PLA.D16.WREF.03426 2021 
   # the second removes an instance of NEON.PLA.D16.WREF.03639 2022 (for testing recruitment and missing)

vst_apparentindividual$plantStatus <- ifelse(vst_apparentindividual$uid == "8bbaedea-c8ea-41e4-8a8f-5ed91a2e27e2", "Downed", vst_apparentindividual$plantStatus)
  # make NEON.PLA.D16.WREF.03425 Dead at t2 2022 to allow testing mortality functionality 

vst_apparentindividual$plantStatus <- ifelse(vst_apparentindividual$uid == "1cde2270-16c5-4a17-8693-bff4ada6f10a", "Standing dead", vst_apparentindividual$plantStatus)
  # make NEON.PLA.D08.LENO.08383 Dead at t2 2023 to allow testing mortality functionality                                                                     



VstDat_YELL <- loadByProduct(
  dpID ="DP1.10098.001",
  site = c("YELL"),
  startdate = "2013-01",
  enddate = "2023-12",
  package = "basic",
  check.size = FALSE,
  release = "RELEASE-2025", 
  token  = Sys.getenv('NEON_PAT')
)

vst_mappingandtagging_YELL <-  VstDat_YELL$vst_mappingandtagging %>% filter(plotID == "YELL_044" | plotID == "YELL_045"  | plotID == "YELL_046")
vst_perplotperyear_YELL <- VstDat_YELL$vst_perplot %>% filter( (plotID == "YELL_044" | plotID == "YELL_045"  | plotID == "YELL_046") & (eventID == "vst_YELL_2022" | eventID == "vst_YELL_2023") )

vst_apparentindividual_YELL <- VstDat_YELL$vst_apparentindividual %>% filter( (plotID == "YELL_044" | plotID == "YELL_045"  | plotID == "YELL_046") & (eventID == "vst_YELL_2022" | eventID == "vst_YELL_2023")  & growthForm == "single bole tree" )
vst_apparentindividual_YELL <- vst_apparentindividual_YELL %>% filter(!(plotID == "YELL_044" & eventID == "vst_YELL_2022"))
vst_apparentindividual_YELL <- vst_apparentindividual_YELL %>% filter(!(plotID == "YELL_045" & eventID == "vst_YELL_2023"))
vst_apparentindividual_YELL <- vst_apparentindividual_YELL %>% filter(!(plotID == "YELL_046"))


vst_apparentindividual_bind <- rbind(vst_apparentindividual, vst_apparentindividual_YELL)
vst_perplotperyear_bind <- rbind(vst_perplotperyear, vst_perplotperyear_YELL)
vst_mappingandtagging_bind <- rbind(vst_mappingandtagging, vst_mappingandtagging_YELL)


VstDat <- list(
   vst_apparentindividual = vst_apparentindividual_bind,
   vst_mappingandtagging = vst_mappingandtagging_bind,
   vst_perplotperyear = vst_perplotperyear_bind,
   'vst_non-woody' = vst_nonwoody
   )
saveRDS(VstDat, "VstDat.rds") # save data locally

