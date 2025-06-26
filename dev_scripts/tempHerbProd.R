hbpTest <- neonUtilities::loadByProduct(dpID = "DP1.10023.001",
                                        site = c("DCFS", "KONZ", "SJER"),
                                        startdate = "2021-09",
                                        enddate = "2024-10",
                                        check.size = FALSE,
                                        token = Sys.getenv("NEON_TOKEN"))

scaleHerbMassOutput <- neonPlants::scaleHerbMass(hbpTest,
                                                 plotSubset = "all")


herbScale <- neonPlants::scaleHerbMass(HbpDat,
                                       plotSubset = "all")


herbProd <- neonPlants::estimateHerbProd(hbpTest,
                                         plotSubset = "all")

#--> unexpected herbGroups in output for 2022 DCFS and 2023 KONZ when plotSubset = "all"
hbp_agb <- scaleHerbMassOutput$hbp_agb

hbp_agb <- hbp_agb %>%
  dplyr::filter(siteID %in% c("DCFS", "KONZ", "SJER"),
                collectDate >= "2022-01-01" & collectDate <= "2023-12-31")

hbp_agb <- test$hbp_agb
plotSubset <- "all"


#   DCFS sampling effort
dcfsBoutEffort <- hbp_agb_plot %>%
  dplyr::filter(siteID == "DCFS") %>%
  dplyr::group_by(siteID,
                  year,
                  plotType,
                  eventID) %>%
  dplyr::summarise(plotCount = length(unique(plotID)),
                   eventID = unique(eventID),
                   startDate = min(collectDate),
                   endDate = max(collectDate))


### Combining SD from groups with unequal sample size --> needed for consumption estimate SD
#--> Example from https://stats.stackexchange.com/questions/117741/adding-two-or-more-means-and-calculating-the-new-standard-deviation

## Create raw dataframe with unequal group sizes
set.seed(644)
dat_raw <- tibble(
  group = c(rep("A", 10), rep("B",20), rep("C",30)),
  x     = rnorm(60)
)


## Overall summary
dat_total <- dat_raw %>%
  summarise(
    n    = n(),
    mean = mean(x),
    sd   = sd(x)
  )

dat_total
#> # A tibble: 1 × 3
#>       n   mean    sd
#>   <int>  <dbl> <dbl>
#> 1    60 0.0542 0.873

## Summary by group
dat_grouped <- dat_raw %>%
  group_by(group) %>%
  summarise(
    n    = n(),
    mean = mean(x),
    sd   = sd(x)
  )

dat_grouped
#> # A tibble: 3 × 4
#>   group     n    mean    sd
#>   <chr> <int>   <dbl> <dbl>
#> 1 A        10  0.203  0.923
#> 2 B        20  0.0917 0.997
#> 3 C        30 -0.0204 0.788

## Calculating overall summary from grouped data
dat_total_grouped <- dat_grouped %>%
  mutate(
    ex  = n * mean,
    exx = sd^2 * (n-1) + ex^2 / n
  ) %>%
  summarise(across(c(n, ex, exx), sum)) %>%
  mutate(
    mean = ex/n,
    sd   = sqrt((exx - ex^2/n)/(n-1))
  ) %>%
  select(n, mean, sd)

dat_total_grouped
#> # A tibble: 1 × 3
#>       n   mean    sd
#>   <int>  <dbl> <dbl>
#> 1    60 0.0542 0.873


## Overall summary from ungroued and grouped data are equal
all.equal(dat_total, dat_total_grouped)
