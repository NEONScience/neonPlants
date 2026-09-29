#   Author: Samuel M Simkin, Courtney L Meier
#   email: samuel.simkin@gmail.com, cmeier@BattelleEcology.org


#   Save data frames as part of package
#--> CM note: Source files are missing
usethis::use_data(parameters,
                  plantIntTrop,
                  priority_plots,
                  taxon_fields,
                  internal = FALSE,
                  overwrite = TRUE,
                  version = 2)



### Process and save 'variables' data
variables <- read.csv("data-raw/variables.csv", header = TRUE, stringsAsFactors = FALSE)

usethis::use_data(variables,
                  internal = FALSE,
                  overwrite = TRUE)
