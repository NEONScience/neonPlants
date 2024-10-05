### Unit tests for rootMassStandardize function ####
### POC: Courtney Meier, cmeier@BattelleEcology.org

# ### Read in test data
# testMass <- readRDS(testthat::test_path("testdata", "valid-rootmass-201807.RDS"))
# 
# 
# 
# ### Test: Function generates expected output type
# testthat::test_that(desc = "Output type", {
#   
#   testthat::expect_type(object = rootMassStandardize(inputMass = testMass),
#                         type = "list")
#   
# })
# 
# 
# 
# ### Test: Function generates expected output class
# testthat::test_that(desc = "Output class", {
#   
#   testthat::expect_s3_class(object = rootMassStandardize(inputMass = testMass),
#                             class = "data.frame")
#   
# })  
# 
# 
# 
# ### Test: Function generates data frame with expected dimensions using test data
# #   Check expected row number of data frame
# testthat::test_that(desc = "Output data frame row number", {
#   
#   testthat::expect_identical(object = nrow(rootMassStandardize(inputMass = testMass)),
#                              expected = as.integer(1365))
# })
# 
# #   Check expected column number of data frame
# testthat::test_that(desc = "Output data frame column number", {
#   
#   testthat::expect_identical(object = ncol(rootMassStandardize(inputMass = testMass)),
#                              expected = as.integer(10))
# })
# 
# 
# 
# ### Test: Generate expected errors for issues with inputMass table
# # Test when inputMass lacks required column
# testthat::test_that(desc = "Table 'inputMass' missing column", {
#   
#   testthat::expect_error(object = rootChemJoin(inputMass = testMass %>%
#                                                  dplyr::select(-dryMass)),
#                          regexp = "Required columns missing from 'inputMass': dryMass")
# })
# 
# #   Test when inputMass has no data
# testthat::test_that(desc = "Table 'inputMass' missing data", {
#   
#   testthat::expect_error(object = rootChemJoin(inputMass = testMass %>%
#                                                  dplyr::filter(uid == "mangrove")),
#                          regexp = "Table 'inputMass' has no data.")
# })

