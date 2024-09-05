### Unit tests for rootMassScale function #########
### POC: Courtney Meier, cmeier@BattelleEcology.org

### Read in test data
testCore <- readRDS(testthat::test_path("testdata", "valid-rootcore-201807.RDS"))
testMass <- readRDS(testthat::test_path("testdata", "valid-rootmass-201807.RDS"))
testDilution <- readRDS(testthat::test_path("testdata", "valid-rootdilution-201807.RDS"))



### Test: Function generates expected output type
#   