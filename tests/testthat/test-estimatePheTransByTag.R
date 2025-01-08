test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
# remove specific df from list to test check whether dfs are present
pheDat = pheDat[names(pheDat) != "phe_perindividual"]

# create intentional duplicate to test duplicate check
tags <- rbind(tags[1,], tags)

# create test set with no transitions
obs$phenophaseStatus <- "no"

# what tests are appropriate here?
