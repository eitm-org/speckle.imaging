
test_that("read_speckle_xls gives you a dataframe", {
  expect(is.data.frame(read_speckle_xls(here("test_data", "speckle_test.xlsx"))),
         "this function is returning something that is not a dataframe")
})

df <- read_speckle_xls(here("test_data", "speckle_test.xlsx"))
df["reagent_dose_ul"] <- df["r1881"] <- df["ar_sirna_dose_ul"] <- df["gfp_sirna_dose"] <- df["scramble_sirna_dose"] <- df["control"] <- 0

test_that("get_yvar_dfs gives you a dataframe", {
  expect(is.data.frame(get_yvar_dfs("nuclei_number_of_spots", df)),
         "this function is returning something that is not a dataframe")
})

test_that("get_yvar_dfs result has a column called 'yvar'", {
  expect_contains(names(get_yvar_dfs("nuclei_number_of_spots", df)), c("yvar"))
})

drm_df <- data.frame(puncta_normalized = c(1),
                     concentration = c(1))

test_that("getDRM returns false when it's not a viable drm dataframe", {
  expect(getDRM(drm_df) == FALSE, "this function is finding a DRM when there is not one")
})

drm_df2 <- data.frame(puncta_normalized = c(10, 11, 10, 11, 2, 1, .001, .004),
                     concentration = c(1, 1, 2, 2, 3, 4, 5, 6))

test_that("getDRM returns true when it is a viable drm dataframe", {
  expect(getDRM(drm_df2), "this function can't find a DRM where there is one")
})


