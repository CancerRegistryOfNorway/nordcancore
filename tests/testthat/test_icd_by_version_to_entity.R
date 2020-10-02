

testthat::test_that("nordcan_metadata_icd_by_version_to_entity() contains no duplicates", {

  dt <- nordcan_metadata_icd_by_version_to_entity()
  dt <- dt[!is.na(dt$icd_code), ]
  testthat::expect_true(
    !any(duplicated(dt, by = c("icd_version", "icd_code")))
  )

})

