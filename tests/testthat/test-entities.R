





testthat::test_that("all entities in column level specs", {
  nordcancore::set_global_nordcan_settings(
    work_dir = tempdir(),
    participant_name = "Norway",
    first_year_incidence = 1953L,
    first_year_mortality = 1953L,
    first_year_region = 1953L,
    last_year_incidence = 2018L,
    last_year_mortality = 2018L,
    last_year_survival = 2018L
  )
  entity_no_set <- nordcan_metadata_entity_no_set("all")
  entity_no_set <- setdiff(entity_no_set, c(888L, 999L))

  entity_col_nms <- nordcan_metadata_column_name_set("column_name_set_entity")
  dt <- nordcan_metadata_column_level_space_dt(entity_col_nms)
  dt_entity_no_set <- setdiff(unlist(dt), NA_integer_)

  testthat::expect_identical(sort(entity_no_set), sort(dt_entity_no_set))

})
