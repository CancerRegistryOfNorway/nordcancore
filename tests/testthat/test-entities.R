





testthat::test_that("all entities in column level specs", {
  nordcancore::set_global_nordcan_settings(
    work_dir = tempdir(),
    participant_name = "Finland",
    stat_cancer_record_count_first_year = 1953L,
    stat_prevalent_subject_count_first_year = 1967L,
    stat_cancer_death_count_first_year = 1953L,
    stat_survival_follow_up_first_year = 1967L,
    regional_data_first_year = 1953L
  )
  entity_no_set <- nordcan_metadata_entity_no_set("all")
  entity_no_set <- setdiff(entity_no_set, c(888L, 999L))

  entity_col_nms <- nordcan_metadata_column_name_set("column_name_set_entity")
  dt <- nordcan_metadata_column_level_space_dt(entity_col_nms)
  dt_entity_no_set <- setdiff(unlist(dt), NA_integer_)

  testthat::expect_identical(sort(entity_no_set), sort(dt_entity_no_set))

})
