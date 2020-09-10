


# this unit test written to ensure we don't have conflicting information.
testthat::test_that(
  desc = "nordcan_entity_levels_by_icd10_and_sex and nordcan_columns agree",
  code = {
    dt <- nordcan_entity_levels_by_icd10_and_sex()
    entity_col_nms <- nordcan_column_name_set("column_name_set_entity")
    testthat::expect_equal(
      sort(names(dt)),
      sort(c("sex", "icd10", entity_col_nms))
    )
  }
)

