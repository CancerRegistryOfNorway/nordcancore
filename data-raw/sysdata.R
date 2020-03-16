


column_set_level_spaces <- list(
  sex = 1:2,
  area = data.table::fread("areas.csv")
  # etc.
)

usethis::use_data(column_set_level_spaces)


