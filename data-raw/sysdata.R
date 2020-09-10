
library("data.table")

nordcan_columns <- data.table::fread("data-raw/nordcan_columns.csv")

download.file(
  url = "https://raw.githubusercontent.com/CancerRegistryOfNorway/NORDCAN/master/specifications/entity_levels_by_icd10_and_sex.csv",
  destfile = "data-raw/entity_levels_by_icd10_and_sex.csv"
)

entity_levels_by_icd10_and_sex <- data.table::fread(
  "data-raw/entity_levels_by_icd10_and_sex.csv"
)

usethis::use_data(nordcan_columns, entity_levels_by_icd10_and_sex, internal = TRUE, overwrite = TRUE)


