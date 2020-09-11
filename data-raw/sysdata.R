
library("data.table")

tf_entity_usage <- tempfile(fileext = ".csv")
download.file(
  url = "https://raw.githubusercontent.com/CancerRegistryOfNorway/NORDCAN/master/specifications/entity_usage_info.csv ",
  destfile = tf_entity_usage
)
tf_icd10_to_entity <- tempfile(fileext = ".csv")
download.file(
  url = "https://raw.githubusercontent.com/CancerRegistryOfNorway/NORDCAN/master/specifications/icd10_to_entity_columns.csv",
  destfile = tf_icd10_to_entity
)
tf_icds <- tempfile(fileext = ".csv")
download.file(
  url = "https://raw.githubusercontent.com/CancerRegistryOfNorway/NORDCAN/master/specifications/icd10_vs_icd7_icd8_icd9.csv",
  destfile = tf_icds
)
tf_regions <- tempfile(fileext = ".csv")
download.file(
  url = "https://raw.githubusercontent.com/CancerRegistryOfNorway/NORDCAN/master/specifications/regions.csv",
  destfile = tf_regions
)

entity_usage_info <- data.table::fread(tf_entity_usage)
icd10_to_entity <- data.table::fread(tf_icd10_to_entity)
icd10_vs_icd7_icd8_icd9 <- data.table::fread(tf_icds)
regions <- data.table::fread(tf_regions)
nordcan_columns <- data.table::fread("data-raw/nordcan_columns.csv")

ne <- new.env()
source("data-raw/column_specifications.R", local = ne)
column_specification_list <- ne$column_specification_list
joint_categorical_column_spaces <- ne$joint_categorical_column_spaces

usethis::use_data(
  column_specifications_list, joint_categorical_column_spaces,
  nordcan_columns, entity_usage_info, icd10_to_entity, icd10_vs_icd7_icd8_icd9,
  regions, internal = TRUE, overwrite = TRUE
)


