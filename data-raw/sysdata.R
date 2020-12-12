

# libraries ---------------------------------------------------------------
library("data.table")
pkgload::load_all(".")

# entity_usage_info ------------------------------------------------------------
entity_usage_info <- data.table::fread(
  specification_dataset_source("entity_usage_info")
)
# these entity numbers NOT USED in 9.0.
entity_usage_info <- entity_usage_info[
  !entity %in% c(51L, 318L),
  ]

# icd10_to_entity --------------------------------------------------------------
# @codedoc_comment_block entity
# The ICD-10 to entity conversion table, icd10_to_entity, is added to package
# nordcancore in script data-raw/sysdata.R. The table is downloaded from
# `r specification_dataset_source("icd10_to_entity")` and added to the package
# as an internal object.
# @codedoc_comment_blcok entity
icd10_to_entity <- data.table::fread(
  specification_dataset_source("icd10_to_entity")
)
icd10_to_entity[, "icd10" := toupper(icd10)]
data.table::setkeyv(icd10_to_entity, "icd10")

# icd10_vs_icd67_icd8_icd9 ------------------------------------------------
icd10_vs_icd67_icd8_icd9 <- data.table::fread(
  specification_dataset_source("icd10_vs_icd67_icd8_icd9")
)
icd_code_col_nms <- setdiff(names(icd10_vs_icd67_icd8_icd9), "icd10_label")
icd10_vs_icd67_icd8_icd9[
  j = (icd_code_col_nms) := lapply(.SD, function(col) {
    gsub("[.]", "", toupper(as.character(col)))
  }),
  .SDcols = icd_code_col_nms
]

# regions ----------------------------------------------------------------------
regions <- data.table::fread(
  specification_dataset_source("regions")
)

# nordcan_columns --------------------------------------------------------------
nordcan_columns <- data.table::fread("data-raw/nordcan_columns.csv")

# column_specification_list, joint_categorical_column_spaces -------------------
ne <- new.env()
source("data-raw/column_specifications.R", local = ne)
column_specification_list <- ne$column_specification_list
joint_categorical_column_spaces <- ne$joint_categorical_column_spaces

# nordcan_version, nordcan_year ------------------------------------------------
nordcan_version <- readLines("data-raw/nordcan_version.txt")
nordcan_year <- as.integer(readLines("data-raw/nordcan_year.txt"))

# save everything --------------------------------------------------------------
usethis::use_data(
  column_specification_list, joint_categorical_column_spaces,
  nordcan_columns, entity_usage_info, icd10_to_entity, icd10_vs_icd67_icd8_icd9,
  regions, nordcan_version, nordcan_year,
  internal = TRUE, overwrite = TRUE
)


