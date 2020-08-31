
library("data.table")

nordcan_columns <- data.table::fread("data-raw/nordcan_columns.csv")

usethis::use_data(nordcan_columns, internal = TRUE, overwrite = TRUE)


