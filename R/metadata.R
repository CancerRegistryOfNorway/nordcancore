




get_column_level_space <- function(col_nms) {
  column_set_level_spaces <- get_internal_dataset("column_set_level_spaces")

  # TODO

  stopifnot(
    data.table::is.data.table(column_level_space),
    identical(col_nms, names(column_level_space))
  )
  column_level_space
}



