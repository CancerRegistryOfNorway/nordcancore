




#' @title Column Level Spaces
#' @description
#' Retrieve allowed levels for categorical NORDCAN columns.
#' @param col_nms `[character]` (mandatory, no default)
#'
#' character string vector of columns names for which to retrieve column
#' space(s)
#' @rdname column_level_spaces


#' @describeIn column_level_spaces retrieves a list of column level spaces, one
#' element of each element of `col_nms`
#' @importFrom data.table is.data.table
#' @importFrom easyassertions assert_is_list assert_has_names
#' @export
get_column_level_spaces <- function(col_nms) {
  column_level_spaces <- get_internal_dataset("column_set_level_spaces")
  easyassertions::assert_is_list(column_level_spaces)
  easyassertions::assert_has_names(column_level_spaces, col_nms)

  column_level_spaces[col_nms]
}


#' @describeIn column_level_spaces retrieves a `data.table` which determines
#' allowed combinations of `col_nms`
#' @importFrom data.table is.data.table CJ
#' @importFrom easyassertions assert_is_list assert_has_names
get_joint_column_level_space <- function(col_nms) {
  column_level_spaces <- get_internal_dataset("column_set_level_spaces")
  easyassertions::assert_is_list(column_level_spaces)
  easyassertions::assert_has_names(column_level_spaces, col_nms)

  do.call(data.table::CJ, column_level_spaces)
}

