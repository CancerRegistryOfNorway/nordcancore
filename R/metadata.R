




#' @title Column Level Spaces
#' @description
#' Retrieve allowed levels for categorical NORDCAN columns.
#' @param col_nms `[character]` (mandatory, no default)
#'
#' character string vector of columns names for which to retrieve column
#' space(s)
#' @rdname column_level_spaces


#' @name column_level_spaces
#' @details
#' - `nordcan_column_level_space_list` retrieves a `list` which
#' determines allowed levels for each column specified in `col_nms`
#' @importFrom dbc assert_is_character_nonNA_vector
#' @export
nordcan_column_level_space_list <- function(col_nms) {
  dbc::assert_is_character_nonNA_vector(col_nms)
  # the actual values would probably come from an internal or exported dataset
  # inside this package.
  # ... datasteps

  stopifnot(
    inherits(output, "list"),
    identical(names(output), col_nms),
    vapply(output, is.integer, logical(1L)),
    !vapply(output, anyDuplicated, logical(1L))
  )
  return(output)
}


#' @name column_level_spaces
#' @details
#' - `nordcan_column_set_level_space_list` retrieves a `list` which
#' determines allowed levels for each column specified in `col_nms`,
#' and the allowed combinations for hierarchical columns
#' @importFrom dbc assert_is_character_nonNA_vector
#' @export
nordcan_column_set_level_space_list <- function(col_nms) {
  # you can think of a better name than this.
  dbc::assert_is_character_nonNA_vector(col_nms)

  # ... datasteps

  # e.g. areas are like norway -> oslo, finland -> helsinki, and it is not
  # possible to have e.g. norway -> helsinki. such columns are hierarchical.
  # for hierarchical cases this function must return a data.table as an element
  # in  the list, e.g.
  # list(area = data.table::data.table(area1 = c(1,1,2,2), area2 = c(1,2,3,4))).
  stopifnot(
    inherits(output, "list"),
    identical(names(output), col_nms),
    vapply(output, inherits, logical(1L), what = c("integer", "data.table")),
    !vapply(output, anyDuplicated, logical(1L))
  )
  return(output)
}

#' @name column_level_spaces
#' @details
#' - `nordcan_column_level_space_dt` retrieves a `data.table` which
#' determines allowed combinations of `col_nms`
#' @importFrom data.table is.data.table
#' @importFrom dbc assert_is_character_nonNA_vector
#' assert_prod_output_is_data.table_with_required_names
#' report_to_assertion tests_to_report
nordcan_column_level_space_dt <- function(col_nms) {
  dbc::assert_prod_input_is_character_nonNA_vector(col_nms)



  dbc::assert_prod_output_is_data.table_with_required_names(
    output,
    required_names = col_nms
  )
  dbc::report_to_assertion(
    dbc::tests_to_report(
      tests = paste0("!duplicated(output, by = ",deparse(col_nms),")")
    ),
    assertion_type = "prod_output"
  )
  return(output)
}

