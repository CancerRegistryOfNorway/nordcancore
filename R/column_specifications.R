
month_level_space <- function() {
  c(
    January = 1L,
    February = 2L,
    March = 3L,
    April = 4L,
    May = 5L,
    June = 6L,
    July = 7L,
    August = 8L,
    September = 9L,
    October = 10L,
    November = 11L,
    December = 12L
  )
}



year_level_space <- function() {
  1800:data.table::year(Sys.Date())
}

#' @title Column Specifications
#' @description Retrieve NORDCAN column specifications.
#' @param column_name `[character]` (mandatory, no default)
#' name of one column for which to retrieve specifications
#' @export
#' @name nordcan_metadata_column_specifications

#' @rdname nordcan_metadata_column_specifications
#' @export
#' @return
#' - `nordcan_metadata_column_format`: a `character` string that names the format; e.g.
#'   `"ID"` or `"Date"`
nordcan_metadata_column_format <- function(column_name) {
  dbc::assert_is_character_nonNA_atom(column_name)
  csl <- get_internal_dataset(
    "column_specification_list", "nordcancore"
  )
  if (!column_name %in% names(csl)) {
    stop("No specifications for column named ", deparse(column_name))
  }
  csl[[column_name]][["format"]]
}


#' @rdname nordcan_metadata_column_specifications
#' @export
#' @return
#' - `nordcan_metadata_column_specifications`: a `list` of specifications; elements of
#'   the list vary by format,
#'   but they all have element `format` (character string)
nordcan_metadata_column_specifications <- function(column_name) {
  dbc::assert_is_character_nonNA_atom(column_name)
  csl <- get_internal_dataset("column_specification_list", "nordcancore")
  if (!column_name %in% names(csl)) {
    stop("No specifications defined for column_name = ", deparse(column_name))
  }
  csl[[column_name]]
}



nordcan_categorical_column_names <- function() {
  unique(unlist(joint_categorical_column_spaces$col_nm_set))
}
entity_column_names <- function() {
  nordcan_metadata_column_name_set("column_name_set_entity")
}











