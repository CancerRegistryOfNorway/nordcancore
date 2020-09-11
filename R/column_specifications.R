
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



#' @importFrom data.table year
year_level_space <- function() {
  1800:data.table::year(Sys.Date())
}

#' @title Column Specifications
#' @description Retrieve NORDCAN column specifications.
#' @param column_name `[character]` (mandatory, no default)
#' name of one column for which to retrieve specifications
#' @export
#' @name nordcan_column_specifications

#' @rdname nordcan_column_specifications
#' @export
#' @importFrom dbc assert_is_character_nonNA_atom
#' @return
#' - `nordcan_column_format`: a `character` string that names the format; e.g.
#'   `"ID"` or `"Date"`
nordcan_column_format <- function(column_name) {
  dbc::assert_is_character_nonNA_atom(column_name)
  if (!column_name %in% names(column_specification_list)) {
    stop("No specifications for column named ", deparse(column_name))
  }
  column_specification_list[[column_name]][["format"]]
}


#' @rdname nordcan_column_specifications
#' @export
#' @importFrom dbc assert_is_character_nonNA_atom
#' @return
#' - `nordcan_column_specifications`: a `list` of specifications; elements of
#'   the list vary by format,
#'   but they all have element `format` (character string)
nordcan_column_specifications <- function(column_name) {
  dbc::assert_is_character_nonNA_atom(column_name)
  csl <- get_internal_dataset("column_specifications_list", "nordcancore")
  if (!column_name %in% names(csl)) {
    stop("No specifications defined for column_name = ", deparse(column_name))
  }
  csl[[column_name]]
}



nordcan_categorical_column_names <- function() {
  unique(unlist(joint_categorical_column_spaces$col_nm_set))
}
entity_column_names <- function() {
  dt <- nordcan_metadata_icd10_to_entity()
  names(dt)[grepl("^entity", names(dt))]
}











