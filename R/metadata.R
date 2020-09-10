




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



assert_user_input_country_name <- function(x) {
  dbc::assert_user_input_is_character_nonNA_atom(x, x_nm = "country_name")
  dbc::assert_user_input_atom_is_in_set(
    x = x,
    x_nm = "country_name",
    set = nordcan_countries()
  )
}
assert_prod_input_country_name <- function(x) {
  dbc::assert_prod_input_is_character_nonNA_atom(x, x_nm = "country_name")
  dbc::assert_prod_input_atom_is_in_set(
    x,
    x_nm = "country_name",
    set = nordcan_countries()
  )
}

nordcan_countries <- function() {
  c("denmark", "finland", "norway", "sweden")
}

global_settings_env <- new.env(parent = emptyenv())
global_settings_env[["country_name"]] <- NA_character_
set_global_nordcan_settings <- function(
  country_name,
  first_stat_cancer_record_count_year,
  last_stat_cancer_record_count_year,
  first_stat_prevalent_subject_count_year,
  last_stat_prevalent_subject_count_year,
  first_stat_survival_follow_up_year,
  last_stat_survival_follow_up_year
  ) {
  arg_nms <- names(formals(set_global_nordcan_settings))
  invisible(lapply(arg_nms, function(arg_nm) {
    is_missing <- eval(substitute(
      missing(ARG),
      list(ARG = as.name(arg_nm))
    ), envir = parent.frame(2L))
    if (is_missing) {
      stop("You need to supply a value to argument named ",
           deparse(arg_nm), "; see ?set_global_nordcan_settings",
           call. = FALSE)
    }
    if (arg_nm == "country_name") {
      assert_user_input_country_name(country_name)
    } else {
      is_year_arg <- grepl("year$", arg_nm)
      if (is_year_arg) {
        dbc::assert_user_input_is_integer_nonNA_gtzero_atom(
          x = get(arg_nm), x_nm = arg_nm
        )
      }
    }
  }))

  invisible(NULL)
}
get_global_nordcan_settings <- function() {
  as.list(global_settings_env)
}



#' @title NORDCAN Entities
#' @description
#' Retrieve definition table of NORDCAN entities by ICD-10 and sex.
#' @format `data.table` with columns
#'
#' - `icd10`, of class `character`
#' - `sex`, of class `integer`
#' and the columns for entities by level, each an `integer` column.
nordcan_entity_levels_by_icd10_and_sex <- function() {
  get_internal_dataset("entity_levels_by_icd10_and_sex", "nordcancore")
}

