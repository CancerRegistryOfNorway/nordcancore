




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
#' - `nordcan_metadata_column_level_space_list` retrieves a `list` which
#' determines allowed levels for each column specified in `col_nms`
#' @importFrom dbc assert_is_character_nonNA_vector
#' @export
nordcan_metadata_column_level_space_list <- function(col_nms) {
  dbc::assert_prod_input_is_character_nonNA_vector(col_nms)
  dbc::assert_prod_input_vector_elems_are_in_set(
    x = col_nms, set = nordcan_categorical_column_names()
  )

  output <- lapply(col_nms, function(col_nm) {
    has_col_nm <- vapply(
      joint_categorical_column_spaces[["col_nm_set"]],
      function(col_nm_set) {
        col_nm %in% col_nm_set
      },
      logical(1L)
    )
    wh_has_col_nm <- which(has_col_nm)[1L]
    dt <-joint_categorical_column_spaces[["joint_level_space"]][[wh_has_col_nm]]
    unique(dt[[col_nm]])
  })
  names(output) <- col_nms
  dbc::report_to_assertion(dbc::tests_to_report(
    tests = c(
      "vapply(output, is.integer, logical(1L))"
    )
  ), assertion_type = "prod_output")
  return(output)
}

#' @name column_level_spaces
#' @details
#' - `nordcan_metadata_column_level_space_dt_list` retrieves a `list` of  `data.table`s,
#'   each of which a set of columns named in `col_nms`
#' @importFrom data.table is.data.table
#' @importFrom dbc assert_is_character_nonNA_vector
#' assert_prod_output_is_data.table_with_required_names
#' report_to_assertion tests_to_report
#' @importFrom data.table .SD
#' @export
nordcan_metadata_column_level_space_dt_list <- function(col_nms) {
  dbc::assert_prod_input_is_character_nonNA_vector(col_nms)
  dbc::assert_prod_input_vector_elems_are_in_set(
    x = col_nms, set = nordcan_categorical_column_names()
  )
  col_nm_sets <- lapply(joint_categorical_column_spaces[["col_nm_set"]],
                        intersect, y = col_nms)
  wh_to_use <- which(vapply(col_nm_sets, length, integer(1L)) > 0L)

  output <- lapply(wh_to_use, function(wh) {
    dt <- joint_categorical_column_spaces[["joint_level_space"]][[wh]]
    col_nm_set <- col_nm_sets[[wh]]
    non_dup <- !duplicated(dt, by = col_nm_set)
    dt <- dt[i = non_dup, j = .SD, .SDcols = col_nm_set]
    dt[]
  })
  names(output) <- vapply(col_nm_sets[wh_to_use], function(col_nm_set) {
    paste(deparse(col_nm_set), collapse = "")
  }, character(1L))
  return(output)
}

#' @name column_level_spaces
#' @details
#' - `nordcan_metadata_column_level_space_dt` retrieves a `data.table` which
#' determines allowed combinations of `col_nms`
#' @importFrom data.table is.data.table
#' @importFrom dbc assert_is_character_nonNA_vector
#' assert_prod_output_is_data.table_with_required_names
#' report_to_assertion tests_to_report
#' @export
nordcan_metadata_column_level_space_dt <- function(col_nms) {
  dbc::assert_prod_input_is_character_nonNA_vector(col_nms)
  dbc::assert_prod_input_vector_elems_are_in_set(
    x = col_nms, set = nordcan_categorical_column_names()
  )

  output <- level_space_list_to_level_space_data_table(
    nordcan_metadata_column_level_space_dt_list(col_nms = col_nms)
  )

  dbc::assert_prod_output_is_data.table_with_required_names(
    output,
    required_names = col_nms
  )
  dbc::report_to_assertion(
    dbc::tests_to_report(
      tests = paste0(
        "!duplicated(output, by = ", paste0(deparse(col_nms), collapse = ""),")"
      )
    ),
    assertion_type = "prod_output"
  )
  return(output)
}




global_settings_env <- new.env(parent = emptyenv())
#' @title NORDCAN Settings
#' @description Set and get settings for NORDCAN software.
#' @name global_nordcan_settings
NULL

#' @rdname global_nordcan_settings
#' @param work_dir `[character]` (mandatory, no default)
#'
#' root directory for any and all (temporarily) stored NORDCAN data; if it
#' does not exist it is attempted to be created
#' @export
set_global_nordcan_settings <- function(
  work_dir,
  stat_cancer_record_count_year_first,
  stat_cancer_record_count_year_last,
  stat_prevalent_subject_count_year_first,
  stat_prevalent_subject_count_year_last,
  stat_survival_follow_up_year_first,
  stat_survival_follow_up_year_last
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
    arg_value <- get(arg_nm)

    is_year_arg <- grepl("year$", arg_nm)
    if (is_year_arg) {
      dbc::assert_user_input_is_integer_nonNA_gtzero_atom(
        x = arg_value, x_nm = arg_nm
      )
    }
    global_settings_env[[arg_nm]] <- arg_value
  }))

  global_settings_env[["work_dir"]] <- normalizePath(
    global_settings_env[["work_dir"]], mustWork = FALSE
  )

  global_settings_env[["iarccrgtools_work_dir"]] <- normalizePath(paste0(
    global_settings_env[["work_dir"]],
    "/iarccrgtools"
  ), mustWork = FALSE)
  global_settings_env[["survival_work_dir"]] <- normalizePath(paste0(
    global_settings_env[["work_dir"]],
    "/survival"
  ), mustWork = FALSE)

  dir_setting_names <- c("work_dir", "iarccrgtools_work_dir",
                         "survival_work_dir")

  lapply(dir_setting_names, function(dir_setting_name) {
    dir_path <- global_settings_env[[dir_setting_name]]
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    } else {
      if (!is_writable(dir_path)) {
        stop("Looks like directory ", deparse(dir_path), " is not writable; ",
             "please ensure you can write files into that directory before ",
             "proceeding.")
      }
    }
  })

  invisible(NULL)
}

#' @rdname global_nordcan_settings
#' @export
get_global_nordcan_settings <- function() {
  as.list(global_settings_env)
}


#' @title NORDCAN Metadata
#' @description
#' Retrieve definition tables on NORDCAN datasets and their contents.
#' @name nordcan_metadata

#' @export
#' @rdname nordcan_metadata
#' @importFrom data.table setDT copy
nordcan_metadata_icd10_to_entity <- function() {
  data.table::setDT(data.table::copy(
    get_internal_dataset("icd10_to_entity", "nordcancore")
  ))[]
}

#' @export
#' @rdname nordcan_metadata
#' @importFrom data.table setDT copy
nordcan_metadata_entity_usage_info <- function() {
  data.table::setDT(data.table::copy(
    get_internal_dataset("entity_usage_info", "nordcancore")
  ))[]
}

#' @export
#' @rdname nordcan_metadata
#' @importFrom data.table setDT copy
nordcan_metadata_icd10_vs_icd7_icd8_icd9 <- function() {
  data.table::setDT(data.table::copy(
    get_internal_dataset("icd10_vs_icd7_icd8_icd9", "nordcancore")
  ))[]
}


#' @export
#' @rdname nordcan_metadata
#' @importFrom data.table :=
nordcan_metadata_entity_by_sex_icd10 <- function() {
  usage <- nordcan_metadata_entity_usage_info()
  icd10_to_entity <- nordcan_metadata_icd10_to_entity()
  icd10_to_entity[, "sex" := NA_integer_]
  entity_col_nms <- names(icd10_to_entity)[
    grepl("^entity", names(icd10_to_entity))
  ]
  i.sex <- NULL # only to appease R CMD CHECK
  # rev: start from smallest group
  lapply(rev(entity_col_nms), function(entity_col_nm) {
    data.table::setnames(usage, "entity", entity_col_nm)
    on.exit(data.table::setnames(usage, entity_col_nm, "entity"))
    icd10_to_entity[
      i = usage,
      on = entity_col_nm,
      j = "sex" := data.table::fifelse(is.na(sex), i.sex, sex)
    ]
    NULL
  })

  data.table::setcolorder(icd10_to_entity, c("sex", "icd10", entity_col_nms))
  return(icd10_to_entity[])
}

#' @export
#' @rdname nordcan_metadata
#' @importFrom data.table :=
nordcan_metadata_entity_by_sex  <- function() {
  dt <- nordcan_metadata_entity_by_sex_icd10()
  dt[, "icd10" := NULL]
  return(unique(dt, by = names(dt))[])
}




#' @export
#' @rdname nordcan_metadata
#' @importFrom dbc assert_prod_input_is_character_nonNA_atom
#' assert_prod_input_atom_is_in_set
nordcan_metadata_entity_no_set <- function(entity_no_set_name) {
  dbc::assert_prod_input_is_character_nonNA_atom(entity_no_set_name)
  entity_no_set_names <- c(
    "cancer_record_count",
    "prevalent_subject_count",
    "cancer_death_count",
    "survival",
    "all"
  )
  dbc::assert_prod_input_atom_is_in_set(
    entity_no_set_name, set = entity_no_set_names
  )
  dt <- nordcan_metadata_entity_usage_info()
  keep <- switch(
    entity_no_set_name,
    cancer_record_count = dt[["incidence/prevalence"]],
    prevalent_subject_count = dt[["incidence/prevalence"]],
    cancer_death_count = dt[["mortality"]],
    survival = dt[["survival"]],
    all = rep(TRUE, nrow(dt))
  )
  dt[["entity"]][keep]
}





