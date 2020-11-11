




#' @title Column Level Spaces
#' @description
#' Retrieve allowed levels for categorical NORDCAN columns.
#' @param col_nms `[character]` (mandatory, no default)
#'
#' character string vector of columns names for which to retrieve column
#' space(s)
#' @rdname column_level_spaces


nordcan_metadata_column_restrictions_by_global_settings <- function() {
  gs <- get_global_nordcan_settings()
  nordcan_year <- nordcan_metadata_nordcan_year()
  participant_info <- nordcan_metadata_participant_info()
  region_number_space <- participant_info[["column_limits"]][["region"]]
  list(
    yoi = list(
      levels = gs[["stat_cancer_record_count_first_year"]]:nordcan_year
    ),
    yof = list(
      levels = gs[["stat_cancer_death_count_first_year"]]:nordcan_year
    ),
    region = list(
      levels = region_number_space
    ),
    period_5 = list(
      levels = rev(seq(
        nordcan_year + 1L, gs[["stat_cancer_record_count_first_year"]], -5L
      )[-1L])
    )
  )
}

#' @name column_level_spaces
#' @details
#' - `nordcan_metadata_column_level_space_list` retrieves a `list` which
#' determines allowed levels for each column specified in `col_nms`
#' @export
nordcan_metadata_column_level_space_list <- function(col_nms) {
  dbc::assert_prod_input_is_character_nonNA_vector(col_nms)
  dbc::assert_prod_input_vector_elems_are_in_set(
    x = col_nms, set = nordcan_categorical_column_names()
  )

  restrictions <- nordcan_metadata_column_restrictions_by_global_settings()

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
    levels <- unique(dt[[col_nm]])
    if (col_nm %in% names(restrictions)) {
      levels <- intersect(restrictions[[col_nm]][["levels"]], levels)
    }
    return(levels)
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
  restrictions <- nordcan_metadata_column_restrictions_by_global_settings()
  output <- lapply(wh_to_use, function(wh) {
    dt <- joint_categorical_column_spaces[["joint_level_space"]][[wh]]
    col_nm_set <- col_nm_sets[[wh]]
    non_dup <- !duplicated(dt, by = col_nm_set)
    dt <- dt[i = non_dup, j = .SD, .SDcols = col_nm_set]
    restricted_col_nms <- intersect(names(dt), names(restrictions))
    if (length(restricted_col_nms) > 0L) {
      for (col_nm in restricted_col_nms) {
        subset <- dt[[col_nm]] %in% restrictions[[col_nm]][["levels"]]
        dt <- dt[subset, ]
      }
    }
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

nordcan_participant_names <- function() {
  regions <- nordcan_metadata_column_specifications("region")[["levels"]]
  regions <- sort(names(regions)[regions %% 10L == 0L])
  regions[!grepl("[nN]ordic", regions)]
}

#' @rdname global_nordcan_settings
#' @param work_dir `[character]` (mandatory, no default)
#'
#' root directory for any and all (temporarily) stored NORDCAN data; if it
#' does not exist it is attempted to be created
#' @param participant_name `[character]` (mandatory, no default)
#'
#' name of NORDCAN participant; e.g. "Denmark" or "Sweden"
#' @param stat_cancer_record_count_first_year `[integer]` (mandatory, no default)
#'
#' first year for which to compute the cancer record count statistics;
#' e.g. `1953L`
#' @param stat_cancer_death_count_first_year
#' `[integer]` (mandatory, no default)
#'
#' first year for the cancer death count statistics; e.g. `1953L`
#' @param regional_data_first_year
#' `[integer]` (mandatory, no default)
#'
#' first year for regional data; e.g. `1953L`
#'
#' @details
#'
#' Some first years for statistics are set using `set_global_nordcan_settings`.
#' However, the first year for survival is fixed to
#' `nordcancore::nordcan_metadata_nordcan_year() - 29L`. Likewise the first
#' year for prevalence is fixed to `stat_cancer_record_count_first_year + 20L`.
#'
#' @export
set_global_nordcan_settings <- function(
  work_dir,
  participant_name,
  stat_cancer_record_count_first_year,
  stat_cancer_death_count_first_year,
  regional_data_first_year
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

    is_year_arg <- grepl("year", arg_nm)
    if (is_year_arg) {
      dbc::assert_user_input_is_integer_nonNA_gtzero_atom(
        x = arg_value, x_nm = arg_nm
      )
    } else if (arg_nm == "participant_name") {
      dbc::assert_user_input_atom_is_in_set(
        x = arg_value, x_nm = arg_nm, set = nordcan_participant_names()
      )
    }
    global_settings_env[[arg_nm]] <- arg_value
  }))

  if (grepl("\\s", global_settings_env[["work_dir"]])) {
    stop("work_dir = ", deparse(global_settings_env[["work_dir"]]),
         " has a whitespace ( ); please ensure that the full path to your ",
         "working directory does not contain any whitespaces")
  }

  last_year <- nordcan_metadata_nordcan_year()
  # survival: 30 latest years, e.g. with last year 2018 -> years 1989-2018
  global_settings_env[["stat_survival_follow_up_first_year"]] <- last_year - 29L

  # prevalence: 20 years after first year of incidence, e.g. 1953 -> 1973
  global_settings_env[["stat_prevalent_subject_count_first_year"]] <- {
    global_settings_env[["stat_cancer_record_count_first_year"]] + 20L
  }

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
  expected_settings_nms <- names(formals(set_global_nordcan_settings))
  found_settings_nms <- ls(global_settings_env)
  miss_settings_nms <- setdiff(expected_settings_nms, found_settings_nms)
  if (length(miss_settings_nms) > 0L) {
    stop("The following setting(s) were not set: ",
         paste0(miss_settings_nms, collapse = ", "), "; see ",
         "?set_global_nordcan_settings")
  }
  as.list(global_settings_env)
}



#' @title NORDCAN Metadata
#' @description
#' Retrieve definition tables on NORDCAN datasets and their contents.
#' @name nordcan_metadata

#' @export
#' @rdname nordcan_metadata
#' @details
#' - `nordcan_metadata_nordcan_year` just returns the current NORDCAN year
#'   as an integer.
nordcan_metadata_nordcan_year <- function() {
  get_internal_dataset("nordcan_year", "nordcancore")
}

#' @export
#' @rdname nordcan_metadata
nordcan_metadata_icd10_to_entity <- function() {
  dt <- data.table::setDT(data.table::copy(
    get_internal_dataset("icd10_to_entity", "nordcancore")
  ))
  stopifnot(
    nchar(dt[["icd10"]]) %in% 3:4
  )
  icd10_char3_space <- unique(substr(unique(dt[["icd10"]]), 1L, 3L))
  icd10_char4_space_dt <- data.table::CJ(
    base = icd10_char3_space,
    fourth_char = 0:9
  )
  icd10_char4_space <- paste0(icd10_char4_space_dt[["base"]],
                              icd10_char4_space_dt[["fourth_char"]])
  add_icd10_char4 <- setdiff(icd10_char4_space, dt[["icd10"]])
  char3_of_add_icd10_char4 <- substr(add_icd10_char4, 1L, 3L)
  char3_exists <- char3_of_add_icd10_char4 %in% dt[["icd10"]]
  add_icd10_char4 <- add_icd10_char4[char3_exists]
  char3_of_add_icd10_char4 <- char3_of_add_icd10_char4[char3_exists]

  add_dt <- dt[dt[["icd10"]] %in% char3_of_add_icd10_char4, ]
  add_dt <- add_dt[
    i = data.table::data.table(icd10 = char3_of_add_icd10_char4),
    on = "icd10"
  ]
  add_dt[, "icd10" := add_icd10_char4]

  dt <- rbind(dt, add_dt)
  dt <- unique(dt, by = names(dt))
  data.table::setkeyv(dt, names(dt))

  return(dt[])
}

#' @export
#' @rdname nordcan_metadata
nordcan_metadata_entity_usage_info <- function() {
  data.table::setDT(data.table::copy(
    get_internal_dataset("entity_usage_info", "nordcancore")
  ))[]
}

#' @export
#' @rdname nordcan_metadata
nordcan_metadata_icd10_vs_icd67_icd8_icd9 <- function() {
  data.table::setDT(data.table::copy(
    get_internal_dataset("icd10_vs_icd67_icd8_icd9", "nordcancore")
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
  sex <- i.sex <- NULL # only to appease R CMD CHECK
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
  dt <- unique(dt, by = names(dt))
  brain_dt <- dt[dt$entity_level_20 == 320L, ][rep(1L, 3L), ]
  dbc::assert_prod_interim_is_data.table_with_required_names(
    dt,
    required_names = "entity_level_30"
  )
  brain_dt[, "entity_level_30" := c(316:317, 319L)]
  dt <- rbind(dt[!dt$entity_level_20 %in% 320L, ], brain_dt)
  return(dt[])
}


#' @export
#' @rdname nordcan_metadata
#' @importFrom data.table :=
nordcan_metadata_entity  <- function() {
  dt <- nordcan_metadata_entity_by_sex()
  dt[, "sex" := NULL]
  dt <- unique(dt, by = names(dt))
  return(dt[])
}



#' @export
#' @rdname nordcan_metadata
#' @param entity_no_set_name `[character]` (mandatory, no default)
#'
#' name of entity number set; e.g. "all"
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




nordcan_metadata_icd_by_version_to_entity_cache_env <- new.env()
nordcan_metadata_icd_by_version_to_entity_cache_env[["cache"]] <- NULL
#' @export
#' @rdname nordcan_metadata
#' @importFrom data.table :=
nordcan_metadata_icd_by_version_to_entity <- function() {
  cached <- nordcan_metadata_icd_by_version_to_entity_cache_env[["cache"]]
  if (!is.null(cached)){
    return(cached[])
  }
  icd_conversion <- nordcan_metadata_icd10_vs_icd67_icd8_icd9()
  icd10_to_entity <- nordcan_metadata_icd10_to_entity()
  icd_code_col_nms <- names(icd_conversion)[
    grepl("^icd[0-9]+$", names(icd_conversion))
  ]
  icd_to_entity <- merge(icd10_to_entity,
                         icd_conversion[, icd_code_col_nms, with = FALSE],
                         by = "icd10")
  entity_col_nms <- nordcan_metadata_column_name_set(
    "column_name_set_entity"
  )
  icd_to_entity <- data.table::melt(
    icd_to_entity,
    id.vars = entity_col_nms,
    measure.vars = icd_code_col_nms,
    variable.name = "icd_version",
    value.name = "icd_code"
  )
  icd_to_entity[
    j = "icd_version" := as.integer(sub("^icd", "", icd_to_entity$icd_version))
  ]
  icd_to_entity <- rbind(
    icd_to_entity[icd_to_entity$icd_version != 67, ],
    icd_to_entity[icd_to_entity$icd_version == 67, ][, "icd_version" := 6L][],
    icd_to_entity[icd_to_entity$icd_version == 67, ][, "icd_version" := 7L][]
  )

  # if user has 4-char code and we have only a matching 3-char definition,
  # we want to use that definition rather than no definition at all.
  # we create fake 4-char definitions based on the 3-char definition while
  # avoiding any pre-existing ones (real 4-char definitions)
  icd_code <- NULL # to appease R CMD CHECK
  short_icd_to_entity <- icd_to_entity[
    nchar(icd_code) == 3L,
  ]
  long_icd_to_entity <- short_icd_to_entity[
    j = list(long_icd_code = paste0(icd_code, 0:9)),
    keyby = eval(names(short_icd_to_entity))
  ]
  long_icd_to_entity[, "icd_code" := NULL]
  data.table::setnames(long_icd_to_entity, "long_icd_code", "icd_code")
  long_icd_to_entity[, "exists" := FALSE]
  long_icd_to_entity[
    i = icd_to_entity,
    on = c("icd_version", "icd_code"),
    j = "exists" := TRUE
  ]
  long_icd_to_entity <- long_icd_to_entity[
    long_icd_to_entity[["exists"]] == FALSE,
  ]
  long_icd_to_entity[, "exists" := NULL]

  icd_to_entity <- rbind(icd_to_entity, long_icd_to_entity)

  icd_to_entity <- unique(icd_to_entity, by = names(icd_to_entity))
  keep <- !is.na(icd_to_entity[["icd_code"]])
  icd_to_entity <- icd_to_entity[keep, ]

  data.table::setkeyv(icd_to_entity, c("icd_version", "icd_code"))
  data.table::setcolorder(icd_to_entity, c("icd_version", "icd_code"))

  nordcan_metadata_icd_by_version_to_entity_cache_env[["cache"]] <-
    icd_to_entity
  return(icd_to_entity[])
}





#' @export
#' @rdname nordcan_metadata
nordcan_metadata_participant_info <- function() {
  gs <- get_global_nordcan_settings()

  region_levels <- nordcan_metadata_column_specifications("region")[["levels"]]
  name <- gs[["participant_name"]]
  topregion_number <- unname(region_levels[name])
  region_number_space <- region_levels[
    substr(region_levels, 1L, 1L) == substr(topregion_number, 1L, 1L)
  ]
  region_number_space <- setdiff(region_number_space, topregion_number)
  if (length(region_number_space) == 0L) {
    # small participants only have the top region and no subregions
    region_number_space <- topregion_number
  }

  list(
    name = name,
    topregion_number = topregion_number,
    column_limits = list(
      region = region_number_space
    )
  )
}



#' @export
#' @rdname nordcan_metadata
nordcan_metadata_nordcan_version <- function() {
  get_internal_dataset("nordcan_version", "nordcancore")
}
