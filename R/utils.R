


is_writable <- function(path) {
  dbc::assert_prod_input_is_character_nonNA_atom(path)
  file.access(path, mode = 2L) == 0L
}




level_space_list_to_level_space_data_table <- function(
  x
) {
  dbc::assert_prod_input_is_list(x)
  dbc::assert_prod_input_is_uniquely_named(x)
  stopifnot(
    vapply(x,
           function(elem) {is.vector(x) || data.table::is.data.table(x)},
           logical(1L))
  )

  contains_dt <- vapply(x, data.table::is.data.table, logical(1L))
  dt <- do.call(data.table::CJ, lapply(seq_along(x), function(i) {
    if (contains_dt[i]) {
      1:nrow(x[[i]])
    } else {
      seq_along(x[[i]])
    }
  }))
  pos_col_nms <- paste0("_____", names(dt), "_pos")
  names(pos_col_nms) <- names(x)
  data.table::setnames(dt, names(dt), pos_col_nms)
  lapply(seq_along(x), function(i) {
    pos_col_nm <- pos_col_nms[i]
    x_i_is_dt <- contains_dt[i]
    x_i <- x[[i]]
    value_col_nms <- if (x_i_is_dt) names(x_i) else names(x)[i]
    pos_vec <- dt[[pos_col_nm]]
    data.table::set(
      x = dt,
      j = value_col_nms,
      value = if (x_i_is_dt) x_i[pos_vec, ] else x_i[pos_vec]
    )
    NULL
  })
  data.table::set(x = dt, j = pos_col_nms, value = NULL)
  data.table::setkeyv(dt, names(dt))
  return(dt[])
}




#' @title Package Data
#' @description Utilities to access datasets contained in R packages.
#' @name package_data_utils
#' @param package_name `[character]` (mandatory, no default)
#'
#' name of R package





#' @export
#' @rdname package_data_utils
get_exported_dataset_names <- function(package_name) {
  dbc::assert_is_character_nonNA_atom(package_name)
  dataset_names <- utils::data(package = package_name)
  dataset_names[["results"]][, "Item"]
}



#' @export
#' @rdname package_data_utils
#' @param dataset_name `[character]` (mandatory, no default)
#'
#' name of dataset to retrieve.
get_exported_dataset <- function(dataset_name, package_name) {
  dbc::assert_is_character_nonNA_atom(dataset_name)
  dbc::assert_is_character_nonNA_atom(package_name)

  expo_data_nms <- get_exported_dataset_names(package_name)
  if (!dataset_name %in% expo_data_nms) {
    stop(
      "requested exported dataset ",
      deparse(dataset_name), " is not one of ",
      deparse(expo_data_nms), " exported datasets in package ",
      package_name
    )
  }
  getExportedValue(ns = package_name, name = dataset_name)
}





#' @export
#' @rdname package_data_utils
get_internal_dataset <- function(dataset_name, package_name) {
  dbc::assert_is_character_nonNA_atom(dataset_name)
  dbc::assert_is_character_nonNA_atom(package_name)

  obj <- tryCatch(
    eval(parse(text = paste0(package_name, ":::", dataset_name))),
    error = function(e) e
  )

  if (inherits(obj, "error")) {
    stop("Internal error: requested internal dataset ",
         deparse(dataset_name), " is not one of the internal datasets ",
         "in package ", package_name,
         ". If you see this, please complain to the ",
         "package maintainer ", utils::maintainer(package_name))
  }

  obj
}



#' @title Entity Detection
#' @description
#' Detect records that have at least one of the supplied entities.
#' @param x `[data.table]` (mandatory, no default)
#'
#' dataset in which to look for records in specific entities
#' @param entities `[integer]` (mandatory, no default)
#'
#' set of entities to look for
#' @return
#' Returns a logical vector of length `nrow(x)`.
#' @export
in_entity_set <- function(x, entities) {
  dbc::assert_is_data.table(x)
  dbc::assert_is_integer_nonNA_vector(entities)
  # TODO: specific assertion fun for entities and nordcan dataset

  entity_col_nms <- names(x)[grepl("^entity", names(x))]

  in_set <- rep(FALSE, nrow(x))
  for (entity_col_nm in entity_col_nms) {
    in_set <- in_set | x[[entity_col_nm]] %in% entities
  }

  return(in_set)
}




#' @title NORDCAN Columns
#' @description
#' Utilities to retrieve information about NORDCAN dataset columns.
#' @name nordcan_columns


#' @rdname nordcan_columns
#' @return
#' - `nordcan_metadata_column_name_set_names`: a `character` vector of names of all
#'   column name sets
#' @export
nordcan_metadata_column_name_set_names <- function() {
  dt <- get_internal_dataset("nordcan_columns", package_name = "nordcancore")
  column_name_set_names <- names(dt)[grepl("^column_name_set_", names(dt))]
  column_name_set_names
}

#' @rdname nordcan_columns
#' @param column_name_set_name `[character]` (mandatory, no default)
#'
#' name of column name, one of the elements from output of
#' `nordcan_metadata_column_name_set_names()`; causes all column names for this set to
#' be retrieved
#' @return
#' - `nordcan_metadata_column_name_set`: a `character` vector of names in queried set
#' @export
nordcan_metadata_column_name_set <- function(column_name_set_name) {
  dbc::assert_is_character_nonNA_atom(column_name_set_name)
  dt <- get_internal_dataset("nordcan_columns", package_name = "nordcancore")
  dbc::assert_atom_is_in_set(
    column_name_set_name, set = nordcan_metadata_column_name_set_names()
  )

  col_nm_set <- dt[["column_name"]]
  col_name_set_data <- dt[[column_name_set_name]]
  if (is.character(col_name_set_data)) {
    names(col_nm_set) <- col_name_set_data
    col_nm_set <- col_nm_set[col_name_set_data != ""]
  } else if (is.logical(col_name_set_data)) {
    col_nm_set <- col_nm_set[col_name_set_data %in% TRUE]
  }

  return(col_nm_set)
}


#' @title NORDCAN Dataset Names
#' @description Retrieve names of NORDCAN datasets.
#' @export
nordcan_metadata_dataset_names <- function() {
  nms <- nordcan_metadata_column_name_set_names()
  nms <- nms[grepl("(_dataset)|(life_table)$", nms)]
  sub("^column_name_set_", "", nms)
}

#' @title NORDCAN IARC CRG Tools Tool Names
#' @description Retrieve names of tools of IARC CRG Tools supported by the
#' NORDCAN R framework.
#' @export
nordcan_iarccrgtools_tool_names <- function() {
  nms <- nordcan_metadata_column_name_set_names()
  nms <- nms[grepl("^column_name_set_iarccrgtools", nms)]
  nms <- sub("^column_name_set_iarccrgtools_((all)|(mandatory))_", "", nms)
  nms <- unique(nms)
  nms
}



#' @title Random Names
#' @description
#' Create unique names for objects, files, etc. that are guaranteed to not
#' already exist.
#' @param n_random_names `[integer]` (mandatory, default `1L`)
#'
#' generate this many random names
#' @param exclude_names `[character]` (optional, default `character(0L)`)
#' names are generated until none of them is one the names
#' supplied here.
#' @param name_length `[integer]` (mandatory, default `10L`)
#'
#' length of each random name
#' @param character_pool `[character]` (mandatory, default `letters`)
#'
#' vector of characters (each must be a string of length one) to sample from
#' when generating random names; the first character in any random name is
#' always generated from `letters` despite what is supplied here
#' @param transform `[function]` (mandatory, default `identity`)
#'
#' this transformation is applied first before checking whether
#' they already exist in `exclude_names`; the function must have the argument
#' `x` to accept the current random names and it must output them after
#' any transformation (such as adding a file extension; see **Examples**)
#' @param n_max_tries `[integer]` (mandatory, default `1000L`)
#'
#' sampling of an individual random name is attempted this many times before
#' giving up (raising an error); one may need to give if for some reason
#' there are so many names to avoid or the pool of characters is so small
#' that no random name can be generated
#' @examples
#'
#' # avoiding writing over a pre-existing file
#' random_names(exclude_names = "my_file.csv",
#'              transform = function(x) paste0(x, ".csv"))
#'
#' # avoiding writing over a pre-existing file path
#' random_names(exclude_names = "path/to/my_file.csv",
#'              transform = function(x) paste0("path/to/", x, ".csv"))
#'
#' # avoiding writing over a pre-existing file path for multiple files
#' random_names(n_random_names = 10L,
#'              exclude_names = "path/to/my_file.csv",
#'              transform = function(x) paste0("path/to/", x, ".csv"))
#'
#' @export
random_names <- function(
  n_random_names = 1L,
  exclude_names = character(0L),
  name_length = 10L,
  character_pool = letters,
  transform = identity,
  n_max_tries = 1000L
  ) {
  dbc::assert_is_integer_nonNA_gtzero_atom(n_random_names)
  dbc::assert_is_character_vector(exclude_names)
  dbc::assert_is_integer_nonNA_gtzero_atom(name_length)
  dbc::assert_is_character_nonNA_vector(character_pool)
  dbc::assert_is_function(transform)
  dbc::assert_is_integer_nonNA_gtzero_atom(n_max_tries)

  if (is.null(transform)) {
    transform <- function(x) x
  }
  exclude_names <- union(exclude_names, NA_character_)
  if (n_random_names == 1L) {
    name <- NA_character_

    for (try_no in 1:n_max_tries) {
      name_start <- sample(letters, size = 1L)
      name_end <- paste0(
        sample(character_pool, size = name_length - 1L, replace = TRUE),
        collapse = ""
      )
      name <- paste0(name_start, name_end)
      name <- transform(name)
      if (!name %in% exclude_names) {
        break
      }
    }
    if (try_no == n_max_tries) {
      stop("could not generate random name after n_max_tries = ",
           n_max_tries)
    }
    return(name)
  } else {
    # browser()
    arg_list <- mget(names(formals(random_names)))
    arg_list[["n_random_names"]] <- 1L
    names <- rep(NA_character_, n_random_names)
    for (i in 1:n_random_names) {
      names[i] <- do.call(random_names, arg_list)
      arg_list[["exclude_names"]] <- union(
        arg_list[["exclude_names"]], names[i]
      )
    }
    return(names)
  }

}





#' @title Handle Function Subset Argument
#' @description
#' This function is intended to be used inside other functions to handle that
#' function's argument `subset`.
#' @param subset_arg_nm `[character]` (mandatory, default `"subset"`)
#'
#' name of subset argument of the function where this function is used
#' @param dataset `[environment, data.frame]` (optional, default `emptyenv()`)
#'
#' if `subset` is an expression, it can evaluated in this context;
#' see ?eval
#' @param function_env `[environment]` (optional, default `parent.frame(1L)`)
#'
#' env where to collect `subset` expression from --- the default should work
#' @param enclosing_env `[environment]` (optional, default `parent.frame(2L)`)
#'
#' env where to continue scoping after `dataset`; think of this as the env
#' where `dataset` exists if it is a data.frame
#' @param function_call `[call]` (optional, default `sys.call(1L)`)
#'
#' in case a warning is emitted due to NA values in the subset, this call
#' is passed to [base::simpleWarning].
#'
#' @details
#'
#' The subset arg can be one of `logical`, `integer`, or `NULL`. `NULL` implies
#' no subset. `logical` must be of length `nrow(dataset)`, if `dataset` is a
#' data.frame. Similarly, `integer` subset must have values in between
#' `-nrow(dataset)` and `nrow(dataset)`.
#'
#' @examples
#'
#' my_fun <- function(x, subset = NULL) {
#'   stopifnot(is.data.frame(x))
#'   evaluated_subset <- handle_subset_arg(dataset = x)
#'   if (is.null(evaluated_subset)) {
#'     n <- nrow(x)
#'   } else {
#'     n <- length(x[[1]][evaluated_subset])
#'   }
#'   return(n)
#' }
#'
#' df <- data.frame(a = 1:5, b = 5:1)
#' my_fun(x = df, subset = df$a > 3)
#' my_fun(x = df, subset = a > 3)
#' my_fun(x = df, subset = 1:3)
#' @export
handle_subset_arg <- function(
  subset_arg_nm = "subset",
  dataset = emptyenv(),
  function_env = parent.frame(1L),
  enclosing_env = parent.frame(2L),
  function_call = sys.call(1L)
) {
  dbc::assert_prod_input_is_character_nonNA_atom(subset_arg_nm)
  dbc::assert_prod_input_has_one_of_classes(
    dataset, classes = c("data.frame", "environment")
  )
  dbc::assert_prod_input_has_class(
    function_env, required_class = "environment"
  )
  dbc::assert_prod_input_has_class(
    enclosing_env, required_class = "environment"
  )
  stopifnot(
    is.name(parse(text = subset_arg_nm)[[1L]])
  )

  # symbol of subset arg of function where this function is used
  # e.g. my_subset_arg in my_fun(my_subset_arg = column > 1L, dataset = my_data)
  subset_arg_symbol <- parse(text = subset_arg_nm)[[1L]]
  # infer the expression supplied to the subset arg of the
  # function where this function is used; e.g.
  # e.g. column > 1L in my_fun(my_subset_arg = column > 1L, dataset = my_data)
  subset_expr <- eval(substitute(
    substitute(SUBSET_OBJ), list(SUBSET_OBJ = subset_arg_symbol)
  ), envir = function_env)
  # evaluate that inferred expression in the context of the dataset and
  # secondarily the enclosing env
  value_eval_env <- as.environment(dataset)
  parent.env(value_eval_env) <- enclosing_env
  subset_value <- eval(subset_expr, envir = value_eval_env)

  subset_expr_text <- paste0(deparse(subset_expr), collapse = "")
  dbc::assert_prod_interim_is_one_of(
    subset_value,
    x_nm = subset_expr_text,
    funs = c("report_is_NULL", "report_is_logical_vector",
             "report_is_number_vector")
  )

  if (anyNA(subset_value)) {
    is_na <- is.na(subset_value)
    msg <- paste0(
      "there were ", sum(is_na), " NA values passed to argument ",
      deparse(subset_arg_nm),"; they will not be included in subset"
    )
    warning(simpleWarning(msg, call = function_call))
    if (is.logical(subset_value)) {
      subset_value[is_na] <- FALSE
    } else {
      subset_value <- subset_value[!is_na]
    }
  }

  if (is.numeric(subset_value) && any(subset_value %% 1L != 0L)) {
    stop("numeric subset is not integer-like; e.g. subset = c(1, 5) ",
         "is fine, but subset = c(1.1, 5.1) is not")
  }

  if (is.data.frame(dataset) && !is.null(subset_value)) {
    if (is.logical(subset_value)) {
      if (!length(subset_value) %in% nrow(dataset)) {
        stop("dataset has ", nrow(dataset), " rows but logical subset is of ",
             "length ", length(subset_value))
      }
    }
    if (is.integer(subset_value) && length(subset_value) > 0L && max(subset_value) > nrow(dataset)) {
      stop("max(", subset_expr_text, ") > number of rows in dataset")
    }
  }

  return(subset_value)
}




#' @export
#' @describeIn handle_subset_arg intersect two subsetting vectors
#' @param subset1 `[NULL, integer, logical]` (mandatory, no default)
#' subset to combine with subset2
#'
#' @param subset2 `[NULL, integer, logical]` (mandatory, no default)
#' subset to combine with subset1
#'
#' @details
#' `subset_and` is used to combine two results of `handle_subset_arg`.
#' If both `subset1` and `subset2` are logical, this is the same as
#' `subset1 & subset2`. However, this function handles all the possible pairs
#' of classes of `subset1` and `subset2` intelligently.
subset_and <- function(
  subset1,
  subset2
) {
  dbc::assert_prod_input_is_one_of(
    x = subset1,
    funs = c("report_is_NULL", "report_is_logical", "report_is_integer")
  )
  dbc::assert_prod_input_is_one_of(
    x = subset2,
    funs = c("report_is_NULL", "report_is_logical", "report_is_integer")
  )
  if (is.null(subset1) && is.null(subset2)) {
    return(NULL)
  }
  if (is.null(subset1)) {
    return(subset2)
  }
  if (is.null(subset2)) {
    return(subset1)
  }
  if (is.logical(subset1) && is.logical(subset2)) {
    return(subset1 & subset2)
  } else if (is.integer(subset1) && is.integer(subset2)) {
    return(intersect(subset1, subset2))
  } else {
    sl <- list(subset1, subset2)
    sl <- lapply(sl, function(s) {
      if (is.logical(s)) {
        which(s)
      } else {
        s
      }
    })
    subset1 <- sl[[1L]]
    subset2 <- sl[[2L]]
    return(intersect(subset1, subset2))
  }
}




specification_dataset_source <- function(dataset_name) {
  dbc::assert_is_character_nonNA_atom(dataset_name)
  url_prefix <- paste0(
    "https://raw.githubusercontent.com/CancerRegistryOfNorway/NORDCAN/master/",
    "specifications/"
  )
  switch(
    dataset_name,
    stop("No source defined for dataset_name = ", deparse(dataset_name)),
    entity_usage_info = paste0(url_prefix, "entity_usage_info.csv"),
    icd10_to_entity = paste0(url_prefix, "icd10_to_entity_columns.csv"),
    icd10_vs_icd67_icd8_icd9 = paste0(url_prefix,
                                      "icd10_vs_icd67_icd8_icd9.csv"),
    regions = paste0(url_prefix, "regions.csv")
  )
}





#' @title Code Documentation
#' @description
#' Using package **codedoc** capabilities, extract documentation blocks from
#' your source code (using [codedoc::extract_keyed_comment_blocks_])
#' to be added to the R documentation for that object.
#' @param text_file_paths `[character]` (mandatory, no default)
#'
#' passed to [codedoc::extract_keyed_comment_blocks_]
#' @param regex `[character]` (mandatory, no default)
#'
#' regular expression used to identify which comment blocks to retain;
#' see [codedoc::extract_keyed_comment_blocks_]; the regex is applied to
#' retain only those results that pertain to the keys that match the regex
#' @param tag `[character]` (mandatory, default `"@details"`)
#'
#' tag under which the extracted text should appear
#' @param extract_arg_list `[list]` (optional, default `list()`)
#'
#' additional args passed to [codedoc::extract_keyed_comment_blocks_], other
#' than `text_file_paths`
#'
#' @param grepl_arg_list `[list]` (optional, default `list(perl = TRUE)`)
#'
#' additional args passed to [base::grepl], other than `pattern` and `x`.
#'
#' @details
#'
#' so you have a roxygen block for your object you want to document. to
#' include programmatically generated lines into the doc, use the `@eval`
#' tag like so:
#'
#' `#' @eval nordcancore::object_code_documentation("R/my_script.R", "my_obj")`
#' @export
object_code_documentation <- function(
  text_file_paths,
  regex,
  tag = "@details",
  extract_arg_list = list(),
  grepl_arg_list = list(perl = TRUE)
  ) {
  dbc::assert_user_input_file_exists(text_file_paths)
  dbc::assert_user_input_is_character_nonNA_atom(regex)
  dbc::assert_user_input_is_character_nonNA_atom(tag)
  dbc::assert_user_input_is_list(extract_arg_list)
  dbc::assert_user_input_is_list(grepl_arg_list)

  extract_arg_list[["text_file_paths"]] <- text_file_paths
  df <- do.call(codedoc::extract_keyed_comment_blocks_, extract_arg_list)

  grepl_arg_list[["pattern"]] <- regex
  grepl_arg_list[["x"]] <- df[["key"]]
  is_match <- do.call(grepl, grepl_arg_list)
  df <- df[is_match, ]
  lines <- unlist(df[["comment_block"]])
  lines <- c(tag, "", lines)
  return(lines)
}




