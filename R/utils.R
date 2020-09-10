




#' @title Package Data
#' @description Utilities to access datasets contained in R packages.
#' @name package_data_utils
#' @param package_name `[character]` (mandatory, no default)
#'
#' name of R package





#' @export
#' @rdname package_data_utils
#' @importFrom utils data
#' @importFrom dbc assert_is_character_nonNA_atom
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
#' @importFrom dbc assert_is_character_nonNA_atom
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
#' @importFrom data.table is.data.table
#' @importFrom utils maintainer
#' @importFrom dbc assert_is_character_nonNA_atom
get_internal_dataset <- function(dataset_name, package_name) {
  dbc::assert_is_character_nonNA_atom(dataset_name)
  dbc::assert_is_character_nonNA_atom(package_name)

  obj <- tryCatch(
    eval(parse(text = paste0(package_name, ":::", dataset_name))),
    error = function(e) e
  )

  if (inherits(obj, "try-error")) {
    stop("Internal error: requested internal dataset ",
         deparse(dataset_name), " is not one of the internal datasets ",
         " in package ", package_name,
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
#' - `nordcan_column_name_set_names`: a `character` vector of names of all
#'   column name sets
#' @export
nordcan_column_name_set_names <- function() {
  dt <- get_internal_dataset("nordcan_columns", package_name = "nordcancore")
  column_name_set_names <- names(dt)[grepl("^column_name_set_", names(dt))]
  column_name_set_names
}

#' @rdname nordcan_columns
#' @param column_name_set_name `[character]` (mandatory, no default)
#'
#' name of column name, one of the elements from output of
#' `nordcan_column_name_set_names()`; causes all column names for this set to
#' be retrieved
#' @return
#' - `nordcan_column_name_set`: a `character` vector of names in queried set
#' @export
#' @importFrom dbc assert_is_character_nonNA_atom assert_atom_is_in_set
nordcan_column_name_set <- function(column_name_set_name) {
  dbc::assert_is_character_nonNA_atom(column_name_set_name)
  dt <- get_internal_dataset("nordcan_columns", package_name = "nordcancore")
  dbc::assert_atom_is_in_set(
    column_name_set_name, set = nordcan_column_name_set_names()
  )

  col_nm_set <- dt[["Variable name"]]
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
nordcan_dataset_names <- function() {
  nms <- nordcan_column_name_set_names()
  nms[grepl("_dataset$", nms)]
}

#' @title NORDCAN IARC CRG Tools Tool Names
#' @description Retrieve names of tools of IARC CRG Tools supported by the
#' NORDCAN R framework.
#' @export
nordcan_iarccrgtools_tool_names <- function() {
  nms <- nordcan_column_name_set_names()
  nms <- nms[grepl("^column_name_set_iarccrgtools", nms)]
  nms <- sub("^column_name_set_iarccrgtools_((all)|(mandatory))_", "", nms)
  nms <- unique(nms)
  nms
}


