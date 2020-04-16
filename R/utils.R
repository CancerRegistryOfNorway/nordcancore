




#' @title Package Data
#' @description Utilities to access datasets contained in R packages.
#' @name package_data_utils
#' @param package_name `[character]` (mandatory, no default)
#'
#' name of R package





#' @export
#' @rdname package_data_utils
#' @importFrom utils data
get_exported_dataset_names <- function(package_name) {
  easyassertions::assert_is_character_nonNA_atom(package_name)
  dataset_names <- utils::data(package = package_name)
  dataset_names[["results"]][, "Item"]
}
#' @export
#' @rdname package_data_utils
get_internal_dataset_names <- function(package_name) {
  easyassertions::assert_is_character_nonNA_atom(package_name)
  # exported datasets are not in namespace!
  ns <- getNamespace(package_name)
  obj_nms <- names(ns)
  is_df <- vapply(obj_nms, function(obj_nm) is.data.frame(ns[[obj_nm]]),
                  logical(1))
  obj_nms[is_df]
}




#' @export
#' @rdname package_data_utils
#' @param dataset_name `[character]` (mandatory, no default)
#'
#' name of dataset to retrieve.
get_exported_dataset <- function(dataset_name, package_name) {
  easyassertions::assert_is_character_nonNA_atom(dataset_name)
  easyassertions::assert_is_character_nonNA_atom(package_name)

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
get_internal_dataset <- function(dataset_name, package_name) {
  easyassertions::assert_is_character_nonNA_atom(dataset_name)
  easyassertions::assert_is_character_nonNA_atom(package_name)

  dataset_nms <- get_internal_dataset_names(package_name)
  if (!dataset_name %in% dataset_nms) {
    stop("Internal error: requested internal dataset ",
         deparse(dataset_name), " is not one of the internal datasets ",
         deparse(dataset_nms), " in package ", package_name,
         ". If you see this, please complain to the ",
         "package maintainer ", utils::maintainer(package_name))
  }

  eval(parse(text = paste0(package_name, ":::", dataset_name)))
}
