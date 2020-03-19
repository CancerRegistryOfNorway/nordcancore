




#' @title Package Data
#' @description Utilities to access datasets contained in R packages.
#' @name package_data_utils





#' @export
#' @rdname package_data_utils
#' @importFrom utils data
get_exported_dataset_names <- function() {
  requireNamespace("utils")
  dataset_names <- utils::data(package = "nordcancore")
  dataset_names[["results"]][, "Item"]
}
#' @export
#' @rdname package_data_utils
get_internal_dataset_names <- function() {
  # exported datasets are not in namespace!
  ns <- getNamespace("nordcancore")
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
get_exported_dataset <- function(dataset_name) {
  stopifnot(
    length(dataset_name) == 1,
    is.character(dataset_name),
    !is.na(dataset_name)
  )

  expo_data_nms <- get_exported_dataset_names()
  if (!dataset_name %in% expo_data_nms) {
    stop(
      "requested exported dataset ",
      deparse(dataset_name), " is not one of ",
      deparse(expo_data_nms)
    )
  }
  getExportedValue(ns = "nordcancore", name = dataset_name)
}





#' @export
#' @rdname package_data_utils
#' @importFrom data.table is.data.table
#' @importFrom utils maintainer
get_internal_dataset <- function(dataset_name) {
  stopifnot(
    length(dataset_name) == 1,
    is.character(dataset_name),
    !is.na(dataset_name)
  )

  dataset_nms <- get_internal_dataset_names()
  if (!dataset_name %in% dataset_nms) {
    stop("Internal error: requested internal dataset ",
         deparse(dataset_name), " is not one of ",
         deparse(dataset_nms), ". If you see this, complain to the ",
         "package maintainer ", utils::maintainer("nordcancore"))
  }

  eval(parse(text = paste0("nordcancore:::", dataset_name)))
}
