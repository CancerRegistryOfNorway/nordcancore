
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


#' @title Assertions
#' @description
#' Assertions specific to NORDCAN metadata.
#' @param x `[R object]`
#'
#' requirements vary.
#' - `assert_prod_input_entities`: an `integer` vector of valid entity numbers
#' - `assert_user_input_entities`: an `integer` vector of valid entity numbers
#' @name assertions

#' @rdname assertions
#' @export
assert_prod_input_entities <- function(x) {
  dbc::assert_prod_input_is_integer_nonNA_vector(x, x_nm = "entities")
  dbc::assert_prod_input_vector_elems_are_in_set(
    x, x_nm = "entities",
    set = nordcan_metadata_entity_no_set("all")
  )
}
#' @rdname assertions
#' @export
assert_user_input_entities <- function(x) {
  dbc::assert_user_input_is_integer_nonNA_vector(x, x_nm = "entities")
  dbc::assert_user_input_vector_elems_are_in_set(
    x, x_nm = "entities",
    set = nordcan_metadata_entity_no_set("all")
  )
}

