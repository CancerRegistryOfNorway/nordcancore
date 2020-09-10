
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
#' - `nordcan_column_specifications`: a `list` of specifications; elements of
#'   the list vary by format,
#'   but they all have element `format` (character string)
nordcan_column_specifications <- function(column_name) {
  dbc::assert_is_character_nonNA_atom(column_name)
  if (!column_name %in% names(column_specification_list)) {
    stop("No specifications for column named ", deparse(column_name))
  }
  column_specification_list[[column_name]]
}

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

column_specification_list <- list(
  pat           = list(format = "String"),

  date_of_birth = list(format = "Date",
                       min = as.Date("1800-01-01"),
                       max = Sys.Date()),

  flag_dob      = list(format = "Categorical",
                       levels = c("Exact date of birth" = 0, "Set date of birth" = 1)),

  age           = list(format = "Numeric",
                       min = 0, max = 121),

  sex           = list(format = "Categorical",
                       levels = c("Male" = 1,  "Female" = 2, "Other & unknown" = 3)),

  region        = list(format = "Region",
                       table = data.frame(Description = c("Denmark", "North Jutland", "Central Jutland", "Southern Denmark", "Greater Copenhagen",
                                                          "Zealand", "Faroe Islands", "Finland", "Helsinki", "Kuopio", "Oulu", "Tampere", "Turku",
                                                          "Iceland", "Reykjavik-Reykjanes", "Rural", "Norway", "Central", "Northern", "Southeastern",
                                                          "Western", "Sweden", "Northern", "Stockholm-Gotland", "Southern", "Southeastern",
                                                          "Uppsala-\u00d6rebro", "Western", "Greenland", "Nordic countries"),
                                          Value = c("10", "11", "12", "13", "14", "15", "20", "30", "31", "32", "33", "34", "35", "40", "41", "42",
                                                    "50", "51", "52", "53", "54", "60", "61", "62", "63", "64", "65", "66", "80", "90"))),


  # geo_code
  # geo_label

  tum  = list(format = "ID"),

  tum_sequence = list(format = "Numeric",
                      min = 1, max = NULL),

  date_of_incidence = list(format = "Date",
                           min = as.Date("1800-01-01"),
                           max = Sys.Date()),

  flag_doi  = list(format = "Categorical",
                   levels = c("Exact date of incidence" = 0, "Set date of incidence" = 1)),

  bod  = list(format = "Categorical",
              levels = c("Death certificate only (DCO)" = 0,
                         "Clinical" = 1,
                         "Clinical investigation" = 2,
                         "Specific tumor markers" = 4,
                         "Cytology" = 5,
                         "Histology of a metastasis" = 6,
                         "Histology of a primary tumor" = 7,
                         "Unknown" = 9)),

  topo    = list(format = "Other", message = "Non-valid values will be picked up by the IARCcrgCheckTool"),
  morpho  = list(format = "Other", message = "Non-valid values will be picked up by the IARCcrgCheckTool"),



  beh  = list(format = "Categorical",
              levels = c("Benign neoplasm" = 0,
                         "Neoplasm of uncertain and unknown behaviour (uncertain whether benign or malignant, \"borderline\" malignant, incipient cancer, bladder papilloma" = 1,
                         "In-situ neoplasm (carcinoma in situ/non invasive/preleukaemia/intraepithelial tumor)" = 2,
                         "Malignant neoplasm" = 3)),




  grade  = list(format = "Categorical",
                levels = c("Death certificate only (DCO)" = 0,
                           "Well differentiated" = 1,
                           "Moderately differentiated" = 2,
                           "Poorly differentiated" = 3,
                           "Undifferentiated, anaplastic" = 4,
                           "T-cell, T-precursor" = 5,
                           "B-cell, Pre-B, B-precursor" = 6,
                           "Null cell, Non T-non B" = 7,
                           "NK-cell (natural killer cell)" = 8,
                           "Unknown or not applicable" = 9)),


  laterality   = list(format = "Categorical",
                      levels = c("Not applicable" = 0,
                                 "Right" = 1,
                                 "Left" = 2,
                                 "Unilateral NOS" = 3,
                                 "Bilateral" = 4,
                                 "Unknown" = 9)),


  autopsy = list(format = "Categorical",
                 levels = c("No (not found at autopsy)" = 0,
                            "Yes (found at autopsy)" = 1,
                            "Unknown" = 9)),






  vit_sta = list(format = "Categorical",
                 levels = c("Alive" = 1,
                            "Dead" = 2,
                            "Lost to follow up/censored (NORDCAN only)" = 3,
                            "Unknown" = 9)),


  end_of_followup = list(format = "Date",
                         min = as.Date("1800-01-01"),
                         max = Sys.Date()),


  flag_eof  = list(format = "Categorical",
                   levels = c("Exact date  for end of follow up" = 0, "Set date  for end of follow up" = 1)),


  cod   = list(format = "Categorical",
               levels = c("Blank" = "", "Unknown" = "R99")),


  icd   = list(format = "Categorical",
               levels = c(1:11,  "Unknown" = "99")),



  tnm_ed = list(format = "Categorical",
                levels = c("Unknown" = "R99")),


  ct = list(format = "Other", message = "JRC Quality Check software will identify non-valid values"),
  cn = list(format = "Other", message = "JRC Quality Check software will identify non-valid values"),
  cm = list(format = "Other", message = "JRC Quality Check software will identify non-valid values"),
  pt = list(format = "Other", message = "JRC Quality Check software will identify non-valid values"),
  pn = list(format = "Other", message = "JRC Quality Check software will identify non-valid values"),
  pm = list(format = "Other", message = "JRC Quality Check software will identify non-valid values"),



  tos   = list(format = "Categorical",
               levels = c("Ann Arbor / Lugano stage" = "A",
                          "Dukes' stage" = "D",
                          "Extent of disease" = "E" ,
                          "FIGO stage" =  "F",
                          "TNM stage, unknown whether clinical or pathological" =  "S",
                          "Clinical TNM stage" = "cIS",
                          "Pathological TNM stage" =  "paS",
                          "Combination of clinical & pathological TNM stage" = "cpS",
                          "Condensed TNM stage" = "coS",
                          "Essential TNM stage" =  "esS",
                          "Tier 1 stage for paediatric tumours" = "T1",
                          "Tier 2 stage for paediatric tumours" = "T2",
                          "Other system" = "8",
                          "Unknown" = "9")),


  stage = list(format = "Categorical",
               levels = c("Stage 0, stage 0a, stage 0is, carcinoma in situ, non-invasive" = 0,
                          "Stage I, FIGO I, localized, localized limited (L), limited, Dukes A" = 1,
                          "Stage II, FIGO II, localized advanced (A), locally advanced, advanced, direct extension, Dukes B" = 2,
                          "Stage III, FIGO III, regional (with or without direct extention), R+, N+, Dukes C" = 3,
                          "Stage IV, FIGO IV, metastatic, distant, M+, Dukes D" = 4,
                          "Unknown" = 9)),


  surgery = list(format = "Categorical",
                 levels = c("No" = 0,
                            "Yes, without specification" = 1,
                            "Yes, local surgery only" = 2,
                            "Yes, 'operative' surgery" = 3,
                            "Unknown" = 9)),


  rt = list(format = "Categorical",
            levels = c("No" = 0,
                       "Yes, without specification" = 1,
                       "Yes, neoadjuvant (pre-operative) radiotherapy" = 2,
                       "Yes, adjuvant (post-operative) radiotherapy" = 3,
                       "Unknown" = 9)),


  cht = list(format = "Categorical",
             levels = c("No" = 0,
                        "Yes, without other specification" = 1,
                        "Yes, neoadjuvant (pre-operative)" = 2,
                        "Yes, adjuvant (post-operative)" = 3,
                        "Yes, both neoadjuvant and adjuvant" = 4,
                        "Unknown" = 9)),



  tt  = list(format = "Categorical",
             levels = c("No" = 0,
                        "Yes" = 1,
                        "Unknown" = 9)),


  it = list(format = "Categorical",
            levels = c("No" = 0,
                       "Yes" = 1,
                       "Unknown" = 9)),

  ht = list(format = "Categorical",
            levels = c("No" = 0,
                       "Yes" = 1,
                       "Unknown" = 9)),


  ot  = list(format = "Categorical",
             levels = c("No" = 0,
                        "Yes, without other specification" = 1,
                        "Yes, neoadjuvant (pre-operative)" = 2,
                        "Yes, adjuvant (post-operative)" = 3,
                        "Unknown" = 9)),


  sct = list(format = "Categorical",
             levels = c("No" = 0,
                        "Yes" = 1,
                        "Unknown" = 9))
  ,
  mob = list(
    format = "Categorical",
    levels = month_level_space()
  ),
  yob = list(
    format = "Categorical",
    levesl = year_level_space()
  ),
  moi = list(
    format = "Categorical",
    levels = month_level_space()
  ),
  yoi = list(
    format = "Categorical",
    levels = year_level_space()
  ),
  mof = list(
    format = "Categorical",
    levels = month_level_space()
  ),
  yof = list(
    format = "Categorical",
    levels = year_level_space()
  ),
  surv_time =  list(
    format = "Integer",
    min = 0L, max = as.integer(100L * 365L)
  ),
  icd10 = list(
    format = "ICD-10"
  ),
  entity = list(
    format = "Entity",
    table = local({
      dt <- nordcan_entity_levels_by_icd10_and_sex()
      dt <- dt[
        j = .SD,
        .SDcols = nordcan_column_name_set("column_name_set_entity")
        ]
      unique(dt, by = names(dt))
    })
  ),
  agegroup = list(
    format = "Integer",
    min = 1L, max = 18L
  ),
  agr_all_ages = list(
    format = "Integer",
    min = 1L, max = 5L
  ),
  agr_bone = list(
    format = "Integer",
    min = 1L, max = 5L
  ),
  agr_all_sites = list(
    format = "Integer",
    min = 1L, max = 5L
  ),
  period = list(
    format = "Integer",
    min = 1800L, max = data.table::year(Sys.Date())
  ),
  excl_imp_error = list(
    format = "String"
  ),
  excl_imp_icd10conversion = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_imp_benign = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_imp_duplicate = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_imp_entitymissing = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_imp_total = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_surv_dco = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_surv_autopsy = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_surv_negativefou = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_surv_zerofou = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  excl_surv_total = list(
    format = "Categorical",
    levels = c(Included = 0L, excluded = 1L)
  ),
  clinical_stage = list(
    format = "String"
  ),
  pathological_stage = list(
    format = "String"
  ),
  tnm_group = list(
    format = "Integer",
    min = 0L, max = 100L
  )

)
column_specification_list[nordcan_column_name_set("column_name_set_entity")] <-
  lapply(nordcan_column_name_set("column_name_set_entity"), function(col_nm) {
    unique(column_specification_list[["entity"]][["table"]][[col_nm]])
  })



