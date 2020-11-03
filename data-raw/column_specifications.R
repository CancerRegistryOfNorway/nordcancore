
devtools::load_all()

column_specification_list <- list(
  pat           = list(format = "String"),

  date_of_birth = list(format = "Date",
                       min = as.Date("1800-01-01"),
                       max = Sys.Date()),

  flag_dob      = list(format = "Categorical",
                       levels = c("Exact date of birth" = 0L, "Set date of birth" = 1L)),

  age           = list(format = "Numeric",
                       min = 0, max = 121),

  sex           = list(format = "Categorical",
                       levels = c("Male" = 1L,  "Female" = 2L, "Other & unknown" = 3L)),

  region        = list(format = "Categorical",
                       levels = {
                         dt <- get_internal_dataset("regions", "nordcancore")
                         structure(
                           dt[["region_no"]], names = dt[["region_name"]]
                         )
                       }),

  geo_code = list(format = "String"),

  geo_label = list(format = "String"),

  tum  = list(format = "ID"),

  tum_sequence = list(format = "Numeric",
                      min = 1L, max = 50L),

  date_of_incidence = list(format = "Date",
                           min = as.Date("1800-01-01"),
                           max = Sys.Date()),

  flag_doi  = list(format = "Categorical",
                   levels = c("Exact date of incidence" = 0L, "Set date of incidence" = 1L)),

  bod  = list(format = "Categorical",
              levels = c("Death certificate only (DCO)" = 0L,
                         "Clinical" = 1L,
                         "Clinical investigation" = 2L,
                         "Specific tumor markers" = 4L,
                         "Cytology" = 5L,
                         "Histology of a metastasis" = 6L,
                         "Histology of a primary tumor" = 7L,
                         "Unknown" = 9L)),

  topo    = list(format = "Other", message = "Non-valid values will be picked up by the IARCcrgCheckTool"),
  morpho  = list(format = "Other", message = "Non-valid values will be picked up by the IARCcrgCheckTool"),



  beh  = list(format = "Categorical",
              levels = c("Benign neoplasm" = 0L,
                         "Neoplasm of uncertain and unknown behaviour (uncertain whether benign or malignant, \"borderline\" malignant, incipient cancer, bladder papilloma" = 1L,
                         "In-situ neoplasm (carcinoma in situ/non invasive/preleukaemia/intraepithelial tumor)" = 2L,
                         "Malignant neoplasm" = 3L)),




  grade  = list(format = "Categorical",
                levels = c("Death certificate only (DCO)" = 0L,
                           "Well differentiated" = 1L,
                           "Moderately differentiated" = 2L,
                           "Poorly differentiated" = 3L,
                           "Undifferentiated, anaplastic" = 4L,
                           "T-cell, T-precursor" = 5L,
                           "B-cell, Pre-B, B-precursor" = 6L,
                           "Null cell, Non T-non B" = 7L,
                           "NK-cell (natural killer cell)" = 8L,
                           "Unknown or not applicable" = 9L)),


  laterality   = list(format = "Categorical",
                      levels = c("Not applicable" = 0L,
                                 "Right" = 1L,
                                 "Left" = 2L,
                                 "Unilateral NOS" = 3L,
                                 "Bilateral" = 4L,
                                 "Unknown" = 9L)),


  autopsy = list(format = "Categorical",
                 levels = c("No (not found at autopsy)" = 0L,
                            "Yes (found at autopsy)" = 1L,
                            "Unknown" = 9L)),






  vit_sta = list(format = "Categorical",
                 levels = c("Alive" = 1L,
                            "Dead" = 2L,
                            "Lost to follow up/censored (NORDCAN only)" = 3L,
                            "Unknown" = 9L)),


  end_of_followup = list(format = "Date",
                         min = as.Date("1800-01-01"),
                         max = Sys.Date()),


  flag_eof  = list(format = "Categorical",
                   levels = c("Exact date  for end of follow up" = 0L, "Set date  for end of follow up" = 1L)),


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
               levels = c("Stage 0, stage 0a, stage 0is, carcinoma in situ, non-invasive" = 0L,
                          "Stage I, FIGO I, localized, localized limited (L), limited, Dukes A" = 1L,
                          "Stage II, FIGO II, localized advanced (A), locally advanced, advanced, direct extension, Dukes B" = 2L,
                          "Stage III, FIGO III, regional (with or without direct extention), R+, N+, Dukes C" = 3L,
                          "Stage IV, FIGO IV, metastatic, distant, M+, Dukes D" = 4L,
                          "Unknown" = 9L)),


  surgery = list(format = "Categorical",
                 levels = c("No" = 0L,
                            "Yes, without specification" = 1L,
                            "Yes, local surgery only" = 2L,
                            "Yes, 'operative' surgery" = 3L,
                            "Unknown" = 9L)),


  rt = list(format = "Categorical",
            levels = c("No" = 0L,
                       "Yes, without specification" = 1L,
                       "Yes, neoadjuvant (pre-operative) radiotherapy" = 2L,
                       "Yes, adjuvant (post-operative) radiotherapy" = 3L,
                       "Unknown" = 9L)),


  cht = list(format = "Categorical",
             levels = c("No" = 0L,
                        "Yes, without other specification" = 1L,
                        "Yes, neoadjuvant (pre-operative)" = 2L,
                        "Yes, adjuvant (post-operative)" = 3L,
                        "Yes, both neoadjuvant and adjuvant" = 4L,
                        "Unknown" = 9L)),



  tt  = list(format = "Categorical",
             levels = c("No" = 0L,
                        "Yes" = 1L,
                        "Unknown" = 9L)),


  it = list(format = "Categorical",
            levels = c("No" = 0L,
                       "Yes" = 1L,
                       "Unknown" = 9L)),

  ht = list(format = "Categorical",
            levels = c("No" = 0L,
                       "Yes" = 1L,
                       "Unknown" = 9L)),


  ot  = list(format = "Categorical",
             levels = c("No" = 0L,
                        "Yes, without other specification" = 1L,
                        "Yes, neoadjuvant (pre-operative)" = 2L,
                        "Yes, adjuvant (post-operative)" = 3L,
                        "Unknown" = 9L)),


  sct = list(format = "Categorical",
             levels = c("No" = 0L,
                        "Yes" = 1L,
                        "Unknown" = 9L))
  ,
  mob = list(
    format = "Categorical",
    levels = month_level_space()
  ),
  yob = list(
    format = "Categorical",
    levels = year_level_space()
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
  agegroup = list(
    format = "Categorical",
    levels = c(1:18, 21L)
  ),
  agr_all_ages = list(
    format = "Categorical",
    levels = 1L
  ),
  agr_bone = list(
    format = "Categorical",
    levels = 1:6
  ),
  agr_all_sites = list(
    format = "Categorical",
    levels = 1:6
  ),
  period_5 = list(
    format = "Categorical",
    levels = 1800L:data.table::year(Sys.Date())
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
  excl_surv_age = list(
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
  ),
  year = list(
    format = "Categorical",
    levels = year_level_space()
  ),
  icd_code = list(
    format = "String"
  ),
  icd_version = list(
    format = "String"
  ),
  cancer_death_count = list(
    format = "Integer",
    min = 0L, max = 1e6L
  ),
  pop_endofyear = list(
    format = "Integer",
    min = 0L, max = 1e9L
  ),
  pop_midyear = list(
    format = "Numeric",
    min = 0.0, max = 1e9
  ),
  prob = list(
    format = "Numeric",
    min = 0.0, max = 1.0
  )
)




joint_categorical_column_spaces <- local({
  formats <- vapply(column_specification_list, function(obj) {
    if ("format" %in% names(obj)) {
      obj[["format"]]
    } else {
      NA_character_
    }
  },
  character(1L))
  categ_col_nms <- names(column_specification_list)[formats %in% "Categorical"]
  level_spaces <- lapply(categ_col_nms, function(col_nm) {
    obj <- column_specification_list[[col_nm]]
    if (!"levels" %in% names(obj)) {
      stop("obj did not have levels; is col_nm = ", col_nm, " categorical ",
           "and does it have levels defined?")
    }
    dt <- data.table::data.table(V1 = unname(obj[["levels"]]))
    data.table::setnames(dt, "V1", col_nm)
    dt[]
  })
  names(level_spaces) <- categ_col_nms
  level_spaces["sex"] <- NULL

  sex_entity_dt <- nordcan_metadata_entity_by_sex()
  sex_entity_dt <- rbind(
    data.table::set(sex_entity_dt[sex_entity_dt$sex == 0L, ], j = "sex",
                    value = 1L)[],
    data.table::set(sex_entity_dt[sex_entity_dt$sex == 0L, ], j = "sex",
                    value = 2L)[],
    sex_entity_dt[sex_entity_dt$sex == 1L, ],
    sex_entity_dt[sex_entity_dt$sex == 2L, ]
  )
  sex_entity_dt <- sex_entity_dt[sex_entity_dt$entity_level_30 != 999L, ]

  data.table::setDT(sex_entity_dt)
  data.table::setkeyv(sex_entity_dt, names(sex_entity_dt))
  level_spaces[["sex_entity"]] <- sex_entity_dt
  out <- data.table::data.table(
    col_nm_set = lapply(level_spaces, names),
    joint_level_space = level_spaces
  )
  out[]
})

column_specification_list[entity_column_names()] <-
  lapply(entity_column_names(), function(col_nm) {
    dt <- nordcancore::nordcan_metadata_entity_by_sex()
    list(
      format = "Categorical",
      levels = unique(dt[[col_nm]])
    )
  })

column_specification_list[["entity"]] <- list(
  format = "Categorical",
  levels = unique(unlist(column_specification_list[entity_column_names()]))
)
