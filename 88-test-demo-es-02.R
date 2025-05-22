
dat

linelist::tags_names()
linelist::tags_types()
linelist::lost_tags_action(action = "error")

dat_validated <- out %>% 
  # dplyr::mutate(date_onset = as.character(date_onset)) %>% 
  linelist::make_linelist(
    id = "case_id",
    gender = "gender",
    date_onset = "date_onset",
    date_reporting = "date_sample"
  ) %>% 
  linelist::validate_linelist() %>% 
  # dplyr::select(case_id)
  linelist::tags_df()

dat_validated
