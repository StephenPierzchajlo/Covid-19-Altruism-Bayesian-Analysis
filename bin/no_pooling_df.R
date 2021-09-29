no_pooling_df <- function(model, week){
  
  # give numerical argument a string.
  week <- ifelse(week == 1, "week 1",
                 ifelse(week == 2, "week 2",
                        ifelse(week == 3, "week 3",
                               "week 4")))
  
  # Make dataframe.
  df <- data_frame(
    model = "no pooling",
    week = week,
    intercept = fixef(model)[1], 
    slope_rp_c = fixef(model)[2],
    slope_pss_c = fixef(model)[3],
    slope_anxiety_c = fixef(model)[4],
    slope_depression_c = fixef(model)[5],
    slope_age_c = fixef(model)[6]
  )
  
  return(df)
  
  }