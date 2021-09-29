counterfactual_plot_all <- function(predictor) {
  
  #'@title counterfactual_plot_all: graphs all counterfactuals for all weeks.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that plots counterfactuals for a given predictor at each week separately.
  #' 
  #'@usage counterfactual_plot(predictor)
  #'
  #'@param predictor string. Name of predictor.
  #'
  #'@details This function creates a counterfactual plot with all weeks overlapping.
  #'
  #'@return The plot is a graph, containing 4 counterfactual estimates for a predictor overlaping all
  #' weeks.
  #' 
  #'@examples
  #'counterfactual_plot_all("pss_c")
  #'counterfactual_plot_all("anxiety_c")
  
  # Set initial values to constant.
  rp_c = mean(cva_bayes$rp_c)
  pss_c = mean(cva_bayes$pss_c)
  anxiety_c = mean(cva_bayes$anxiety_c)
  depression_c = mean(cva_bayes$depression_c)
  week = cva_bayes$week
  age_c = mean(cva_bayes$age_c)
  
  # All columns are constant. One needs to be variable. The predictor argument determines which
  #predictors switches from constant to variable.
  if (predictor == "rp_c") {
    rp_c = seq(from = -5, to = 3, length.out = 601)
    lab_x = "RP Coefficient"
    } else if (predictor == "pss_c") {
      pss_c = seq(from = -5, to = 5, length.out = 601)
      lab_x = "PSS Coefficient"
      } else if (predictor == "anxiety_c") {
        anxiety_c = seq(from = -5, to = 5, length.out = 601)
        lab_x = "Anxiety Coefficient"
        } else if (predictor == "depression_c") {
          depression_c = seq(from = -5, to = 5, length.out = 601)
          lab_x = "Depression Coefficient"
          } else {
            age_c = seq(from = -5, to = 5, length.out = 601)
            lab_x = "Age Coefficient"
          }
  
  # Collect predictors in dataframe.
  nd <- tibble(
    rp_c = rp_c,
    pss_c = pss_c,
    anxiety_c = anxiety_c,
    depression_c = depression_c,
    week = week,
    age_c = age_c)
  
  # Combine predictors with marginal model estimates into 1 dataframe.
  aaaa <- fitted(covid_bayes_model_2_final, newdata = nd) %>% 
    as_tibble() %>% 
    rename(f_ll = Q2.5,
           f_ul = Q97.5) %>% 
    bind_cols(predict(covid_bayes_model_2_final, newdata = nd) %>% 
                as_tibble() %>% 
                transmute(p_ll = Q2.5, p_ul = Q97.5), nd)
  
  # Select only the predictor that the counterfactual is being estimated for.
  x = select(aaaa, Estimate, predictor, week, p_ll, p_ul, f_ll, f_ul)
  
  # Make graph for each week.
  graph <- ggplot(data = x, aes_string(x = predictor, y = "Estimate"))
  for (i in list("Week 1", "Week 2", "Week 3", "Week 4")) {
    graph <- graph + geom_ribbon(data = x[x$week == i, ],
                                 aes(ymin = p_ll, ymax = p_ul), fill = "firebrick", alpha = 1/5)
  }
  
  # Set additional graphing parameters.
  graph <- graph + 
    coord_cartesian(xlim = range(x[,2]), ylim = c(-3, 4)) +
    labs(subtitle = "Counterfactual plot: Week 1-4", y = "SRA", x = lab_x) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  # Return graph.
  return(graph)
  
}