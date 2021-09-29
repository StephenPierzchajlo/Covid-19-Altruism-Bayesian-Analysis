counterfactual_plot <- function(predictor) {
  
  #'@title counterfactual_plot: graphs for separate predictor counterfactuals.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that plots counterfactuals for a given predictor at each week separately.
  #' 
  #'@usage counterfactual_plot(predictor)
  #'
  #'@param predictor string. Name of predictor.
  #'
  #'@details This function creates a counterfactual plot. 
  #'
  #'@return The plot is a 2x2 grid, containing 4 plots, each being a counterfactual estimate for a
  #' predictor at 1 of 4 weeks.
  #' 
  #'@examples
  #'counterfactual_plot("pss_c")
  #'counterfactual_plot("anxiety_c")
  
  # Set initial values as constants.
  rp_c = mean(cva_bayes$rp_c)
  pss_c = mean(cva_bayes$pss_c)
  anxiety_c = mean(cva_bayes$anxiety_c)
  depression_c = mean(cva_bayes$depression_c)
  week = cva_bayes$week
  age_c = mean(cva_bayes$age_c)
  
  # All columns are constant. One needs to be variable. The predictor argument
  # determines which predictors switches from constant to variable.
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
  
  # Empty list to store graphs.
  graph_list <- list()
  
  # Store a graph for each week in list index graph_list[[i]].
  for (i in c("Week 1", "Week 2", "Week 3", "Week 4")) {
    graph_list[[i]] <- ggplot(data = x[x$week == i, ], aes_string(x = predictor, y = "Estimate")) +
      geom_ribbon(aes(ymin = p_ll, ymax = p_ul), fill = "firebrick", alpha = 1/5) +
      geom_smooth(aes(
        ymin = f_ll, ymax = f_ul
        ),
        stat = "identity", fill = "firebrick", color = "firebrick4", alpha = 1/5, size = 1/4) +
      coord_cartesian(xlim = range(select(cva_bayes, predictor)), ylim = c(-3, 4)) +
      labs(subtitle = paste0("Counterfactual plot:", i), y = "Self-Reported Altruism", x = lab_x) +
      theme_bw() +
      theme(panel.grid = element_blank())
  }
  
  # Create 2x2 plot.
  graph <- annotate_figure(ggarrange(graph_list[[1]],
                                     graph_list[[2]],
                                     graph_list[[3]],
                                     graph_list[[4]],
                                     ncol = 2, nrow = 2),
                           top = text_grob(
                             "Risk Perception", color = "black", face = "bold", size = 14
                             ))
  
  # Return graph.
  return(graph)
  
  }