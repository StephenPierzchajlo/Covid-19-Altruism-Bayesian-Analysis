plot_correlations <- function(x.var, y.var){
  
  # Add quotes to function arguments.
  x.var <- rlang::quo_name(rlang::enquo(x.var))
  y.var <- rlang::quo_name(rlang::enquo(y.var))
  
  # Select columns from cva_full dataframe.
  x.var <- CVA_full[ , which(names(CVA_full) %in% c(x.var))]
  y.var <- CVA_full[ , which(names(CVA_full) %in% c(y.var))]
  
  # Create correlation graph.
  graph <- ggplot(cva_full, aes(x = x.var, y = y.var, color = Week)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE, color = "black", size = 0.5) +
    facet_wrap(~Week, nrow = 1) +
    scale_color_brewer(palette = "Pastel2") +
    theme_pubr() +
    theme(legend.position = "none", text = element_text(size = 10))
  
  # Return graph.
  return(graph)
  
  }