posterior_plot <- function(parameter, fixed, title) {
  
  #'@title posterior_plot: Plot posterior distribution and credibility interval.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that plots cposterior distribution and credibility intervals for a
  #'parameter.
  #' 
  #'@usage posterior_plot(parameter, fixed, title) 
  #'
  #'@param parameter character. Name of predictor.
  #'@param fixed dbl. Row predictor is located at.
  #'@param title string. Name of graph title.
  #'
  #'@details This function creates a posterior distribution for a parameter. Parameters must
  #'be taken from a tidyMCMC object.
  #'
  #'@return The plot is a graph containing a posterior distribution.
  #' 
  #'@examples
  #'posterior_plot(b_rp_c, 2, "Risk Perception")

  # Add quotation marks around parameter arguement.
  parameter <- rlang::quo_name(rlang::enquo(parameter))
  
  # Make posterior grpah.
  ggplot(filter(model1tranformed, Parameter == parameter, Iteration > 2000), aes(x = value)) +
  geom_density(fill  = "yellow", alpha = .5) +
  geom_vline(xintercept = 0, col  = "red", size = 1) +
  scale_x_continuous(name = "Coefficient", limits = c(-1, 1)) + 
  geom_vline(
    xintercept = summary(covid_bayes_model_2_final)$fixed[fixed, 3:4], col = "blue", linetype = 2
    ) +
  theme_light() +
  labs(title = title) +
  theme(plot.title = element_text(size = 8, hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))
  
  }