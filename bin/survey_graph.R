survey_graph <- function(dat, x.var, fill, title){
  
  #'@title survey_graph: graphs for questionnaires.
  #'
  #'@author Stephen Pierzchajlo
  #'
  #'@description Function that plots response frequency for each questionnaire.
  #' 
  #'@usage survey_graph(dat, x.var, fill, title)
  #'
  #'@param dat character. Name of dataframe.
  #'@param x.var character. column name of questionnaire variable.
  #'@param fill character. what do proportions of the bar graph represent_ Here,
  #'always a number.
  #'@param title string. Title of graph. Always in quatations.
  #'
  #'@details This function creates a bar graph for questionnaire data collected
  #'in the COVID-19/Altruism study. All arguments must be inputed.
  #'
  #'@return A horizontal bar graph where the y-axis contains each item response, the
  #'x-axis is scaled from 0 to 1, and the bars are filled based on the proportion of
  #'participants responses to that questionnaire item.
  #' 
  #'@examples
  #'survey_graph(cva_sra_long, sra, answer, "Self-Reported Altruism Scale")
  
  # Add quotations to x.var and fill.
  x.var <- enquo(x.var)
  fill <- enquo(fill)
  
  # Create ggplot object.
  graph <- ggplot(dat, aes_string(x = x.var, fill = fill, na.rm = TRUE)) +
    geom_bar(position = position_fill(reverse = TRUE), na.rm = TRUE) +
    theme(text = element_text(size = 11)) +
    coord_flip() + 
    xlab("") +
    ylab("Number of responses") +
    scale_fill_brewer(type = "div") + 
    labs(fill = "Answer") +
    ggtitle(title)
  
  # Return graph.
  return(graph)
  
  }