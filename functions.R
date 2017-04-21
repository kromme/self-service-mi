

plot_variable_against_selection <- function(t_var, workingset ){
  
  t_var_class = class(as.data.frame(workingset)[,t_var])
  
  # plot categorical
  if (t_var_class%in% c('factor', 'character')){
    g <- ggplot(workingset, aes_string('selected', fill = t_var ) ) + geom_bar(position = 'fill') 
      #scale_y_continuous(scales::percent)
  }
  
  # plot numerical
  if (t_var_class %in% c('numeric', 'integer')){
    g <-  workingset %>% group_by(selected) %>% 
      summarise_(mean  = paste0('mean(',t_var,', na.rm = TRUE)')) %>% 
      ggplot(., aes(selected, mean, fill = selected ) ) + geom_bar(stat = 'identity')+ 
      theme(legend.position="none")
      
  }
  
  g <- g  +
    ggtitle(t_var)+
    theme(plot.background = element_rect(fill = '#ECF0F5', colour = '#ECF0F5')) +
    theme(panel.background = element_rect(fill = '#ECF0F5', colour = '#ECF0F5'))
  
  return(g)
}


log_usage <- function(tolog, file = 'logfile.txt'){
  value = paste0(Sys.time(),';', tolog)
  write.table(value, file, append = TRUE, col.names = F, row.names = F, quote = FALSE)
  cat(value)
}

