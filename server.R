library(TAL)
library(ggplot2)
library(RODBC)
library(dplyr)
library(caret)
library(plotly)
library(shiny)
source('functions.R')


analyses_results <- readRDS('results_db.rds')
customer_data <- TAL_autoload_data('00 Data/sauna_scv.csv',header = T, sep = ';', dec = '.')
customer_data$customer_id <- as.numeric(customer_data$customer_id)


log_usage('New start;')


function(input, output) {
  
  campaignIds <- as.character(unique(analyses_results$campaignId))
  campaigns <- unique(paste(analyses_results$campaignId , '-' ,  analyses_results$CampagneName))
  
  output$choose_analysis <- renderUI({
    selectInput("analysis", "Analyse", campaigns)
  })
  
  
  
  dataset <- reactive({
    
    analyse <- input$analysis
    top = as.numeric(sub("%", "", input$top))/100
    
    log_usage(paste0('top:',input$top, ';analyse:', analyse))
    
    cat(analyse, top)
    
    analyse_value <- strsplit( analyse, ' - ')[[1]][1]
    name_value <- strsplit(analyse, ' - ')[[1]][2]
    
    
    #
    dataset <- analyses_results %>%  
      filter( campaignId == analyse_value) %>% 
      inner_join(customer_data, by = c('id'= 'customer_id')) %>% 
      
      mutate(selected = ifelse(as.numeric(decile) <= top *10, paste0('Top ', top * 100, '%') ,paste0('Overige ', (1-top)* 100, '%'))) %>% 
      select(-c(campaignId,                 
                CampagneName,               
                idLevel,
                id ))
      
    
    dataset
  })
  
  
  output$choose_column <- renderUI({
    
    cols <- names(dataset())[!names(dataset()) %in% c('decile', 'prediction')]
    
    selectInput("column", "Kies variabele",cols)
    
  })
  
  
  
  
  
  
  
  output$decileplot <- renderPlotly({
    top = as.numeric(sub("%", "", input$top))/100
    
    gd <-dataset() %>% group_by(decile= as.factor(decile)) %>% summarise(median_kans = median(prediction)) %>% 
      ggplot(.,aes( decile, median_kans )) +geom_bar(stat = 'identity', alpha = 0.8) +
      ylab('Mediaan van voorspelling') + xlab('Decielen') +
      scale_y_continuous(labels = scales::percent) +
      geom_vline(xintercept = (top +0.05) * 10, colour = 'steelblue')+
      theme(plot.background = element_rect(fill = '#ECF0F5', colour = '#ECF0F5')) +
      theme(panel.background = element_rect(fill = '#ECF0F5', colour = '#ECF0F5'))
    
    ggplotly(gd)
    
  } )
  
  
  
  
  output$statText <- renderUI({
    analyse <- input$analysis
    top = as.numeric(sub("%", "", input$top))/100  
    # standaard cijfers  
    aantal = dataset() %>% filter(selected == paste0('Top ', top * 100, '%')) %>% nrow
    actief = dataset() %>% filter(selected == paste0('Top ', top * 100, '%')) %>% filter(churn == 0) %>% nrow / aantal

    # kansen
    ratio_mean = dataset() %>% filter(selected == paste0('Top ', top * 100, '%'))  %>% summarise(mean(prediction)) / dataset() %>% summarise(mean(prediction))
    ratio_other = dataset() %>% filter(selected == paste0('Top ', top * 100, '%'))  %>% summarise(mean(prediction)) / dataset() %>% filter(selected != paste0('Top ', top * 100, '%')) %>% summarise(mean(prediction))
    
    HTML(paste(
      analyse,
      paste0('Aantal klanten: ', aantal),
      paste0('Aantal actief: ', round(actief*100,0),'%'),
      paste0('Ratio hogere conversie t.o.v. gemiddelde: ', round(ratio_mean,2)),
      paste0('Ratio hogere conversie t.o.v. lagere groepen: ', round(ratio_other,2)),
      
      
      sep = '<br/>'))
    
    
  })
  
  
  
  
  
  output$graph <- renderPlotly({
    dataset <- dataset()
    column <- input$column
    
    
    g <- plot_variable_against_selection(column, dataset)
    ggplotly(g)
    
    
  })
  
  
  
  
}