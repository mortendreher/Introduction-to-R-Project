library(shiny)
library(dplyr)
library(ggplot2)

server = function(input, output){
  df <- read.csv("liver_cancer.csv", header=TRUE)
  scatter <- reactive(
    if(input$scat_var_group == 'None'){
      ggplot(data=df, mapping=aes(x=as.numeric(unlist(df[colnames(df) == input$scat_var_x])), y=as.numeric(unlist(df[colnames(df) == input$scat_var_y])))) +
        geom_point() +
        labs(title=paste0("Variables ", colnames(df[colnames(df) == input$scat_var_x]) , " and ",
                          colnames(df[colnames(df) == input$scat_var_y])),
             x=colnames(df[colnames(df) == input$scat_var_x]), 
             y=colnames(df[colnames(df) == input$scat_var_y]))
    }else{
    ggplot(data=df, mapping=aes(x=as.numeric(unlist(df[colnames(df) == input$scat_var_x])), y=as.numeric(unlist(df[colnames(df) == input$scat_var_y])))) +
      geom_point(mapping=aes(colour=as.numeric(unlist(df[colnames(df) == input$scat_var_group])))) + 
      labs(title=paste0("Variables ", colnames(df[colnames(df) == input$scat_var_x]) , " and ",
                        colnames(df[colnames(df) == input$scat_var_y]), " grouped by ",
                        colnames(df[colnames(df) == input$scat_var_group])),
                        x=colnames(df[colnames(df) == input$scat_var_x]), 
           y=colnames(df[colnames(df) == input$scat_var_y]), colour=colnames(df[colnames(df) == input$scat_var_group]))
    }
  )
  box <- reactive(
    if(input$box_var_group == 'None'){
      ggplot(data=df) + geom_boxplot(mapping = aes(x = as.numeric(unlist(df[colnames(df) == input$box_var_x])))) + 
        labs(title=paste0("Variables ", colnames(df[colnames(df) == input$box_var_x])),
             x=colnames(df[colnames(df) == input$box_var_x]))
    }else{
    ggplot(data=df) + geom_boxplot(mapping = aes(x = as.numeric(unlist(df[colnames(df) == input$box_var_x])), y=as.numeric(unlist(df[colnames(df) == input$box_var_group])),
                                                 group=as.numeric(unlist(df[colnames(df) == input$box_var_group])))) + 
      labs(title=paste0("Variables ", colnames(df[colnames(df) == input$box_var_x]) , " and ",
                        colnames(df[colnames(df) == input$box_var_group])),
           x=colnames(df[colnames(df) == input$box_var_x]), 
           y=colnames(df[colnames(df) == input$box_var_group]))
    } 
  )
  histo <- reactive(
    ggplot(data=df) + geom_histogram(mapping = aes(x=as.numeric(unlist(df[colnames(df) == input$histo_var_x])))) +
      labs(title=paste0("Histogram of variable ", colnames(df[colnames(df) == input$histo_var_x])),
           x=colnames(df[colnames(df) == input$histo_var_x]), 
           y="Frequency")
    
  )
  # sum <- reactive({
  #   summary_table <- t(as.numeric(summary(as.numeric(unlist(df[colnames(df) == input$tab_sum])))))
  #   knitr::kable(summary_table, col.names = c("Min", "Q1", "Median", "Mean", "Q3", "Max"))
  # })
  freq <- reactive(
    (table(as.numeric(unlist(df[colnames(df) == input$tab_freq_1])), as.numeric(unlist(df[colnames(df) == input$tab_freq_2]))))
   )
  # risk <- reactive({
  #   (table(as.numeric(unlist(df[colnames(df) == input$tab_risk_1])), as.numeric(unlist(df[colnames(df) == input$tab_risk_2]))))
  # })
  # ci <- reactive({
  #   error <- qt(p=1-(input$slider_ci/2), df=length(as.numeric(unlist(df[colnames(df) == input$tab_ci])))-1)*
  #     (sd(as.numeric(unlist(df[colnames(df) == input$tab_ci]))))/sqrt(length(as.numeric(unlist(df[colnames(df) == input$tab_ci]))))
  #   
  #   left <- mean(as.numeric(unlist(df[colnames(df) == input$tab_ci]))) - error
  #   right <- mean(as.numeric(unlist(df[colnames(df) == input$tab_ci]))) + error
  #   expr <- rbind(c("Alpha", "Lower limit", "Mean", "Upper limit", "Standard error"), 
  #                 round(c(input$slider_ci, left, mean(as.numeric(unlist(df[colnames(df) == input$tab_ci]))), right, error),
  #                       digits=2))
  #   colnames = FALSE
  # })
  output$scatter <- renderPlot({
    scatter()
  })
  output$box <- renderPlot({
    box()
  })
  output$histo <- renderPlot({
    histo()
  })
  output$sum <- renderTable(
    expr <- rbind(c("Min", "Q1", "Median", "Mean", "Q3", "Max"),
                  round(t(as.numeric(summary(as.numeric(unlist(df[colnames(df) == input$tab_sum]))))), digits=2)),
    colnames = FALSE
  )
  output$freq <- renderTable({
    freq()
  })
  output$risk <- renderTable(
    # expr <- rbind(c(input$tab_risk_1, input$tab_risk_2),
    #               ftable(as.character(unlist(df[colnames(df) == input$tab_risk_1])), as.character(unlist(df[colnames(df) == input$tab_risk_2])))),
    #               colnames = FALSE
    
    table(as.character(unlist(df[colnames(df) == input$tab_risk_1])),
          as.character(unlist(df[colnames(df) == input$tab_risk_2])))
  )
  # output$ci <- renderTable(
  #   ci()
  # )
}