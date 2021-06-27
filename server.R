library(shiny)
library(tidyverse)
library(ggplot2)
library(survminer)
library(gmodels)

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
      geom_point(mapping=aes(colour=as.character(unlist(df[colnames(df) == input$scat_var_group])))) + 
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
    ggplot(data=df) + geom_boxplot(mapping = aes(x = as.numeric(unlist(df[colnames(df) == input$box_var_x])), y=as.character(unlist(df[colnames(df) == input$box_var_group])),
                                                 group=as.character(unlist(df[colnames(df) == input$box_var_group])))) + 
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
    if(input$tab_freq_2=='None') {
      select(df, input$tab_freq_1)
    }
    else {
      select(df, input$tab_freq_1, input$tab_freq_2)
    }
    )
  risk <- reactive({
     select(df, input$tab_risk_1, input$tab_risk_2)

     
     
     })
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
     x <- as.data.frame(table(freq()))
     if(length(x) < 3) {
       names(x) = c(input$tab_freq_1, "Freq")
     }
     else {
       names(x) = c(input$tab_freq_1, input$tab_freq_2, "Freq")
     }
     expr <- (x)
  })
  output$risk <- renderTable({
    risk_df <- risk()
    
    if(length(risk_df) > 1) { 
      unique1 <- sort((unique(risk_df[,1])))
      unique2 <- sort((unique(risk_df[,2])))
      m <- matrix(0,nrow=length(unique1), ncol=length(unique2)+1, dimnames=list(a=unique1, b=c("",unique2)))
      m <- data.frame(m,row.names = unique1)
      m[,1] <- (unique1)
      for(i in (1:length(unique1))) {
        
        for(j in (2:(length(unique2)+1))) { 
          
          m[i,j] <- (sum(risk_df[,1]==unique1[i] & risk_df[,2]==unique2[j-1]))
          
          }
        
      }
    }
    colnames(m) <- c(" ", unique2)
    format.data.frame(m, digits=0)
  })
  # output$ci <- renderTable(
  #   ci()
  # )
}