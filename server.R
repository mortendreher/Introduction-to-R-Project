source("global.R")

server <- function(input, output, session) {
  dataset <- reactive({
    select(df, c(ID, input$c_vars))
  })
  myChoices <- c_vars_choices

  observeEvent(input$c_all, {
    updateCheckboxGroupInput(session, inputId = "c_vars", selected = c_vars_choices)
  })

  observeEvent(input$c_vars, {
    if (length(input$c_vars) != length(c_vars_choices)) {
      updateCheckboxInput(session, "c_all", value = FALSE)
    }
  })
  output$out_dataset <- renderDataTable(
    dataset()
  )
  scatter <- reactive({
    plot <- if (input$scat_var_group == "None") {
      ggplot(data = df, mapping = aes(x = get(input$scat_var_x), y = get(input$scat_var_y))) +
        geom_point(colour = "steelblue", size = 2) +
        labs(
          title = paste0(
            "Variables ", labels(df[colnames(df) == input$scat_var_x]), " and ",
            labels(df[colnames(df) == input$scat_var_y])
          ),
          x = labels(df[colnames(df) == input$scat_var_x]),
          y = labels(df[colnames(df) == input$scat_var_y])
        ) +
        theme_gray()
    } else {
      ggplot(data = df, mapping = aes(x = get(input$scat_var_x), y = get(input$scat_var_y))) +
        geom_point(mapping = aes(colour = unlist_as_char(input$scat_var_group, df)), size = 2) +
        labs(
          title = paste0(
            "Variables ", labels(df[colnames(df) == input$scat_var_x]), " and ",
            labels(df[colnames(df) == input$scat_var_y]), " grouped by ",
            labels(df[colnames(df) == input$scat_var_group])
          ),
          x = labels((df[colnames(df) == input$scat_var_x])),
          y = labels((df[colnames(df) == input$scat_var_y])),
          colour = labels(df[colnames(df) == input$scat_var_group])
        ) +
        theme_gray()
    }
    ggplotly(plot)
  })
  box <- reactive(
    if (input$box_var_group == "None") {
      ggplot(data = df) +
        geom_boxplot(mapping = aes(x = get(input$box_var_x, df)), fill = "steelblue") +
        labs(
          title = paste0("Variable ", labels(df[colnames(df) == input$box_var_x])),
          x = labels(df[colnames(df) == input$box_var_x])
        ) +
        theme_minimal()
    } else {
      ggplot(data = df) +
        geom_boxplot(mapping = aes(
          x = get(input$box_var_x),
          y = unlist_as_char(input$box_var_group, df),
          fill = unlist_as_char(input$box_var_group, df)
        )) +
        scale_fill_discrete(name = input$box_var_group, guide = guide_legend(reverse = TRUE)) +
        labs(
          title = paste0(
            "Variables ", labels(df[colnames(df) == input$box_var_x]), " and ",
            labels(df[colnames(df) == input$box_var_group])
          ),
          x = labels(df[colnames(df) == input$box_var_x]),
          y = labels(df[colnames(df) == input$box_var_group])
        ) +
        theme_minimal()
    }
  )
  histo <- reactive(
    ggplot(data = df) +
      geom_histogram(mapping = aes(x = get(input$histo_var_x)), colour = "black", fill = "steelblue") +
      labs(
        title = paste0("Histogram of variable ", labels(df[colnames(df) == input$histo_var_x])),
        x = labels(df[colnames(df) == input$histo_var_x]),
        y = "Frequency"
      ) +
      theme_minimal()
  )

  freq <- reactive(
    if (input$tab_freq_2 == "None") {
      select(df, input$tab_freq_1)
    }
    else {
      select(df, input$tab_freq_1, input$tab_freq_2)
    }
  )

  risk <- reactive(
    if (input$tab_risk_1 != input$tab_risk_2) {
      select(df, input$tab_risk_1, input$tab_risk_2)
    }
    else {
      select(df, input$tab_risk_1)
    }
  )
  
  ci_out <- reactive({
    select(df, input$tab_ci)
  })
  
  kap_mei <- reactive({
    fit <- survfit(data=df_alive[13:14,], Surv(time=as.numeric(df_surv[1,]), event=as.numeric(df_surv[2,]))~1)
      # survfit(Surv(time, status) ~ 1, data = lung)
    ggsurvplot(fit=df_alive, risk.table = TRUE)
    # browser()
  })
  
  sims_var <- reactive({
    if(!is.na(input$sims_pat) & input$sims_pat >= 1 & input$sims_pat <=300 & is.integer(input$sims_pat) ) { 
    if(input$sims=='bili') {
      ggplot(data=df_bili, mapping=aes(x=1:12, y=df_bili[,input$sims_pat])) + geom_line(colour="steelblue") + 
        geom_area(fill="steelblue", alpha=0.5) +
        geom_point() + scale_x_continuous(breaks=seq(0,12,by=1)) +
        labs(x="Time in months", y="Bilirubin", title=paste("Bilirubin over time for patient", input$sims_pat))
    } else if(input$sims=='bmi') {
      ggplot(data=df_bmi, mapping=aes(x=1:12, y=df_bmi[,input$sims_pat])) + geom_line(colour="steelblue") +
        geom_area(fill="steelblue", alpha=0.5) +
        geom_point() + scale_x_continuous(breaks=seq(0,12,by=1)) +
        labs(x="Time in months", y="BMI", title=paste("BMI over time for patient", input$sims_pat))
    } else if(input$sims=='size') {
      ggplot(data=df_size, mapping=aes(x=1:12, y=df_size[,input$sims_pat])) + geom_line(colour="steelblue") +
        geom_area(fill="steelblue", alpha=0.5) +
        geom_point() + scale_x_continuous(breaks=seq(0,12,by=1)) +
        labs(x="Time in months", y="Tumour size", title=paste("Tumour size over time for patient", input$sims_pat))
    } else if(input$sims=='weight') {
      ggplot(data=df_weight, mapping=aes(x=1:12, y=df_weight[,input$sims_pat])) + geom_line(colour="steelblue") +
        geom_area(fill="steelblue", alpha=0.5) +
        geom_point() + scale_x_continuous(breaks=seq(0,12,by=1)) +
        labs(x="Time in months", y="Weight", title=paste("Weight over time for patient", input$sims_pat))
    }
    }
  })

  output$scatter <- renderPlotly({
    scatter()
  })

  output$box <- renderPlot({
    box()
  })

  output$histo <- renderPlot({
    histo()
  })

  output$sum <- renderTable({
    expr <- rbind(round(t(as.numeric(summary(as.numeric(unlist(df[colnames(df) == input$tab_sum]))))), digits = 2))
    colnames(expr) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")
    expr
  })

  output$freq <- renderTable({
    x <- as.data.frame(table(freq()))
    if (length(x) < 3) {
      names(x) <- c(input$tab_freq_1, "Freq")
    }
    else {
      names(x) <- c(input$tab_freq_1, input$tab_freq_2, "Freq")
    }
    expr <- (x)
  })

  output$risk <- renderTable({
    risk_df <- risk() %>%
      count(.data[[input$tab_risk_1]], .data[[input$tab_risk_2]])

    risk_df <- as.data.frame(rbind(risk_df$n[1:2], risk_df$n[3:4]))
    risk_df <- as.data.frame(cbind(c(0, 1), risk_df)) %>%
      mutate(
        Risk_d1 = risk_df[1, 1] / (risk_df[1, 1] + risk_df[1, 2]),
        Risk_d0 = risk_df[2, 1] / (risk_df[2, 1] + risk_df[2, 2]),
        Risk_diff = Risk_d1 - Risk_d0,
        Odds_ratio = (risk_df[1, 1] * risk_df[2, 2]) / (risk_df[2, 1] * risk_df[1, 2])
      )
    colnames(risk_df) <- c(" ", 0, 1, "Risk_d1", "Risk_d0", "Risk_diff", "Odds_ratio")
    risk_df %>%
      gt() %>%
      fmt_number(
        columns = vars(Risk_d1, Risk_d0, Risk_diff, Odds_ratio),
        decimals = 2
      )
  })

  output$ci <- renderTable({
    ci_var <- as.numeric(unlist(ci_out()))
    alpha <- 1 - input$slider_ci
    error <- qt(p = 1 - (alpha / 2), df = length(ci_var) - 1) * (sd(ci_var)) / sqrt(length(ci_var))
    left <- mean(ci_var) - error
    right <- mean(ci_var) + error
    tab <- rbind(round(c(alpha, left, mean(ci_var), right, error),
      digits = 4
    ))
    colnames(tab) <- c("Alpha", "Lower limit", "Mean", "Upper limit", "Standard error")
    tab
  })
  
  output$plot_km <- renderPlotly({
     kap_mei()
    })
  
  output$plot_sims <- renderPlotly({
    sims_var()
  })
}