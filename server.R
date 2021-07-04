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
        theme_gray(base_size = 14)
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
        theme_gray(base_size = 14)
    }
    ggplotly(plot)
  })
  box <- reactive(
    if (input$box_var_group == "None") {
      ggplot(data = df) +
        geom_boxplot(mapping = aes(x = get(input$box_var_x, df)), fill = "steelblue") +
        labs(
          title = paste0("Boxplot for variable ", labels(df[colnames(df) == input$box_var_x])),
          x = labels(df[colnames(df) == input$box_var_x])
        ) +
        theme_minimal(base_size = 16)
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
            "Boxplot for variable ", labels(df[colnames(df) == input$box_var_x]), " grouped by ",
            labels(df[colnames(df) == input$box_var_group])
          ),
          x = labels(df[colnames(df) == input$box_var_x]),
          y = labels(df[colnames(df) == input$box_var_group])
        ) +
        theme_minimal(base_size = 16)
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
      theme_minimal(base_size = 16)
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
    browser()
  })

  sims_var <- reactive({
    time <- 1:12
    if (!is.na(input$sims_pat) & input$sims_pat >= 1 & input$sims_pat <= 300 & is.integer(input$sims_pat)) {
      if (input$sims == "bili") {
        ggplot(data = df_bili, mapping = aes(x = time, y = df_bili[, input$sims_pat])) +
          geom_line(colour = "steelblue") +
          geom_area(fill = "steelblue", alpha = 0.5) +
          geom_point() +
          scale_x_continuous(breaks = seq(0, 12, by = 1)) +
          labs(x = "Time in months", y = "Bilirubin", title = paste("Bilirubin over time for patient", input$sims_pat))
      } else if (input$sims == "bmi") {
        ggplot(data = df_bmi, mapping = aes(x = time, y = df_bmi[, input$sims_pat])) +
          geom_line(colour = "steelblue") +
          geom_area(fill = "steelblue", alpha = 0.5) +
          geom_point() +
          scale_x_continuous(breaks = seq(0, 12, by = 1)) +
          labs(x = "Time in months", y = "BMI", title = paste("BMI over time for patient", input$sims_pat))
      } else if (input$sims == "size") {
        ggplot(data = df_size, mapping = aes(x = time, y = df_size[, input$sims_pat])) +
          geom_line(colour = "steelblue") +
          geom_area(fill = "steelblue", alpha = 0.5) +
          geom_point() +
          scale_x_continuous(breaks = seq(0, 12, by = 1)) +
          labs(x = "Time in months", y = "Tumour size", title = paste("Tumour size over time for patient", input$sims_pat))
      } else if (input$sims == "weight") {
        ggplot(data = df_weight, mapping = aes(x = time, y = df_weight[, input$sims_pat])) +
          geom_line(colour = "steelblue") +
          geom_area(fill = "steelblue", alpha = 0.5) +
          geom_point() +
          scale_x_continuous(breaks = seq(0, 12, by = 1)) +
          labs(x = "Time in months", y = "Weight", title = paste("Weight over time for patient", input$sims_pat))
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

  output$freq <- render_gt({
    x <- as.data.frame(table(freq()))
    if (length(x) < 3) {
      names(x) <- c(labels(df[colnames(df) == input$tab_freq_1]), "Freq") # labels(df[colnames(df) == input$tab_risk_2])
    }
    else {
      names(x) <- c(
        labels(df[colnames(df) == input$tab_freq_1]),
        labels(df[colnames(df) == input$tab_freq_2]), "Freq"
      )
    }
    expr <- (x)
  })

  output$risk <- render_gt({
    if (input$tab_risk_1 != input$tab_risk_2) {
      risk_df <- risk() %>%
        count(.data[[input$tab_risk_1]], .data[[input$tab_risk_2]])

      risk_df <- as.data.frame(rbind(risk_df$n[1:2], risk_df$n[3:4]))
      risk_df <- as.data.frame(cbind(placeholder = c("0", "1"), risk_df)) %>%
        mutate(
          # Risk_d1 = risk_df[1, 2] / (risk_df[1, 1] + risk_df[1, 2]),
          # Risk_d0 = risk_df[2, 2] / (risk_df[2, 1] + risk_df[2, 2]),
          Risk = risk_df[, 2] / (risk_df[, 1] + risk_df[, 2]),
          Risk_diff = (risk_df[1, 2] / (risk_df[1, 1] + risk_df[1, 2])) - (risk_df[2, 2] / (risk_df[2, 1] + risk_df[2, 2])),
          Relative_risk = (risk_df[1, 2] / (risk_df[1, 1] + risk_df[1, 2])) / (risk_df[2, 2] / (risk_df[2, 1] + risk_df[2, 2])),
          Odds = risk_df[, 2] / risk_df[, 1],
          Odds_ratio = (risk_df[1, 1] * risk_df[2, 2]) / (risk_df[2, 1] * risk_df[1, 2])
        )
      risk_df[2, 5] <- risk_df[2, 5] * (-1)
      risk_df[2, 6] <- 1 / risk_df[2, 6]
      risk_df[2, 8] <- 1 / risk_df[2, 8]

      risk_df <- gt(risk_df) %>%
        fmt_number(
          columns = c(Risk, Risk_diff, Relative_risk, Odds, Odds_ratio),
          decimals = 2
        ) %>%
        tab_spanner(label = labels(df[colnames(df) == input$tab_risk_2]), columns = c(V1, V2)) %>%
        cols_label(
          placeholder = labels(df[colnames(df) == input$tab_risk_1]), V1 = "0", V2 = "1",
          Risk = "Risk in group", Risk_diff = "Risk difference", Relative_risk = "Relative risk",
          Odds = "Odds in group", Odds_ratio = "Odds ratio"
        )
      risk_df
    }
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

  output$plot_km <- renderPlot({
    fit <- survfit(Surv(df_surv$time, df_surv$event) ~ 1, data = df_surv)
    ggsurvplot(fit = fit, palette = "#2E9FDF", risk.table = TRUE)
  })

  output$plot_sims <- renderPlotly({
    sims_var()
  })
}