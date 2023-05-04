# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run th
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # ----- start of cookie code ----- #
  observeEvent(input$cookies, {
    if (!is.null(input$cookies)) {
      if (!("dfe_analytics" %in% names(input$cookies))) {
        shinyalert(
          inputId = "cookie_consent",
          title = "Cookie consent",
          text = "This site uses cookies to record traffic flow using Google Analytics",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Accept",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else {
        msg <- list(
          name = "dfe_analytics",
          value = input$cookies$dfe_analytics
        )
        session$sendCustomMessage("analytics-consent", msg)
        if ("cookies" %in% names(input)) {
          if ("dfe_analytics" %in% names(input$cookies)) {
            if (input$cookies$dfe_analytics == "denied") {
              ga_msg <- list(name = paste0("_ga_", google_analytics_key))
              session$sendCustomMessage("cookie-remove", ga_msg)
            }
          }
        }
      }
    }
  })

  output$headlines_data <- renderUI({
    if (input$bartab_toggle == "Chart") {
      plotlyOutput("bar_headlines")
    } else {
      tableOutput("table_headlines")
    }
  })

  output$table_headlines <- renderTable({
    df <- reactive_headlines() %>%
      select(time_period, la_name, reactive_xaxis()$colid, reactive_breakdown()$colid, "pupil_yield") %>%
      pivot_wider(
        names_from = reactive_xaxis()$colid,
        values_from = "pupil_yield"
      )
    colnames(df)[1:3] <- c("Financial Year", "Local authority", reactive_breakdown()$name)
    return(df)
  })

  output$timeseries_data <- renderUI({
    if (input$timetab_toggle == "Chart") {
      plotlyOutput("linePYtime_period")
    } else {
      tableOutput("table_timeseries")
    }
  })

  output$table_timeseries <- renderTable({
    df <- reactivePYtime_period() %>%
      select(time_period, la_name, education_phase, number_of_pupils, completed_properties_in_fy, pupil_yield)
    colnames(df) <- c("Financial year", "Local authority", "School phase", "# pupils", "Completed properties", "Pupil yield")
    return(df)
  })

  observeEvent(input$cookie_consent, {
    msg <- list(
      name = "dfe_analytics",
      value = ifelse(input$cookie_consent, "granted", "denied")
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "denied") {
          ga_msg <- list(name = paste0("_ga_", google_analytics_key))
          session$sendCustomMessage("cookie-remove", ga_msg)
        }
      }
    }
  })

  observeEvent(input$remove, {
    msg <- list(name = "dfe_analytics", value = "denied")
    session$sendCustomMessage("cookie-remove", msg)
    session$sendCustomMessage("analytics-consent", msg)
  })

  cookies_data <- reactive({
    input$cookies
  })

  output$cookie_status <- renderText({
    cookie_text_stem <- "To better understand the reach of our dashboard tools, this site uses cookies to identify numbers of unique users as part of Google Analytics. You have chosen to"
    cookie_text_tail <- "the use of cookies on this website."
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "granted") {
          paste(cookie_text_stem, "accept", cookie_text_tail)
        } else {
          paste(cookie_text_stem, "reject", cookie_text_tail)
        }
      }
    } else {
      "Cookies consent has not been confirmed."
    }
  })

  # ----- end of cookie code ----- #

  # Simple server stuff goes here ------------------------------------------------------------
  reactive_headlines <- reactive({
    print(reactive_filters()$colid)
    print(paste(input$filter1, input$filter2, input$filter3, input$filter4))
    df_py %>% filter(
      la_name == input$selectLA,
      time_period == input$select_year,
      get(reactive_filters()$colid[1]) == input$filter1,
      get(reactive_filters()$colid[2]) == input$filter2,
      get(reactive_filters()$colid[3]) == input$filter3,
      get(reactive_filters()$colid[4]) == input$filter4
    )
  })

  reactivePYtime_period <- reactive({
    df_py %>% filter(
      la_name == input$selectLA,
      tenure == "All", housing == "All", number_of_bedrooms == "All",
      education_phase == input$timeseries.phase
    )
  })

  reactive_xaxis <- reactive({
    filter_list %>% filter(name == input$select_xaxis)
  })

  reactive_breakdown <- reactive({
    filter_list %>% filter(name == input$select_breakdown)
  })

  reactive_filters <- reactive({
    filter_list %>%
      filter(!(name %in% c(input$select_breakdown, input$select_xaxis)))
  })

  observeEvent(
    input$select_xaxis,
    {
      updateSelectizeInput(
        session, "select_breakdown",
        choices = filter_list %>% filter(name != input$select_xaxis, !(name %in% c("Housing type", "Tenure", "Early years uplift"))) %>% pull(name)
      )
    }
  )

  observeEvent(reactive_filters(), {
    for (i in 1:4) {
      cat("=============================", fill = TRUE)
      updateSelectizeInput(
        session,
        paste0("filter", i),
        label = reactive_filters()$name[i],
        choices = choices[reactive_filters()$colid[i]][[1]],
        selected = reactive_filters()$default[i]
      )
    }
  })
  output$headline_title <- renderUI(
    h2(paste0("Pupil Yield is spilt by ",input$select_xaxis, " and ",input$select_breakdown, ""))
  )

  # Define server logic required to draw a histogram
  output$bar_headlines <- renderPlotly({
    print(reactive_headlines())
    ggplotly(create_bar_headline(reactive_headlines(), input$selectLA, reactive_xaxis(), reactive_breakdown())) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  # Render time_period line chart of pupil yield
  output$linePYtime_period <- renderPlotly({
    ggplotly(create_py_time_period(reactivePYtime_period())) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  reactiveBenchmark <- reactive({
    df_py %>%
      filter(
        local_authority %in% c(input$selectArea, input$selectBenchLAs),
        education_phase == input$selecteducation_phase,
        time_period == max(time_period, na.rm = TRUE)
      )
  })

  output$colBenchmark <- renderPlotly({
    ggplotly(
      plotAvgRevBenchmark(reactiveBenchmark()) %>%
        config(displayModeBar = F),
      height = 420
    )
  })

  output$tabBenchmark <- renderDataTable({
    datatable(
      reactiveBenchmark() %>%
        select(
          Area = local_authority,
          `Average Revenue Balance (£)` = average_revenue_balance,
          `Total Revenue Balance (£m)` = total_revenue_balance_million
        ),
      options = list(
        scrollX = TRUE,
        paging = FALSE
      )
    )
  })

  # Define server logic to create a box

  output$boxavgRevBal <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      paste0("£", format(
        (reactive_headlines() %>% filter(
          time_period == max(time_period, na.rm = TRUE),
          la_name == input$selectLA,
          education_phase == input$selectPhase
        ))$average_revenue_balance,
        big.mark = ","
      )),
      # add subtitle to explain what it's showing
      paste0("This is the latest value for the selected inputs"),
      color = "blue"
    )
  })
  output$boxpcRevBal <- renderValueBox({
    latest <- (reactive_headlines() %>% filter(
      time_period == max(time_period, na.rm = TRUE),
      la_name == input$selectLA,
      education_phase == input$selectPhase
    ))$average_revenue_balance
    penult <- (reactive_headlines() %>% filter(
      time_period == max(time_period, na.rm = TRUE) - 1,
      la_name == input$selectArea,
      education_phase == input$selectPhase
    ))$average_revenue_balance

    # Put value into box to plug into app
    valueBox(
      # take input number
      paste0("£", format(latest - penult,
        big.mark = ","
      )),
      # add subtitle to explain what it's hsowing
      paste0("Change on previous year"),
      color = "blue"
    )
  })

  observeEvent(input$link_to_app_content_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  })

  # Download the underlying data button
  output$download_data <- downloadHandler(
    filename = "shiny_template_underlying_data.csv",
    content = function(file) {
      write.csv(df_py, file)
    }
  )
  output$technicaltable <- renderTable(technical_table)

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
