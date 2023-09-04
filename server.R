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
      mutate(pupil_yield = str_trim(format(pupil_yield, digits = 2, nsmall = 2))) %>%
      pivot_wider(
        names_from = reactive_xaxis()$colid,
        values_from = "pupil_yield",
        values_fill = "z"
      )
    colnames(df)[1:3] <- c("Academic Year", "Local authority", reactive_breakdown()$name)
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
    colnames(df) <- c("Academic year", "Local authority", "School phase", "# pupils", "Completed properties", "Pupil yield")
    return(df)
  })

  output$timeseries_caption <- renderUI({
    tags$p("This chart shows the cumulative Pupil Yield over time for school phase set to ", tolower(input$timeseries.phase), " and housing type set to ", tolower(input$timeseries.housing), ". ")
  })
  
  #post completion tab
  
  output$pc_data <- renderUI({
    if (input$postcomtab_toggle == "Chart") {
      plotlyOutput("linepctime_period")
    } else {
      tableOutput("table_timeseriespc")
    }
  })
  
  output$table_timeseriespc <- renderTable({
    df <- reactivepctime_period() %>%
      select(time_period, la_name, education_phase, number_of_pupils, completed_properties_in_ay, pupil_yield, years_after_completion
)
    colnames(df) <- c("Academic year", "Local authority", "School phase", "# pupils", "Completed properties", "Pupil yield", "Years after completion")
    return(df)
  })
  
  output$timeseriespc_caption <- renderUI({
    tags$p("This chart shows the yearly pupil yeild and average pupil yield by school phase as ", tolower(input$timeseries.phase), " and housing type as ", tolower(input$timeseries.housing), ". ")
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
  reactive_area <- reactive({
    if (input$geographic_level_choice == "England") {
      "England"
    } else if (input$geographic_level_choice == "County/Unitary") {
      input$selectLA
    } else {
      input$selectLAD
    }
  })

  reactive_headlines <- reactive({
    headlines <- df_py %>% filter(
      la_name == reactive_area(),
      time_period == input$select_year,
      get(reactive_filters()$colid[1]) == input$filter1,
      get(reactive_filters()$colid[2]) == input$filter2,
      get(reactive_filters()$colid[3]) == input$filter3
    )
    if (input$agg_beds) {
      headlines %>% filter(!number_of_bedrooms %in% c("2", "3"))
    } else {
      headlines %>% filter(!number_of_bedrooms %in% c("2+", "3+"))
    }
  })

  reactivePYtime_period <- reactive({
    df_py %>% filter(
      la_name == reactive_area(),
      tenure == "All", housing == input$timeseries.housing, number_of_bedrooms == "All",
      education_phase == input$timeseries.phase
    )
  })

  reactivepctime_period <- reactive({
    df_pc %>% filter(
      la_name == reactive_area(),
      time_period == input$time.period,
      education_phase == input$education.phase
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
    input$selectLA,
    {
      updateSelectizeInput(
        session, "selectLAD",
        choices = la_lad_lookup %>% filter(la_name == input$selectLA) %>% pull(lad_name) %>% sort()
      )
    }
  )

  # Headlines server scripts ------------------------------------------------

  observeEvent(
    input$select_xaxis,
    {
      choices_update <- filter_list %>%
        filter(name != input$select_xaxis) %>%
        pull(name)
      updateSelectizeInput(
        session, "select_breakdown",
        choices = choices_update,
        selected = ifelse(input$select_xaxis == input$select_breakdown, choices_update[1], input$select_breakdown)
      )
    }
  )

  # remove post-16 from drop down if 2021/22 selected
  choices <- reactive({
    if (input$agg_beds) {
      choicesnumber_beds <- c("All", "1", "2+", "3+", "4+")
    } else {
      choicesnumber_beds <- c("All", "1", "2", "3", "4+")
    } 
    if (input$select_year == '2021/22') {
      choicesPhase <- c("Early Years", "Primary", "Secondary","Special Schools/AP")
    } else {
      choicesPhase <- c("Early Years", "Primary", "Secondary","Post-16","Special Schools/AP")
    }
    list(
      education_type = choiceseducation_type,
      education_phase = choicesPhase,
      housing = choiceshousing,
      tenure = choicestenure,
      number_of_bedrooms = choicesnumber_beds
    )
  })

  observeEvent(c(reactive_filters(), choices()), {
    for (i in 1:3) {
      cat("=============================", fill = TRUE)
      message(paste0("Updating filter ", i, " to ", reactive_filters()$name[i]))
      updateSelectizeInput(
        session,
        paste0("filter", i),
        label = reactive_filters()$name[i],
        choices = choices()[reactive_filters()$colid[i][[1]]],
        selected = reactive_filters()$default[i]
      )
      message(paste0("Updated filter ", i, " to ", reactive_filters()$name[i]))
    }
  })

  observeEvent(input$reset_headline_input, {
    updateSelectizeInput(
      session,
      "select_xaxis",
      selected = "School phase"
    )
    updateSelectizeInput(
      session,
      "select_breakdown",
      selected = "School type"
    )
    updateSelectizeInput(
      session,
      "select_year",
      selected = "2021/22"
    )
    for (i in 1:4) {
      updateSelectizeInput(
        session,
        paste0("filter", i),
        label = reactive_filters()$name[i],
        choices = choices()[reactive_filters()$colid[i]][[1]],
        selected = reactive_filters()$default[i]
      )
    }
  })

  output$headlines_title <- renderUI(
    h2(paste0("Pupil Yield is split by ", input$select_xaxis, " and ", input$select_breakdown, " for ", reactive_area()))
  )

  output$headlines_caption <- renderUI(
    p(paste0(
      "This chart shows Pupil Yields for ",
      tolower(paste0(
        reactive_filters()$name[1], " set to ", input$filter1, ", ",
        reactive_filters()$name[2], " set to ", input$filter2, " and ",
        reactive_filters()$name[3], " set to ", input$filter3, " for properties completed up to ",
        ifelse(input$select_year == "All", "all years", input$select_year), ". "
      ))
    ))
  )
  
  # Add post16 caption if 2021/22 selected
  output$post16_2122_caption <- renderUI(
    p(strong(paste0(
      ifelse(input$select_year == "2021/22", "Select 2020/21 or earlier for Post-16 Yields.","") 
    )))
  )


  # Define server logic required to draw a histogram
  output$bar_headlines <- renderPlotly({
    ggplotly(
      create_bar_headline(
        reactive_headlines(), input$selectLA,
        reactive_xaxis(), reactive_breakdown()
      ),
      tooltip = c("text")
    ) %>%
      config(displayModeBar = F) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2) # ,
        # hovermode = "x unified"
      )
  })


  # Timeseries server scripts -----------------------------------------------
 
   # Timeseries chart time
  output$timeseries_title <- renderUI(
    h2(paste0("Pupil Yield over time as the number of completed properties increases for ", reactive_area()))
  )
  
  # Add caption if Post-16 selected
  output$post16_caption <- renderUI(
    p(strong(paste0(
       ifelse(input$timeseries.phase == "Post-16", "Post-16 Pupil Yield data up to 2020/21 only.","") 
         )))
  )

  # Render time_period line chart of pupil yield
  output$linePYtime_period <- renderPlotly({
    ggplotly(create_py_time_period(reactivePYtime_period()),
      tooltip = c("text")
    ) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  # Render time_period line chart of post completion
  output$linepctime_period <- renderPlotly({
    validate(
      need(
        nrow(reactivepctime_period())>0,
        'Sorry, no data found for selected combination.')
    )
    ggplotly(create_pc_time_period(reactivepctime_period()),
             tooltip = c("text")
    ) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })



  
  reactiveBenchmark <- reactive({
    df_py %>%
      filter(
        local_authority %in% c(input$selectLA, input$selectBenchLAs),
        education_phase == input$selecteducation_phase,
        time_period == max(time_period, na.rm = TRUE)
      )
  })

  output$colBenchmark <- renderPlotly({
    ggplotly(
      plotAvgRevBenchmark(reactiveBenchmark()) %>%
        config(displayModeBar = F),
      height = 560
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

  observeEvent(input$link_to_app_content_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  })

  # Download the underlying data button
  output$download_headlines_data <- downloadHandler(
    filename = "pupil_yield_underlying_data.csv",
    content = function(file) {

      write.csv(df_py_download, file, row.names = FALSE)

    }
  )

  output$download_averages_data <- downloadHandler(
    filename = "pupil_yield_underlying_data.csv",
    content = function(file) {

      write.csv(df_py_download, file, row.names = FALSE)

    }
  )
  
  output$download_pc_data <- downloadHandler(
    filename = "post_completion_underlying_data.csv",
    content = function(file) {
      write.csv(df_pc, file, row.names = FALSE)

    }
  )

  output$download_send_data <- downloadHandler(
    filename = "ehcp_underlying_data.csv",
    content = function(file) {

      write.csv(df_ehcp_download, file, row.names = FALSE)

    }
  )

  output$technicaltable <- renderTable(technical_table)



  # Post completion tab scripts ---------------------------------------------

  output$postcompletion_title <- renderUI(
    h2(paste0("Pupil Yield post completion for ", reactive_area()))
  )


  # SEND server scripts -----------------------------------------------------

  output$send_title <- renderUI(
    h2(paste0("Pupils with Special Educational Needs and Disabilities for ", reactive_area()))
  )

  ehcp_lines <- reactive({
    df_ehcp %>%
      filter(
        AcademicYear == input$send_year,
        LTLA22NM == reactive_area()
      )
  })

  output$send_box_1 <- renderValueBox(
    valueBox(
      ehcp_lines() %>% filter(SENprovision_Name == "EHCP") %>% pull(Percentage) %>% paste("%"),
      "EHCP"
    )
  )

  output$send_box_2 <- renderValueBox(
    valueBox(
      ehcp_lines() %>% filter(SENprovision_Name == "No SEN support") %>% pull(Percentage) %>% paste("%"),
      "No SEN support"
    )
  )

  output$send_box_3 <- renderValueBox(
    valueBox(
      ehcp_lines() %>% filter(SENprovision_Name == "SEN support") %>% pull(Percentage) %>% paste("%"),
      "SEN support"
    )
  )

  # actionLinks
  observeEvent(input$linkHeadlinesTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "tabsetpanels", selected = "Headlines")
  })

  observeEvent(input$linkAveragesTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "tabsetpanels", selected = "Averages")
  })

  observeEvent(input$linkPCTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "tabsetpanels", selected = "post-completion time series")
  })

  observeEvent(input$linkSENDTab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "tabsetpanels", selected = "SEND")
  })


  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
