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

  # Simple server stuff goes here ------------------------------------------------------------
  reactive_headlines <- reactive({
    df_py %>% filter(
      la_name == input$selectLA,
      education_type == input$selecteducation_type,
      time_period == input$select_year
    )
  })
  
  reactivePYtime_period <- reactive({
    df_py %>% filter(
      la_name == input$selectLA,
      education_phase == input$selecteducation_phase
    )
  })
  
  reactive_xaxis <- reactive({
    xaxis <- tolower(gsub(' ', '_',input$select_xaxis))
    if(input$select_xaxis=='School phase'){
      xaxis <- 'education_phase'
    } else if(input$select_xaxis=='Tenure'){
      xaxis <- 'tenure'
    }else if(input$select_xaxis=='Housing Type'){
      xaxis <- 'housing'
    }else if(input$select_xaxis=='School Type'){
      xaxis <- 'education_type'
    }
    return(xaxis)
  })

  reactive_breakdown <- reactive({
    breakdown <- tolower(gsub(' ', '_',input$select_breakdown))
    if(input$select_breakdown=='School phase'){
      breakdown <- 'education_phase'
    } else if(input$select_breakdown=='Tenure'){
      breakdown <- 'tenure'
    }
    return(breakdown)
  })
  
  reactive_filters <- reactive({
    filters <- filter_list[!(filter_list %in% c(input$select_breakdown,input$select_xaxis))]
    return(filters)
  })
  
  observeEvent(reactive_filters(),{
    for(i in 1:4){
      updateSelectizeInput(
        session,
        paste0('filter',i),
        label=reactive_filters()[i]
        )
    }
  })
  
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
    ggplotly(plotAvgRevBenchmark(reactiveBenchmark()) %>%
      config(displayModeBar = F),
    height = 420
    )
  })

  output$tabBenchmark <- renderDataTable({
    datatable(reactiveBenchmark() %>%
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
      paste0("£", format((reactive_headlines() %>% filter(
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


  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
