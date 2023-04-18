homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("Estimating Pupil Yield from Housing Development in England"),
          br(),
          br()
        ),
        
        ## Left panel -------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "This section is useful if you want to understand how well different industries retain graduates.",
                  h3("Introduction"),
                  p("The dashboard provides data on......"),
                  p("You might want to add some brief introductory text here alongside some links to different tabs within your dashboard. Here's an example of a link working:"),
                  p(actionLink("link_to_app_content_tab", "Dashboard panel")),
                  p("You need to add an observeEvent() function to the server.R script for any link that navigates within your App.")
                ),
                br()
              )
            )
          ),
        ),
        
        ## Right panel ------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Background Info")
              ),
              div(
                class = "panel-body",
                h3("Context and purpose"),
                p("This app is the DfE Analytical Service's R-Shiny template demonstration app and is being developed to provide a coherent styling for DfE dashboards alongside some useful example componenets that teams can adapt for their own uses."),
                p("DfE teams using this template should avoid changing the styling and layout, keeping the header, footer and side navigation list formats."),
                p("You might want to add some relevant background information for your users here. For example some useful links to your EES publication, data sources and other relevant resources."),
                h3("Guidance sources"),
                p("For example, here we'll add some of the key resources we draw on to guide styling and vizualisation...")
              )
            )
          )
        )
      )
    )
  )
  
}


dashboard_panel <- function() {
  tabPanel(
    value = "dashboard",
    "Dashboard",
    
    # Define UI for application that draws a histogram
    
    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width=12,
          h1("Pupil Yield Data Dashboard"),
        ),
        column(
          width=12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              
              # selectizeInputs (up to 4 columns, if 4 columns set to width 3 each)
              # column 1   
              gov_row(         
                column(
                  width=4,
                  selectizeInput(
                    inputId = "selectArea",
                    label = "Choose a Geography:",
                    choices = choicesgeographic_level
                  )),
                column(
                  width=4,
                  selectizeInput(
                    inputId = "selectLA",
                    label = "Choose a LA:",
                    choices = choicesLAs,
                    selected="LA1"
                  )),
                column(
                  width=4,
                  selectizeInput(
                    inputId = "select_year",
                    label = "Choose a Year:",
                    choices = choicesYears,
                    selected=max(choicesYears)
                  ))
              ),
              p("Choose the chart variables here:"),
              gov_row(
                column(
                  width=4,
                  selectizeInput(
                    inputId='select_xaxis',
                    label='Choose x-axis variable',
                    choices = c('School phase', 'School type', 'Housing type', 'Tenure', 'Number of bedrooms')
                  )),
                column(
                  width=4,
                  selectizeInput(
                    inputId='select_breakdown',
                    label='Choose breakdown variable',
                    choices = c('Tenure', 'Housing type', 'Number of bedrooms', 'School phase', 'School type')
                  ))
                
              ),
              gov_row(
                
                # selectizeInput(
                #   inputId = "selectphase_type",
                #   label = "Choose a Pupil Yield Type:",
                #   choices = choicesPhase
                # )), 
                
                # column 2  
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "selecteducation_type",
                    label = "Choose an Education Type:",
                    choices = choiceseducation_type,
                    selected="Mainstream"
                  ),
                  selectizeInput(
                    inputId = "selecteducation_phase",
                    label = "Choose a Phase:",
                    choices = choicesPhase,
                    selected="Primary"
                  )),
                
                # column 3
                column(
                  width = 4,
                  selectizeInput("selecthousing_type",
                                 "Choose a Housing Type:",
                                 choices = choiceshousing
                  ),
                  selectizeInput(
                    inputId = "selecttenure",
                    label = "Choose a Tenure:",
                    choices = choicestenure
                  ),
                  selectizeInput(
                    inputId = "selectnumner_beds",
                    label = "Choose the number of bedrooms:",
                    choices = choicesnumber_beds
                  )),
                
                
                # column 4
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "selectearly_years_uplift",
                    label = "Early Years Uplift:",
                    choices = choicesearly_years_uplift
                  ))
              ),
              
              column(
                width = 12,
                paste("Download the underlying data for this dashboard:"), br(),
                downloadButton(
                  outputId = "download_data",
                  label= "Download data",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                )
              ))
          )
        ),
        
        column(
          width=12,
          tabsetPanel(id = "tabsetpanels",
                      tabPanel(
                        "Headlines",
                        fluidRow(
                          column(
                            width=12,
                            h2("Insert auto title here"),
                            valueBoxOutput("boxavgRevBal", width = 6),
                            valueBoxOutput("boxpcRevBal", width = 6),
                            box(
                              width=12,
                              plotlyOutput("bar_headlines")))
                        )
                      ),
                      tabPanel(
                        "Peaks & Averages",
                        fluidRow(
                          column(
                            width=12,
                            h2("Pupil Yield showing peak and average"),
                            p("This is the standard paragraph style for adding guiding info around data content."),
                            column(
                              width=6,
                              box(
                                width=12,
                                plotlyOutput("linePYtime_period")
                              )
                            ),
                            column(
                              width=6,
                              div(
                                class = "well",
                                style = "min-height: 100%; height: 100%; overflow-y: visible",
                                fluidRow(
                                  column(
                                    width=12,
                                    selectizeInput("selectBenchLAs",
                                                   "Select benchamrk LAs",
                                                   choices = choicesLAs,
                                                   multiple=TRUE,
                                                   options = list(maxItems = 3)
                                    )
                                  )
                                )
                              ),
                              dataTableOutput("tabBenchmark")
                            ))
                        ))
          )
        )
        # add box to show user input
      )
    )
  )
}

technical_panel <- function() {
  tabPanel(
    "Technical",
    gov_main_layout(
      gov_row(
        column(width=12,
               h1("Technical Details"),
               br("This Document **application name**.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
               h2("SUb Heading 1"),
               br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
               tags$div(tags$ul(
                 tags$li("add text"),
                 tags$li("add text"),
                 tags$li("add text")
               )),
               h2("add tecxt"),
               br("add text"),
               tags$div(tags$ul(
                 tags$li("List"),
                 tags$li("known"),
                 tags$li("limitations, e.g."),
                 tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
               )),
               h2("SUb Heading"),
               br(
                 "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
                 a(href = "mailto:email@education.gov.uk", "email@education.gov.uk")
               )
        )
      )
    )
  )
}

