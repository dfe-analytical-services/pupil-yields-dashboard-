homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("Estimating Pupil Yield from Housing Development in England"),
          p("DfE has worked with the Office for National Statistics (ONS) to develop a recommended methodology for estimating pupil yield from housing development, to assist local authorities demonstrating the need for education facilities during local plan preparation and the consideration of planning applications. There are technical notes explaining the processes employed in preparing pupil yield data, sitting alongside new guidance, so that local authorities will be able to replicate our methodology when producing similar local pupil yield data in the future."),
          p("The Pupil Yield Dashboard provides pupil yield factors for each local authority in England, at county, unitary, and district level (where applicable). They can be displayed by education type (mainstream or special), phase (early years, primary, secondary and post-16) and filtered according to housing type (flats/houses), tenure (market/affordable) and size (bedroom numbers). Alongside headline figures for each local authority, there is a graph showing annual pupil yield factors since 2008, and how pupil yield develops over time."),
          p("Pupil yield factors are the number of pupils living in the properties divided by the number of completed properties (see technical notes for more details). The technical notes with this dashboard are provided for local authorities with a responsibility for providing sufficient school places under the Education Act 1996 – principally analysts/data scientists involved in pupil forecasting and other population modelling."),
          br(),
          br()
        ),

        ## Contents panel -------------------------------------------------------

        column(
          12,
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
                  h3("Tabs in the dashboard"),
                  h4(actionLink("linkHeadlinesTab", "Headlines")),
                  p("This tab displays the pupil yield factor for each combination of breakdowns (school type, housing type, bedrooms, and tenure)."),
                  h4(actionLink("linkAveragesTab", "Cumulative time series")),
                  p("This tab displays the cumulative pupil yield factor over time by school phase and housing type. This will show where additional yield has sped up or slowed down by looking at the slope of the curve. Data can be interrogated further on the headlines tab if required."),
                  h4(actionLink("linkPCTab", "Post completion time series")),
                  p("This tab displays the pupil yield factor each year after developments have completed, for all developments or only developments completed in certain academic years.  This will show where pupil yield peaks post development."),
                  strong("Note:"), ("the sample size reduces each year post completion and so care should be taken when using yields furthest from completion."),
                  h4(actionLink("linkSENDTab", "Special Educational Needs and Disabilities")),
                  p("This tab shows the proportion of pupils living in the properties completed up to the selected academic year that required Special Education Needs (SEN) support or had Education, Health and Care Plans (EHCPs)."),
                ),
                br()
              )
            )
          ),
        ),
      )
    )
  )
}


dashboard_panel <- function() {
  tabPanel(
    value = "dashboard",
    "Pupil Yield Dashboard",

    # Define UI for application that draws a histogram

    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Pupil Yield Data Dashboard"),
          # p("This draft Pupil Yield Data Dashboard (which is still under development ) includes anonymous/dummy data from four local authorities at present."),
        ),
        column(
          width = 12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(

              # selectizeInputs (up to 4 columns, if 4 columns set to width 3 each)
              # column 1
              gov_row(
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "geographic_level_choice",
                    label = "Choose a geographic level:",
                    choices = choicesgeographic_level
                  )
                ),
                column(
                  width = 4,
                  conditionalPanel(
                    condition = "input.geographic_level_choice == 'County/Unitary' || input.geographic_level_choice == 'District'",
                    selectizeInput(
                      inputId = "selectLA",
                      label = "Choose a County/Unitary Local Authority:",
                      choices = choicesLAs
                    )
                  )
                ),
                column(
                  width = 4,
                  conditionalPanel(
                    condition = "input.geographic_level_choice == 'District'",
                    selectizeInput(
                      inputId = "selectLAD",
                      label = "Choose a Local Authority District:",
                      choices = choicesLADs
                    )
                  )
                )
              )
            )
          )
        ),
        column(
          width = 12,
          tabsetPanel(
            id = "tabsetpanels",

            # Headlines panel ---------------------------------------------------------
            tabPanel(
              value = "Headlines",
              title = "Headlines",
              gov_row(
                column(
                  width = 12,
                  uiOutput("headlines_title"),
                  radioGroupButtons(
                    "bartab_toggle",
                    label = NULL,
                    choices = c("Chart", "Table"),
                    selected = "Chart"
                  ),
                  uiOutput("headlines_data"),
                  uiOutput("headlines_caption")
                )
              ),
              gov_row(
                column(
                  12,
                  tags$h3("Choose the chart breakdowns here:")
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "select_xaxis",
                    label = "Choose first breakdown variable",
                    choices = filter_list %>% filter(!(name %in% c("Early years uplift"))) %>% pull(name),
                    selected = "School phase"
                  )
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "select_breakdown",
                    label = "Choose next breakdown variable",
                    choices = filter_list %>% pull(name),
                    selected = "School type"
                  )
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "select_year",
                    label = "Choose a Year:",
                    choices = choicesYears,
                    selected = "2021/22"
                  )
                )
              ),
              uiOutput("post16_2122_caption", style="color:red"),
              gov_row(
                column(
                  12,
                  tags$h3("Choose the chart filters here:"),
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "filter1",
                    label = "Choose number of bedrooms:",
                    choices = choices_default$number_of_bedrooms,
                    selected = "All"
                  )
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "filter2",
                    choices = choices_default$housing,
                    label = "Choose a housing type:",
                    selected = "All"
                  )
                ),
                column(
                  width = 4,
                  selectizeInput("filter3",
                    "Choose a tenure:",
                    choices = choices_default$tenure,
                    selected = "All"
                  )
                )
              ),
                        gov_row(
                column(
                  12,
                  checkboxInput("agg_beds", "Aggregate bedroom numbers")
                )
              ),
              gov_row(
                column(
                  12,
                  actionButton("reset_headline_input", "Reset filters")
                )
              ),
              gov_row(
                tags$hr(),
                column(
                  width = 12,
                  paste("Download the underlying data for this dashboard:"), br(),
                  downloadButton(
                    outputId = "download_headlines_data",
                    label = "Download data",
                    icon = shiny::icon("download"),
                    class = "downloadButton"
                  )
                )
              )
            ),

            # Cumulative time-series --------------------------------------------------
            tabPanel(
              value = "Averages",
              title = "Cumulative time series",
              fluidRow(
                column(
                  width = 12,
                  uiOutput("timeseries_title"),
                  column(
                    width = 12,
                    radioGroupButtons(
                      "timetab_toggle",
                      label = NULL,
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    uiOutput("timeseries_data"),
                    uiOutput("timeseries_caption"),
                     column(
                      width = 4,
                      selectizeInput(
                        "timeseries.phase",
                        label = "Choose a school phase",
                        choices = choices_default$education_phase,
                        selected = "Primary"
                      )
                    ),
                    column(
                      width = 4,
                      selectizeInput(
                        "timeseries.housing",
                        label = "Choose a housing type",
                        choices = choices_default$housing
                      )
                    )
                  ),
                  uiOutput("post16_caption", style="color:red"),
                  gov_row(
                    column(
                      width = 12,
                      tags$hr(),
                      paste("Download the underlying data for this dashboard:"), br(),
                      downloadButton(
                        outputId = "download_averages_data",
                        label = "Download data",
                        icon = shiny::icon("download"),
                        class = "downloadButton"
                      )
                    )
                  )
                )
              )
            ),
            tabPanel(
              value = "post-completion time series",
              title = "Post completion time series",
              fluidRow(
                column(
                  width = 12,

                  h2("Pupil Yield post completion"),
                  #p("This is the standard paragraph style for adding guiding info around data content."),
                  column(
                    width = 12,
                    radioGroupButtons(
                      "postcomtab_toggle",
                      label = NULL,
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    uiOutput("pc_data"),
                    uiOutput("pc_caption"),
                    p("This data does not account for developments that are still under construction and only partially occupied, when pupil yield is lower. This tab may show higher pupil yield factors than in the “headlines” tab. "),
                    column(
                      width = 4,
                      selectizeInput(
                        "education.phase",
                        label = "Choose an school phase",
                        choices = choicespc$education_phase,
                        selected = 'Primary'

                      )
                    ),
                    column(
                      width = 4,
                      selectizeInput(
                        "time.period",
                        label = "Choose a Year:",
                        choices = choicespc$time_period,
                        selected = 'All' 
                      )
                    ),


            # SEND panel --------------------------------------------------------------

                  gov_row(
                    column(
                      width = 12,
                      tags$hr(),
                      paste("Download the underlying data for this dashboard:"), br(),
                      downloadButton(
                        outputId = "download_pc_data",
                        label = "Download data",
                        icon = shiny::icon("download"),
                        class = "downloadButton"
                      )
                    )
                  )
                )
              )
            )
            
            )
            ,

            tabPanel(
              value = "SEND",
              title = "Special Educational Needs and Disabilities",
              gov_row(
                column(
                  width = 12,
                  uiOutput("send_title"),
                  selectizeInput(
                    "send_year",
                    label = "Choose a Year",
                    choices = df_ehcp %>% arrange(AcademicYear) %>% pull(AcademicYear) %>% unique(),
                    selected = max(df_ehcp %>% filter(AcademicYear != "Total") %>% pull(AcademicYear))
                  ),
                  p(" "),
                  valueBoxOutput("send_box_1", width = 6),
                  valueBoxOutput("send_box_3", width = 6),
                  p("Proportion of pupils living in the properties completed up to the selected academic year that required Special Education Needs (SEN) support or had Education, Health and Care Plans (EHCPs).
")
                )
              ),
              gov_row(
                tags$hr(),
                column(
                  width = 12,
                  paste("Download the underlying data for this dashboard:"), br(),
                  downloadButton(
                    outputId = "download_send_data",
                    label = "Download data",
                    icon = shiny::icon("download"),
                    class = "downloadButton"
                  )
                )
              )
            )
          )
        )
        # add box to show user input
      )
    )
  )
}

technical_panel <- function() {
  tabPanel(
    value = "technical",
    "Technical Notes",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Technical Notes"),
          h2("Data Sources"),
          p("Data for this project were obtained from the Ordnance Survey (OS), the Valuation Office Agency (VOA) via the Office for National Statistics (ONS), and the Department for Education’s (DfE) National Pupil Database (NPD). Each data source and its use are described in the relevant sections below."),
          h2("Properties - OS New Build Data"),
          p("The OS Consultancy and Technical Services team used", a(href = "https://www.ordnancesurvey.co.uk/products/addressbase", "OS AddressBase"), "to identify new build or newly developed properties.  This dataset is referred to as the OS new build data and was provided to the DfE in June 2022, under the Public Sector Geospatial Agreement (PSGA). The dataset allows for analysis by housing type, and tenure. Data are available for 2008 onwards."),
          p("For more information about the PSGA and how to access the OS AddressBase, see", a(href = "https://www.ordnancesurvey.co.uk/customers/public-sector/public-sector-geospatial-agreement", " OS information.")),
          p("The following files were obtained from OS."),
          tableOutput("technicaltable"),
        )
      ),
      h2("Properties - VOA Property Bedroom Data"),
      p("The ONS provided us with property bedroom data from the VOA. This data gives the number of bedrooms for a given UPRN."),
      strong("Limitations"),
      p("Due to issues around disclosure, the ONS were not able to provide bedroom data for all properties. The VOA data was joined on to the OS new build data via the UPRN to give the number of bedrooms for each property where this was available. There was bedroom data available for 78.3% of the properties in the OS new build data. We have described below how we attributed assumed bedroom numbers to the remaining “unknowns” after pupil addresses were matched to the OS new build data. More information on the detailed method for the VOA data joining can be provided on request."),
      h2("Pupils - National Pupil Database"),
      p("The National Pupil Database is stored centrally within the DfE. This contains returns from the termly School Census, alongside many other datasets. "),
      p("The following datasets from the NPD were used to count the number of pupils in each development over time and allow for breakdowns such as phase, education type, etc."),
      p("•	The School Census, which covers pupils in maintained schools and nurseries. "),
      p("•	The Early Years (EY) Census, which has information on children attending any private, voluntary and independent sector nursery with children receiving funding from the Department. "),
      p("•	The Alternative Provision (AP) Census, which has information on children in AP ( i.e. a school not maintained by an LA but which the authority is paying full tuition fees for). "),
      p("•	The Pupil Referral Unit (PRU) census for years 2009-2012 (after which pupils are included in the School Census). "),
      p("•	The Individualised Learner Record (ILR), which covers all young people in Further Education.", (strong(" Note:")), "The ILR is not within the NPD itself."),
      br(),
      p("If refreshing this pupil yield analysis in future, local authorities would need to use the pupil census returns from their area to replicate and expand this dataset."),
      h2("Data Cleaning"),
      p("Before attempting the address matching between property and pupil data described below, DfE cleaned the pupil data to remove formatting inconsistencies and identify any missing data. With increased use of Unique Property Reference Numbers (UPRNs) in pupil census collections in future years, such data cleaning should significantly reduce. Given that LAs will have access to school census data in their area, they may find it easier to source and clean their data. More information on the detailed method for data cleaning to enable address matching can be provided on request."),
      h2("Data Linking"),
      p("To obtain a pupil yield for each development, property addresses needed to be matched to pupil addresses in the NPD. Firstly, the OS dataset was reduced to only include new build developments of size ten or more dwellings which commenced and completed between January 2008 and 2022. Since the national OS dataset was constructed in 2007, any new builds prior to this time are not robustly identified. As 2008 is the earliest year available for OS new build data, the number of development schemes in the data increases as the years progress."),
      p("Due to the inconsistencies in how pupil addresses were recorded in the NPD, we employed exact and probabilistic address matching techniques to ensure accuracy and reliability in the pupil yield data. Full details of the methods used are available on request. However, we are working with MIS suppliers to enable increased use of UPRNs with pupil addresses, which should simplify the address matching process for local authorities producing similar pupil yield data in future."),
      h2("Testing"),
      p("The accuracy of the address matching was tested using a sample of 200,000 addresses from the pupil census and testing how accurately they were matched to the OS new builds data. Testing showed that the majority of the matching came from exact matching. The partial matching was 96.41% correct, meaning that there were 3.39% of partially matched addresses that should not have been partially matched. The unmatched addresses were 92.85% correct, meaning that there were 7.15% of addresses that were not matched that should have been."),
      p("Overall (assuming the percentages extend for the entire dataset), the accuracy level of the matching is 96.29%, where the error of 3.71% is made up of incorrectly matched addresses (0.27%) and un-matched addresses that should have matched (3.45%). More information on the testing we carried out is available on request."),
      h2("Building the dataset"),
      p("The address matching outlined above results in a table of pupil reference number, address as given in the census and all the relevant OS new build data. This table is then joined with the cleaned census data on pupil reference and full address to provide a final matched table that contains all the census addresses across all academic years that have been identified as a new build and has the required fields from the census and the OS new build data. The table can then be aggregated ready for analysis."),
      p("Property bedroom data was available for 70% of the properties after address-matching (lower than prior to matching). Addresses where the bedroom data was not available were labelled as ‘Unknown’. To ensure these properties were not “lost” from the data, we distributed this count across 2, 3 and 4+ bedrooms proportionally at a scheme level (or LA level when there was no bedroom data at a scheme level) by the housing type and affordable/market tenure."),
      h2("Pupil Yield Analysis"),
      p("From the linked dataset, we calculated pupil yield factors by dividing the number of pupils by the number of completed properties, for each applicable breakdown combination. Within these yield factors, a pupil is counted for every year that they live in a new build development. For example, a child would be included in annual counts for year one, two, three, etc. They will continue to be counted even if they change phase (e.g. from Primary to Secondary) but will at this point be captured in the yield factors for their new phase."),
      p("The yields can be broken down by education setting, phase, academic year, housing type, market/affordable tenure and number of bedrooms."),
      p(strong("Education setting:"), ("If the pupil is in an AP, PRU or special school then we defined the setting as a special setting, otherwise mainstream.")),
      p(strong("Phase:"), ("Early years, post-16, primary, secondary and special (where special covers all phases where a pupil is in a special, AP or PRU school or is in a post 16 setting with a EHCP up to the age of 25)")),
      p(strong("Academic year:"), ("For pupils, this is the number of pupils in new build properties in the Spring census for that academic year. For properties, this is the number of completed properties up to the start of that academic year. For example, in the academic year 2021/22 we have the pupil numbers in the Spring census (in Jan 2022) and the number of completed properties up to and including 31st August 2021.")),
      p(strong("Housing type:"), ("Flat, house, or total (all properties)")),
      p(strong("Property tenure:"), ("Whether affordable or market housing, or total (all properties)")),
      p(strong("Bedrooms: "), ("Number of bedrooms in the property, if available, or total (all properties)")),
      br(),
      p(strong("Limitations")),
      p("Calculating pupil yield factors for developments that commenced and completed between January 2008 and 2022  helps to demonstrate when pupil yields peak and stabilise, though in many cases secondary phase pupil yields are on an upward trajectory and the data time period is not long enough to show the peak."),
      p("DfE’s pupil yield data counts all pupils living in the sample developments, whether they moved into the property from elsewhere or were born there. We have not discounted pupils who have moved within the local area, recognising that patterns of pupil migration and the backfilling of vacated properties by more incoming families will vary from place to place. Local authorities can adjust our pupil yield factors to account for local evidence, where appropriate."),

      ## h2("Sub Heading"),
      ## p("Add text"),
      ## h2("Sub Heading"),
      ## p("Add text here"),
      ## tags$div(tags$ul(
      ## tags$li("add text"),
      ## tags$li("add text"),
      ## tags$li("add text"),
    )
  )
}
