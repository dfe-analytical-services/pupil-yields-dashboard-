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
                  p("The Pupil Yield Dashboard provides pupil yield factors for each local authority in England, at county, unitary and district level (where applicable). Pupil yield factors can be displayed by education type (mainstream or special), phase (early years, primary, secondary and post-16) and filtered according to housing type (flats/houses), tenure (market/affordable) and size (bedroom numbers). Alongside headline figures for each local authority, there is a graph showing annual pupil yield factors since 2008, and how pupil yield develops over time."),
                  p("The technical notes with this dashboard are provided for local authorities with a responsibility for providing sufficient school places under the Education Act 1996 – principally analysts/data scientists involved in pupil forecasting and other population modelling."),
                  ## p(actionLink("link_to_app_content_tab", "Dashboard panel")),
                  ## p("You need to add an observeEvent() function to the server.R script for any link that navigates within your App.")
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
                p("DfE has worked with the Office for National Statistics (ONS) to develop a recommended methodology for estimating pupil yield from housing development, to assist local authorities demonstrating the need for education facilities during local plan preparation and the consideration of planning applications. There are technical notes explaining the processes employed in preparing pupil yield data, sitting alongside new guidance, so that local authorities will be able to replicate our methodology when producing similar local pupil yield data in the future."),
                p(""),
                ## h3("Guidance sources"),
                ## p("For example, here we'll add some of the key resources we draw on to guide styling and vizualisation...")
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
          width = 12,
          h1("Pupil Yield Data Dashboard"),
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
                    inputId = "selectArea",
                    label = "Choose a Geography:",
                    choices = choicesgeographic_level
                  )
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "selectLA",
                    label = "Choose a LA:",
                    choices = choicesLAs,
                    selected = "LA1"
                  )
                )
              ),
              column(
                width = 12,
                paste("Download the underlying data for this dashboard:"), br(),
                downloadButton(
                  outputId = "download_data",
                  label = "Download data",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                )
              )
            )
          )
        ),
        column(
          width = 12,
          tabsetPanel(
            id = "tabsetpanels",
            tabPanel(
              "Headlines",
              gov_row(
                column(
                  width = 12,
                  uiOutput("headline_title"),
                  radioGroupButtons(
                    "bartab_toggle",
                    label = NULL,
                    choices = c("Chart", "Table"),
                    selected = "Chart"
                  ),
                  uiOutput("headlines_data")
                )
              ),
              gov_row(
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "select_xaxis",
                    label = "Choose x-axis variable",
                    choices = filter_list %>% filter(!(name %in% c("Early years uplift"))) %>% pull(name),
                    selected = "School phase"
                  )
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "select_breakdown",
                    label = "Choose breakdown variable",
                    choices = filter_list %>% filter(!(name %in% c("Housing type", "Tenure"))) %>% pull(name),
                    selected = "Housing type"
                  )
                ),
                column(
                  width = 4,
                  selectizeInput(
                    inputId = "select_year",
                    label = "Choose a Year:",
                    choices = choicesYears,
                    selected = choicesYears[1]
                  )
                )
              ),
              gov_row(
                column(
                  12,
                  tags$h2("Choose the chart filters here:"),
                ),
                column(
                  width = 3,
                  selectizeInput(
                    inputId = "filter1",
                    label = "Choose a Phase:",
                    choices = choicesPhase,
                    selected = "All"
                  )
                ),
                column(
                  width = 3,
                  selectizeInput(
                    inputId = "filter2",
                    choices = choiceseducation_type,
                    label = "Choose an Education Type:",
                    selected = "All"
                  )
                ),
                column(
                  width = 3,
                  selectizeInput("filter3",
                    "Choose a Housing Type:",
                    choices = choiceshousing,
                    selected = "All"
                  )
                ),
                column(
                  width = 3,
                  selectizeInput("filter4",
                    "Choose Early Years Uplift:",
                    choices = choicesearly_years_uplift,
                    selected = "Off"
                  ),
                )
              )
            ),
            tabPanel(
              "Peaks & Averages",
              fluidRow(
                column(
                  width = 12,
                  h2("Pupil Yield showing peak and average"),
                  p("This is the standard paragraph style for adding guiding info around data content."),
                  column(
                    width = 12,
                    radioGroupButtons(
                      "timetab_toggle",
                      label = NULL,
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    uiOutput("timeseries_data")
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
    "Technical",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Technical Details"),
          h2("Data Sources"),
          p("Data for this project were obtained from the Ordnance Survey (OS) under the Public Sector Geospatial Agreement (PSGA), the ONS, and the National Pupil Database (NPD) which is held by the Department for Education. All data sources and their use are described in the relevant sections below."),
          h2("OS New Build Data"),
          p("The OS build-out data is provided by the Ordnance Survey (OS) Consultancy and Technical Services team as derived content, using OS AddressBase to identify new build or newly developed properties."),
          p("For more information, including how to access the OS AddressBase, see OS information about the Public Sector Geospatial Agreement (PSGA), which is available to all public sector organisations. DfE received AddressBase data from OS in June 2022."),
        )
      ),
      h2("National Pupil Database"),
      p("DfE stores all pupil information from the National Pupil Database centrally in the Pupil Data Repository (PDR). This contains returns from the termly School Census, alongside many other datasets. For this project, the School Census provided the primary data source, given that it contains pupil addresses and ages. We supplemented this with the Early Years (EY) Census (pupil-level returns from providers with children in receipt of a government funded place), the Alternative Provision Census (such as special schools, in-patients at hospitals, etc.), the Pupil Referral Unit (PRU) census (for years 2009-2012, after which pupils are included in the School Census), and the Individualised Learner Record (ILR) for all young people in Further Education (FE). This data is required to build accurate pupil profiles within new developments, allowing education type, phase and Special Educational Needs (SEN) to be constructed and analysed, and broken down by housing type, tenure and size (bedroom numbers)."),
      p("If replicating this pupil yield research in future, local authorities would use the pupil census returns from their area to identify addresses and other information."),
      h2("Data Cleaning"),
      p("DfE cleaned the pupil data before attempting address matching to remove formatting inconsistencies and human errors and identify any missing data. Data cleaning of the OS new build data was sone in R Studio, and the data cleaning of the pupil census data was done in SQL Server Management Studio. With increased use of Unique Property Reference Numbers (UPRNs) in pupil census collections in future years, such data cleaning should become unnecessary."),
      h2("OS Residential New Build Data"),
      tableOutput("technicaltable"),
      p("To tidy above table format"),
      h2("Pupil Data Repository (PDR) Data"),
      p("School Census data (and the other relevant census data) are accessed by DfE through the PDR. These data are given in whichever format a school has submitted its census details and therefore formatting and data quality are variable. Given that LAs will have access to school census data in their area, they may find it easier to source and clean data at LA level than we have at national level. More information on the detailed method for data cleaning to enable address matching can be provided on request. However, we are working with Management Information System (MIS) suppliers to enable increased use of UPRNs with pupil addresses, minimising the need for data cleaning in future."),
      h2("Data Linking"),
      p("To obtain a pupil yield for each development, property addresses needed to be matched to pupil addresses in the PDR. Before any matching began, the OS dataset was subset to only include new build developments of size ten or more dwellings which commenced and completed between January 2008 and. 2008 is the earliest year available for OS housing build-out data, and there are more development schemes in the data as the years progress."),
      h2("Exact and probabilistic matching"),
      p("Due to the inconsistencies in how pupil addresses were recorded in the PDR, we employed exact and probabilistic address matching techniques to ensure accuracy and reliability in the pupil yield data. Full details of the methods used are available on request. However, we are working with MIS suppliers to enable increased use of UPRNs with pupil addresses, which should simplify the address matching process for local authorities producing similar pupil yield data in future."),
      h2("Testing"),
      p("The accuracy of the address matching was tested using 200,000 addresses from the pupil census and testing how accurately they were matched to the OS new builds data. The testing was carried out by running the addresses through the exact matching and then the remaining unmatched addresses were put through the probabilistic matching. A manual check was done between the census address and new build address to determine if each match was correct. The unmatched census addresses were also checked to determine if they were correctly not matched to a new build – this was done by looking at all the properties in the postcode of the unmatched address and checking if any matched."),
      p("Testing showed that the majority of the matching came from exact matching. The partial matching was 96.41% correct, meaning that there were 3.39% of partially matched addresses that should not have been matched. The unmatched addresses were 92.85% correct, meaning that there were 7.15% of addresses that were not matched that should have been."),
      p("Overall, (assuming the percentages extend for the entire exact matched and unmatched samples), this test of 200,000 addresses provides an accuracy of 96.29%, where the error of 3.71% is made up of incorrectly matched addresses (0.27%) and missed addresses that should have matched but did not (3.45%). More information on the testing we carried out is available on request."),
      h2("Matching To Census"),
      p("The address matching outlined above results in a table of pupil reference number, address as given in the census and all the relevant OS new build data. This table is then joined with the cleaned census data on pupil reference and full address to provide a final matched table that contains all the census addresses across all academic years that have been identified as a new build and has the required fields from the census and the OS new build data. The table is then to be aggregated ready to be used in the pupil yields dashboard."),
      p("To add in VOA"),
      h2("Pupil Yield Analysis"),
      p("From the linked dataset, we calculated pupil yield metrics."),
      p("The pupil yield research aims to measure the number of pupils living in the sample of developments in each year, from pre-completion since 2008 through to 2022. Within these yield factors, a pupil is counted for every year that they live in a new build development. For example, a child would be included in annual counts for year one, two, three, etc., once they enter a new build property. They will continue to be counted even if they change phase (for instance from Primary to Secondary) but will at this point be captured in the yield factors for their new phase."),
      p("Since the national OS dataset was constructed in 2007, any new builds prior to this time are not robustly identified. As such, we included developments which both commenced and completed since 2008. Calculating pupil yield factors over this time frame helps to demonstrate when pupil yields peak and stabilise, though in many cases secondary phase pupil yields are on an upward trajectory and the data time period is not long enough to show the peak."),
      p("DfE’s pupil yield data counts all pupils living in the sample developments, whether they moved into the property from elsewhere or were born there. We have not discounted pupils who have moved within the local area, recognising that patterns of pupil migration and the backfilling of vacated properties by more incoming families will vary from place to place. Local authorities can adjust our pupil yield factors to account for local evidence, where appropriate."),
      p("Local authorities can also choose whether or not to apply our Early Years Uplift. This has been included as an option to account for young children who attend early years settings but are not taking up a government-funded place under DfE’s early years entitlements, and are therefore not counted in the Early Years or School Census (until they reach school age)."),
      p("The department collects the ‘Childcare and early years survey of parents’, which has included percentages for Early Years uptake within the sample population since the 2009-10 academic year. We use these and assume they are national estimates, applying the percentages to the mid-year population counts from the ONS for ages zero to four. By subtracting our own Early Years counts in the PDR from those calculated from the percentage uptake of the overall Early Years population, a figure is equated which comprises those pupils missed in the Early Years and School Census. Further detail on this method is available on request, and more information is also available at Childcare and Early Years Survey of Parents and Mid-Year Population Estimates. Local authorities with more granular data on the numbers of children taking up places in early years settings that are not counted in the Early Years or School Census collections may wish to use this in preference to our Early Years Uplift."),
      p("To insert weblinks in above line"),

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
