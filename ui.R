# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

ui <- function(input, output, session) {
  navbarPage(
    useShinyjs(),
    includeCSS("www/shiny_gov_style.css"),
    useShinydashboard(),

    # Application title -----------------------------------------------------------------------------------
    title = "Title of app here",
    footer = p(
      HTML("&nbsp;"),
      img(src = "dfe_logo.svg", height = 100, width = 150, alt = "Department for Education logo"),
      br(),
      p(
        HTML("&nbsp;"), "Link back to relevant statistics on ", a(href = "https://explore-education-statistics.service.gov.uk/", "EES."),
        br(),
        HTML("&nbsp;"), "If you would like to provide feedback on this dashboard, please complete our ",
        a(href = "https://forms.office.com/", "online survey"),
        br(),
        HTML("&nbsp;"), "Underlying code for this application can be found in ", a(href = "https://github.com/dfe-analytical-services/shiny-template", "our online repo."),
      )
    ),

    # App code goes here -----------------------------------------------------------------------------------

    # Create first tab--------------------------------------------------------------------------------------

    tabPanel(
      "App content",

      # Define UI for application that draws a histogram

      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          width = 2,
          sliderInput("bins",
            "Number of bins:",
            min = 1,
            max = 50,
            value = 30
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          width = 10,
          plotOutput("distPlot"),
          br(),
          # add box to show user input
          valueBoxOutput("box_info", width = 6)
        )
      )
    ),

    # Create the accessibility statement-----------------
    tabPanel(
      "Accessibility",
      h2("Accessibility statement"),
      br("This accessibility statement applies to the **application name**.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
      h3("WCAG 2.1 compliance"),
      br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
      tags$div(tags$ul(
        tags$li("uses colours that have sufficient contrast"),
        tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
        tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
      )),
      h3("Limitations"),
      br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
      tags$div(tags$ul(
        tags$li("List"),
        tags$li("known"),
        tags$li("limitations, e.g."),
        tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
      )),
      h3("Feedback"),
      br(
        "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
        a(href = "mailto:email@education.gov.uk", "email@education.gov.uk")
      )
    ) # End of accessibility tab
  ) # End of navBarPage
} # End of ui
