a11y_panel <- function() {
  tabPanel(
    "Accessibility",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Accessibility statement"),
          p("This accessibility statement applies to the Department for Education's Pupil Yields Dashboard. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
          h2("WCAG 2.1 compliance"),
          p(
            "We follow the reccomendations of the ", 
            a(style = "color:#007fb0", href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), 
            "This application has been checked using the ", a(style = "color:#007fb0", href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), 
            ", which detected a small number of accessibility issues as outlined below. Each page in this application has been audited for accessiblity with the page-snapshot functionality in the ",
            a(style = "color:#007fb0", href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"),
            ". This application does not fully pass the accessibility auditing, partially due to limitations in the software (R-Shiny) used to produce the dashboard. ",
            "Specific reasons for this are outlined in the limitations section below, whilst to mitigate the accessibility challenges here, we also provide the", 
            tags$a(href="https://explore-education-statistics.service.gov.uk/find-statistics/pupil-yield-from-housing-developments","underlying data via the DfE Explore Education Statistics platform"), ". ",
            "This app does however follow the following guidelines:"
          ),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
            tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
          )),
          h2("Limitations"),
          p(
            "We recognise that there are still issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users." # For example, these
          ),
          tags$div(tags$ul(
            tags$li("some elements fail to have the appropriate aria tags"),
            tags$li("some table header ids are not assigned correctly"),
            tags$li("some image elements do not have an alt attributes (note that where this is the case, those images are primarily for presentation )")
          )),
          h2("Browser compatibility"),
          p("The browsers used were Edge Chromium and Chrome as these are standard in the DfE and its agencies. The operating system used was Windows."),
          p("This statement was prepared and last updated on 8th August 2023."),
          h2("Feedback"),
          p(
            "If you have any feedback on how we could further improve the accessibility of this application, please contact",
            a(href = "mailto:sylvia.williams@education.gov.uk", "sylvia.williams@education.gov.uk")
          )
        )
      )
    )
  )
}

support_links <- function() {
  tabPanel(
    "Support & Feedback",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Give us feedback"),
          "The analysis in this dashboard is new. The dashboard itself is also hosted on a new platform that will continue to be developed. If you have any feedback or suggestions for improvements, please email ",
          a(href = "mailto:Pupil.Yields@education.gov.uk", "Pupil.Yields@education.gov.uk", .noWS = c("after")), ".",
          br(),
          h2("Find more information on the data"),
          "The data displayed in the dashboard can be downloaded from the Dashboard tab. This is not a full set of underlying data that feeds the calculation, which is subject to data protection rules. ",
          br(),
          br("A summary of  methodological information can be found in the Technical Notes tab. Full details of the methods used are available on request from ",
          a(href = "mailto:Pupil.Yields@education.gov.uk", "Pupil.Yields@education.gov.uk", .noWS = c("after")),
          ".",
          br(),
          h2("Contact us"),
          "If you have questions about the dashboard or data within it, please contact ",
          a(href = "mailto:Pupil.Yields@education.gov.uk", "Pupil.Yields@education.gov.uk", .noWS = c("after")), br(),
          h2("See the source code"),
          "The source code for this dashboard is available in our ",
          a(href = "https://github.com/dfe-analytical-services/pupil-yields-dashboard-", "GitHub repository", .noWS = c("after")),
          ". Code used to produce the analysis cannot be shared as it is specific to DfE systems.",
          br(),
          br(),
          br(),
          br(),
          br(),
          br()
          )
        )
      )
    )
  )
}
