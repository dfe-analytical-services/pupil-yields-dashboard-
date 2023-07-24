a11y_panel <- function() {
  tabPanel(
    "Accessibility",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Accessibility statement"),
          h2("Using this dashboard"),
          br("This R-shiny dashboard is run by Department for Education (DfE). We follow the recommendations of the WCAG 2.1 requirements. "),
          br("We want as many people as possible to be able to use the report by:"),
          tags$div(tags$ul(
            tags$li("Navigating most of the dashboard using keyboard and speech recognition software"),
            tags$li("Use most of the dashboard using a screen reader (including the most recent version of JAWS)"),
            tags$li("Using assistive technologies such as ZoomText and Fusion"),
            tags$li("Using different platforms including laptops, tablets and mobile phones"),
            tags$li("Zoom in up to 300% without the text spilling off the screen")
          )),
          h2("WCAG 2.1 compliance"),
          br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
            tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
          )),
          h2("Limitations"),
          br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users."),
          ## tags$div(tags$ul(
          ##  tags$li("List"),
          ##  tags$li("known"),
          ##  tags$li("limitations, e.g."),
          ##  tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
          ## )),
          h2("Browser compatibility"),
          br("The browsers used were Edge Chromium and Chrome as these are standard in the DfE and its agencies. The operating system used was Windows."),
          br("This statement was prepared on 4th May 2023."),
          br("This statement was last updated on 4th May 2023."),
          h2("Feedback"),
          br(
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
          br("A summary of  methodological information can be found in the Technical tab. Full details of the methods used are available on request from ",
          a(href = "mailto:Pupil.Yields@education.gov.uk", "Pupil.Yields@education.gov.uk", .noWS = c("after")),
          ".",
          br(),
          h2("Contact us"),
          "If you have questions about the dashboard or data within it, please contact ",
          a(href = "mailto:Pupil.Yields@education.gov.uk", "Pupil.Yields@education.gov.uk", .noWS = c("after")), br(),
          h2("See the source code"),
          "The source code for this dashboard is available in our ",
          a(href = "https://github.com/dfe-analytical-services/shiny-template", "GitHub repository", .noWS = c("after")),
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
