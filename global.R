# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(DT))
shhh(library(xfun))
shhh(library(tidyr))

# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
}

# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"

source("R/support_links.R")
source("R/read_data.R")

# Read in the data
df_py <- read_revenue_data()
# Get geographical levels from data

choicesgeographic_level <- unique(df_py$geographic_level)

choicesLAs <- unique(df_py$la_name)

choicesYears <- unique(df_py$time_period)

choiceseducation_type <- unique(df_py$education_type) %>% sort()

choicesPhase <- unique(df_py$education_phase)

# choicesaffordability <- unique(df_py$affordability)

choicesnumber_developments <- unique(df_py$number_developments)

# choicesrurality <- unique(df_py$rurality)

choiceshousing <- unique(df_py$housing)

choicestenure <- unique(df_py$tenure)

# choiceshimidlow <- unique(df_py$himidlow)

choicesnumber_beds <- unique(df_py$number_of_bedrooms)

choicesearly_years_uplift <- unique(df_py$early_years_uplift)

dfe_palette <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3D3D3D", "#A285D1")
