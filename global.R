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
shhh(library(tidyr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(DT))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))
shhh(library(checkmate))
# shhh(library(shinya11y))

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
  message("R scripts")
  message("----------------------------------------")
  r_scripts <- eval(styler::style_dir("R/")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, r_scripts, test_scripts)
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
sites_list <- c(site_primary, site_overflow) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "Statistical publication" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/" # Update with parent publication link
google_analytics_key <- "Z967JJVQQX"

source("R/read_data.R")

la_lad_lookup <- read.csv("data/la_lad_hierarchy.csv", stringsAsFactors = F) %>%
  mutate(
    la_name = gsub(",", "", la_name),
    lad_name = gsub(",", "", lad_name),
    date_of_introduction=as.Date(date_of_introduction),
    date_of_termination=as.Date(date_of_termination)
  ) %>%
  filter(
    status == "live" | date_of_termination >= as.Date("2023-03-31"),
    date_of_introduction <= as.Date("2023-03-31") | is.na(date_of_introduction)
  )

# Read in the data
df_py <- read_data()
df_ehcp <- read_ehcp()
df_pc <- read_pc()

# Get geographical levels from data

df_py$education_phase <- factor(df_py$education_phase, levels = )

choicesgeographic_level <- c("England", "County/Unitary", "District")
choicesLAs <- df_py %>%
  filter(geographic_level == "County/Unitary" | la_name=='Cardiff') %>%
  pull(la_name) %>%
  unique() %>%
  sort()
choicesLADs <- df_py %>%
  filter(geographic_level == "District") %>%
  pull(la_name) %>%
  unique() %>%
  sort()

choicesYears <- unique(df_py$time_period) %>% sort(decreasing = TRUE)
df_py$time_period <- factor(df_py$time_period, levels = choicesYears %>% sort())


filter_list <- data.frame(
  name = c("School phase", "School type", "Housing type", "Number of bedrooms", "Tenure"),
  colid = c("education_phase", "education_type", "housing", "number_of_bedrooms", "tenure"),
  default = c("Primary", "Mainstream", "All", "All", "All")
)

choiceseducation_type <- unique(df_py$education_type) %>% sort()

choicesPhase <- c("Early Years", "Primary", "Secondary", "Post-16", "SP/Alternative")
df_py$education_phase <- factor(df_py$education_phase, levels = choicesPhase)

# choicesaffordability <- unique(df_py$affordability)

choicesnumber_developments <- c("All", unique(df_py$number_developments) %>% sort()) %>% unique()

# choicesrurality <- unique(df_py$rurality)

choiceshousing <- c("All", unique(df_py$housing) %>% sort()) %>% unique()

choicestenure <- c("All", unique(df_py$tenure) %>% sort()) %>% unique()

# choiceshimidlow <- unique(df_py$himidlow)

choicesnumber_beds <- c("All", unique(df_py$number_of_bedrooms) %>% sort()) %>% unique()

choices <- list(
  education_type = choiceseducation_type,
  education_phase = choicesPhase,
  housing = choiceshousing,
  tenure = choicestenure,
  number_of_bedrooms = choicesnumber_beds
  
)

#post completion data
#Get geographical levels from data

df_pc$education_phase <- factor(df_pc$education_phase, levels = )

choicesgeographic_levelpc <- c("England", "County/Unitary", "District")
choicesLAs <- df_pc %>%
  filter(geographic_level == "County/Unitary" | la_name=='Cardiff') %>%
  pull(la_name) %>%
  unique() %>%
  sort()
choicesLADspc <- df_pc %>%
  filter(geographic_level == "District") %>%
  pull(la_name) %>%
  unique() %>%
  sort()

choicesYearspc <- unique(df_pc$time_period) %>% sort(decreasing = TRUE)
df_pc$time_period <- factor(df_pc$time_period, levels = choicesYearspc %>% sort())


filter_listpc <- data.frame(
  name = c("School phase", "School type", "Years After Completion"),
  colid = c("education_phase", "education_type", "years_after_completion" ),
  default = c("Primary", "Mainstream", "All" )
)

choiceseducation_typepc <- unique(df_pc$education_type) %>% sort()

choicesPhasepc <- c("Early Years", "Primary", "Secondary", "Post-16", "SP/Alternative")
df_pc$education_phasepc <- factor(df_pc$education_phase, levels = choicesPhasepc)

choicesYearsAfterCompletion <- unique(df_pc$years_after_completion) %>% sort(decreasing = TRUE)
df_pc$years_after_completion <- factor(df_pc$years_after_completion, levels = choicesYearsAfterCompletion %>% sort())

choicespc <- list(
  education_type = choiceseducation_typepc,
  education_phase = choicesPhasepc,
  years_after_completion = choicesYearspc

)


dfe_palette <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3D3D3D", "#A285D1")

technical_table <- read.csv("data/TechnicalTable.csv")
