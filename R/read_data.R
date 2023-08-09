# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.

read_data <- function(file = "data/PYJuly.csv") {
  # This reads in an example file. For the purposes of this demo, we're using the
  # latest test data.
  df <- read.csv(file) # %>%
  # filter(early_years_uplift == 'Off') %>% select(-early_years_uplift)
  df$time_period <- paste(
    substr(df$time_period, 1, 4),
    substr(df$time_period, 5, 6),
    sep = "/"
  )

  df_bedrooms2p <- df %>%
    filter(number_of_bedrooms %in% c("2", "3", "4+")) %>%
    summarise(
      number_of_pupils = sum(number_of_pupils),
      completed_properties_in_fy = sum(completed_properties_in_fy),
      .by = c(time_period, la_name, tenure, housing, education_phase, education_type, geographic_level)
    ) %>%
    mutate(number_of_bedrooms = "2+", .before = education_phase) %>%
    mutate(pupil_yield = number_of_pupils / completed_properties_in_fy)

  df_bedrooms3p <- df %>%
    filter(number_of_bedrooms %in% c("3", "4+")) %>%
    summarise(
      number_of_pupils = sum(number_of_pupils),
      completed_properties_in_fy = sum(completed_properties_in_fy),
      .by = c(time_period, la_name, tenure, housing, education_phase, education_type, geographic_level)
    ) %>%
    mutate(number_of_bedrooms = "3+", .before = education_phase) %>%
    mutate(pupil_yield = number_of_pupils / completed_properties_in_fy)

  df <- df %>%
    rbind(df_bedrooms2p) %>%
    rbind(df_bedrooms3p)

  # df_means <- df %>%
  # summarise(
  #  pupil_yield = mean(pupil_yield),
  # .by = c(education_phase, la_name, education_type, geographic_level, tenure, housing, number_of_bedrooms)
  # ) %>%
  # mutate(
  # time_period = "All",
  # completed_properties_in_fy = NA,
  # number_of_pupils = NA
  #  ) %>%
  # select(
  #  time_period,
  # la_name,
  # tenure,
  # housing,
  # number_of_bedrooms,
  # education_phase,
  # education_type,
  # geographic_level,
  # number_of_pupils,
  # completed_properties_in_fy,
  # pupil_yield
  # )
  # df <- df %>% rbind(df_means)

  df <- df %>%
    select(
      time_period, geographic_level, la_name, education_phase, education_type, tenure, housing,
      number_of_bedrooms, number_of_pupils, completed_properties_in_fy, pupil_yield
    ) %>%
    arrange(time_period, geographic_level, la_name, education_phase, education_type, tenure, housing, number_of_bedrooms) %>%
    mutate(
      tenure = factor(tenure, levels = c("Affordable", "Market", "All")),
      housing = factor(housing, levels = c("Flats", "Houses", "All"))
    )


  return(df)
}

read_ehcp <- function() {
  read.csv("data/EHCPJuly.csv", stringsAsFactors = FALSE) %>%
    mutate(
      AcademicYear = if_else(
        AcademicYear == "Total", "Total",
        paste(substr(AcademicYear, 1, 4), substr(AcademicYear, 5, 6), sep = "/")
      )
    )
}

# read in metadata to get variable names for files for download--------------
metadata_PY <- read.csv("data/PYMetadata.csv", encoding = "UTF-8")
metadata_EHCP <- read.csv("data/EHCPMetadata.csv", encoding = "UTF-8")
