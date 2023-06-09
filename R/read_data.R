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

read_data <- function(file = "data/YieldsDummyData.csv") {
  # This reads in an example file. For the purposes of this demo, we're using the
  # latest test data.
  df <- read.csv(file)
  df$time_period <- paste(
    substr(df$time_period, 1, 4),
    substr(df$time_period, 5, 6),
    sep = "/"
  )

  df_means <-df%>%filter(tenure=="All",
                         housing=="All",
                         number_of_bedrooms=="All",
                         early_years_uplift=="off"
                         )%>%group_by(education_phase,
                                      la_name,
                                      education_type,
                                      geographic_level)%>%
    summarise(pupil_yield=mean(pupil_yield))%>%
    mutate(time_period="All",
           tenure="All",
           housing="All",
           number_of_bedrooms="All",
           early_years_uplift="off",
           completed_properties_in_fy=NA,
           number_of_pupils=NA)%>%
    select(time_period,
           la_name,
           tenure,
           housing,
           number_of_bedrooms,
           education_phase,
           education_type,
           geographic_level,
           number_of_pupils,
           early_years_uplift,
           completed_properties_in_fy,
           pupil_yield)
  df<-df%>%rbind(df_means)

  return(df)
}
