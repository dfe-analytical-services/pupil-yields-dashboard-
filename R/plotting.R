create_bar_headline <- function(df, inputArea, xaxis, breakdown) {
  ggplot(
    df,
    aes(x = get(xaxis), y = pupil_yield, fill = get(breakdown))
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    scale_fill_manual(values = dfe_palette, name=breakdown) +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0),
      legend.position = "top"
    ) +
    xlab(xaxis) +
    ylab("Pupil yield")
}

create_py_time_period <- function(dfPY){
  print(dfPY)
  dfPY_filtered <- dfPY %>% filter(tenure=='All', housing=='All', number_of_bedrooms=='All', education_phase=='Secondary')
  mean_py <- mean(dfPY_filtered$pupil_yield)
  print(dfPY_filtered)
  print(mean_py)
  ggplot(dfPY_filtered, aes(
    x=time_period,
    y=pupil_yield,
    color=education_phase
    )
         ) + 
    geom_line() +       
    geom_hline(yintercept=mean_py) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0),
      legend.position = "none"
    ) +
    xlab("Year") +
    ylab("Pupil Yield")
}
