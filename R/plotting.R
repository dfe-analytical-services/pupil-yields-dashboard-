create_bar_headline <- function(df, inputArea, xaxis, breakdown) {
  ggplot(
    df,
    aes(
      x = get(xaxis$colid),
      y = pupil_yield,
      fill = get(breakdown$colid),
      text = paste0(
        "<i>Pupil Yield</i>: ", round(pupil_yield, 2),
        "<br><b>", xaxis$name, "</b>: ", get(xaxis$colid),
        "<br><b>", breakdown$name, "</b>: ", get(breakdown$colid)
      )
    )
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    scale_fill_manual(values = dfe_palette, name = breakdown$name) +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0),
      legend.position = "top"
    ) +
    ylim(0, max(c(0.6, df$pupil_yield * 1.02))) +
    xlab(xaxis$name) +
    ylab("Pupil Yield")
}

create_py_time_period <- function(dfPY) {
  dfPY_filtered <- dfPY
  mean_py <- mean(dfPY_filtered$pupil_yield)
  ggplot(
    dfPY_filtered %>% filter(time_period != "All"),
    aes(
      x = time_period,
      y = pupil_yield,
      group = tenure,
      text = paste0(
        "<i>Pupil Yield</i>: ", pupil_yield,
        "<br><b>Financial year</b>: ", time_period
      )
    )
  ) +
    geom_line(aes(color = "Yearly Pupil Yield")) +
    geom_hline(aes(yintercept = mean_py, color = "Mean Pupil Yield"), linetype = "dashed") +
    scale_colour_manual(values = dfe_palette) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 320),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line(size = 1.0),
      legend.position = "top"
    ) +
    #    ylim(0, max(c(0.6, dfPY_filtered$pupil_yield * 1.02))) +
    xlab("Year") +
    ylab("Pupil Yield") +
    labs(color = "")
}
