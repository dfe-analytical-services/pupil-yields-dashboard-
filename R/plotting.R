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
    ylab("Pupil yield")
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
        "<i>Pupil yield</i>: ", pupil_yield,
        "<br><b>Financial year</b>: ", time_period
      )
    )
  ) +
    geom_line(aes(color = "Yearly pupil yield")) +
    geom_hline(aes(yintercept = mean_py, color = "Mean pupil yield"), linetype = "dashed") +
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

#post completion chart 
create_pc_time_period <- function(dfPC) {
  dfPC_filtered <- dfPC
  mean_pC <- mean(dfPC_filtered$pupil_yield)
  ggplot(
    dfPC_filtered %>% filter(time_period != "All"),
    aes(
      x = years_after_completion,
      y = pupil_yield,
      group = tenure,
      text = paste0(
        "<i>Pupil yield</i>: ", pupil_yield,
        "<br><b>Years after completion</b>: ", years_after_completion
      )
    )
  ) +
    geom_line(aes(color = "Yearly pupil yield")) +
    geom_hline(aes(yintercept = mean_py, color = "Mean pupil yield"), linetype = "dashed") +
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
    xlab("Years after completion") +
    ylab("Pupil Yield") +
    labs(color = "")
}