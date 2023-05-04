create_bar_headline <- function(df, inputArea, xaxis, breakdown) {
  ggplot(
    df,
    aes(
      x    = get(xaxis$colid), 
      y    = pupil_yield, 
      fill = get(breakdown$colid),
      text = paste0('<i>Pupil Yield</i>: ',pupil_yield,
                    '<br><b>',xaxis$name,'</b>: ', get(xaxis$colid),
                    '<br><b>',breakdown$name,'</b>: ', get(breakdown$colid))
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
    xlab(xaxis$name) +
    ylab("Pupil yield")
}

create_py_time_period <- function(dfPY) {
  dfPY_filtered <- dfPY
  mean_py <- mean(dfPY_filtered$pupil_yield)
  ggplot(dfPY_filtered, aes(
    x = time_period,
    y = pupil_yield,
    group = tenure,
    text = paste0('<i>Pupil yield</i>: ',pupil_yield,
                  '<br><b>Financial year</b>: ', time_period)
  )) +
    geom_line(color = dfe_palette[1]) +
    geom_hline(yintercept = mean_py, color = dfe_palette[2], linetype = "dashed") +
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
