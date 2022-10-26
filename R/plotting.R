createPYBarChart <- function(dfRevenueBalance,inputArea, group, subgroup='number_beds'){

ggplot(dfRevenueBalance, aes(x=.data[[group]],y=pupil_yield,fill=.data[[subgroup]])) + 
  geom_bar(stat = "identity",position='dodge') +       
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.line = element_line( size = 1.0),
    legend.position = 'top'
  )  +
  xlab(group) +
  ylab("Pupil Yield") 
}

plotAvgRevBenchmark <- function(dfRevenueBalance,inputArea){
  
  ggplot(dfRevenueBalance, aes(x=area_name,
                               y=average_revenue_balance,
                               fill=area_name)) + 
    geom_col() +       
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line( size = 1.0),
      legend.position = "none"
    ) +
    scale_y_continuous(
      labels = scales::number_format(accuracy = 1, big = ',', prefix='£')) +
    xlab("Area") +
    ylab("Average revenue balance") +
    scale_fill_manual(
      "Area",
      breaks = unique(dfRevenueBalance$area_name),
      values = gss_colour_pallette
    )     
}
