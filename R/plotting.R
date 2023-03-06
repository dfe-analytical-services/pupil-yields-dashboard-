createPYbarchart <- function(df){

ggplot(df, aes(x=tenure,y=pupil_yield,fill=housing)) + 
  geom_bar(stat = "identity",position='dodge') +       
  theme_classic() +
    scale_fill_manual(values = dfe_palette)+
  theme(
    text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.line = element_line( size = 1.0),
    legend.position = 'top'
  )  +
  xlab("Tenure") +
  ylab("Pupil yield") 
}

plotPYtime_period <- function(dfPY){
  
  ggplot(dfPY, 
         aes(
           x=time_period,
           y=pupil_yield
           ) 
         ) +
    geom_line() +       
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 300),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.line = element_line( size = 1.0),
      legend.position = "none"
    ) +
    xlab("Year") +
    ylab("Pupil Yield")
    
}
