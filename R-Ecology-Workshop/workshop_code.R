library(ggplot2)
library(ratdat)
?complete_old
View(complete_old)
str(complete_old)
#adding a mapping to x and y axes
ggplot(data = complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) + geom_point(alpha = 0.2)+ scale_x_log10()

myplot<-ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) + geom_jitter(alpha = 0.2, aes(color = plot_type)) + geom_boxplot(outlier.shape = NA, fill = NA)

ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length, color = plot_type)) + geom_jitter(alpha = 0.2, aes(color = plot_type)) + geom_violin(fill = "white" )

finalplot <- myplot + 
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(title = "Rodent size by plot type",
       x = "Plot type",
       y = "Hindfoot length (mm)") +
  labs(subtitle = "This plot is cool")+
  theme(myplot.subtitle = element_text(face = "bold"))+
  facet_wrap(vars(sex),ncol=1)

  ggsave(filename = "images/rodent_size_plots.jpg",plot = finalplot,height = 6, width = 8)
