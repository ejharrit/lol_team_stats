library(ggplot2)
theme_dis <- theme_minimal(base_size = 14, base_family = "Georgia")+
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="gray96", colour=NA),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key        = element_rect(fill="transparent", colour=NA),
      plot.title = element_text(
        color = rgb(251, 146, 28, maxColorValue = 255), 
        face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(51, 51, 51, maxColorValue = 255),
        size = rel(0.8)),
      axis.text = element_text(
        color = rgb(51, 51, 51, maxColorValue = 255),
        size = rel(0.65)),
      panel.grid.major = element_line(
        rgb(51, 51, 51, maxColorValue = 255),
        linetype = "dotted")
    )

