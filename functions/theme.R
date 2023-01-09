ax.title <- 14
ax.text <- 12
ax.legend <- 12
strip <- 12
a4_width <- 160
my_theme <- theme_classic() + theme( axis.text = element_text(size = ax.text),
                                     text = element_text(size = ax.text),
                                     axis.title = element_text(size = ax.title),
                                     axis.title.y = element_text(vjust = +3),
                                     axis.title.x = element_text(vjust = -0.75),
                                     line = element_line(size = 0.3),
                                     strip.background = element_rect(colour = "white", fill =
                                                                       "gray80"),
                                     strip.text = element_text(size = strip))

col <- c("#d38685") 