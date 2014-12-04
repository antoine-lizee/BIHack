## Script to create the explanotary graph in the remarks section
#
# Copytight Antoine Lizee antoine.lizee@gmail.com 2014/11


# Definition of the data --------------------------------------------------

df <- data.frame(dT = c(3,4,3,10),
                 dY = c(3,2,5,2),
                 label = c("Prototype ready of the main screen & graph", 
                           "Adding persistance with sqlite3", 
                           "Creating the two other panels,\nwrapping things up for v1", 
                           "Random tinkering around UX adjustements, flow management and reliability. \nCode cleaning")
)

df <- within(df, {
  x1 <- c(0, cumsum(df$dT)[-nrow(df)])
  x2 <- cumsum(df$dT)
  y1 <- c(0, cumsum(df$dY)[-nrow(df)])
  y2 <- cumsum(df$dY)
})

df$yield <- df$dY / df$dT

df$hjust <- c(-0.03, -0.06, 0.5, 0.5)
df$xtext <- c(3, 7, 5, 9)

df$hjust <- c(1,1,0,0)
df$xtext <- c(19.8, 19.8, 0.2, 0.2)

axisArrow <- grid::arrow(length = grid::unit(0.15,"inches"), angle = 25)

gg <- ggplot(df) + theme_bw(base_size = 18, base_family = "xkcd") +
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), size = 2, lineend = "round") +
  #   geom_rect(aes(xmin = x1, xmax = x2, fill = yield), ymin = 0, ymax = max(df$y2), color = NA, alpha = 0.3) +
  geom_rect(aes(ymin = y1, ymax = y2, fill = yield), xmin = 0, xmax = max(df$x2), color = NA, alpha = 0.18) +
  geom_segment(aes(x = 0, xend = max(x2), y = y2, yend = y2), linetype = "46", size = 0.6) +
  geom_text(aes(x = xtext, y= y2-0.2, label = label, hjust = hjust), vjust = 1, size = 4.5) +
  guides(fill = NULL) + labs(x = "time spent - hours", y = "result", title = "Development of the HP with -SHINY-") +
  scale_x_continuous(breaks = df$x2) +
  scale_y_continuous(breaks = NULL, limits = c(0,max(df$y2) + 0.5), expand = c(0.01,0)) + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(4, name = "RdYlGn")) +
  annotate("segment", x=0,xend=Inf,y=0,yend=0, arrow=axisArrow, size = 0.6, color = "grey50") + 
  annotate("segment", y=0,yend=Inf,x=0,xend=0, arrow=axisArrow, size = 0.6, color = "grey50") +
  theme(
    panel.border = element_blank(),
    axis.title.x = element_text(hjust = +0.8, vjust = 2.6),# size = rel(1.4)),
    axis.title.y = element_text(hjust = +0.9, vjust = 1.4),# size = rel(1.4)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.8))

print(gg)
ggsave("www/progressGraph.png", width = 10, height = 5, units = "in", dpi = 80)
