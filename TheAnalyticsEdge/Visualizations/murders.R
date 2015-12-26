murders <- read.csv("murders.csv")

states_map <- map_data("state")
str(states_map)

ggplot(states_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black")

murders$region <- tolower(murders$State)

murder_map <- merge(states_map, murders, by="region")

ggplot(murder_map, aes(x=long, y=lat, group=group, fill=Murders)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide="legend")

ggplot(murder_map, aes(x=long, y=lat, group=group, fill=Population)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide="legend")

murder_map$murder_rate <- murder_map$Murders/murder_map$Population*100000

ggplot(murder_map, aes(x=long, y=lat, group=group, fill=murder_rate)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0,10))

ggplot(murder_map, aes(x=long, y=lat, group=group, fill=GunOwnership)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide="legend")

theme_invisible <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(colour = NA,size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       theme_text(colour = NA,size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        theme_segment(colour = NA, size = 0.2),
    axis.title.x =      theme_text(colour = NA,size = base_size, vjust = 1),
    axis.title.y =      theme_text(colour = NA,size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),
    
    panel.background =  theme_rect(fill = NA, colour = NA), 
    panel.border =      theme_rect(fill = NA, colour=NA), 
    panel.grid.major =  theme_line(colour = NA, size = 0.2),
    panel.grid.minor =  theme_line(colour = NA, size = 0.5),
    panel.margin =      unit(0.25, "lines"),
    
    strip.background =  theme_rect(fill = NA, colour = NA), 
    strip.text.x =      theme_text(colour = NA,size = base_size * 0.8),
    strip.text.y =      theme_text(colour = NA,size = base_size * 0.8, angle = -90),
    
    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(colour = NA,size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

theme_set(theme_gray())

data <- as.data.frame(matrix(data=c("Don't")))
colnames(data) <- c("DoNot")
pie <- ggplot(data, aes(x = DoNot)) + 
  geom_bar(aes(fill=DoNot),width=1)
pie <- pie + coord_polar(theta = "y") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line =         element_blank(),
        axis.text.x =       element_blank(),
        axis.text.y =       element_blank(),
        axis.ticks =        element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  ggtitle("How Often Is it Appropriate to Use a Pie Chart") +
  scale_fill_manual(values="#D55E00")

pdf("piecharts.pdf")
png("piecharts.png", width=1000, height=1000, antialias="cleartype")
print(pie)
dev.off()

install.packages("Cairo")
library(Cairo)
Cairo(file="piecharts.png", 
      type="png",
      units="in", 
      width=6, 
      height=6, 
      pointsize=12, 
      dpi=72)
print(pie)
dev.off()

d<-data.frame(x=1:5, y1=1:5, y2=2:6)

ggplot(d, aes(x)) + 
  geom_line(aes(y=y1, colour="1")) + 
  geom_line(aes(y=y2, colour="2")) +
  scale_colour_manual(values=c("red", "blue"))