library(ggplot2)

intl <- read.csv("intl.csv")
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=PercentOfIntl))

intl <- transform(intl, Region=reorder(Region, -PercentOfIntl))
intl$PercentOfIntl <- intl$PercentOfIntl*100

ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity", fill="dark blue") +
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
  ylab("Percentage of International Students") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1))

library(ggmap)

intlall <- read.csv("intlall.csv", stringsAsFactors=FALSE)
intlall[is.na(intlall)] <- 0

world_map <- map_data("world")
world_map <- merge(world_map, intlall, by.x="region", by.y="Citizenship")

ggplot(world_map, aes(x=long, y=lat, group=group)) +
geom_polygon(fill="white", color="black", aes(fill=Total)) +
  coord_map("mercator")

world_map = world_map[order(world_map$group, world_map$order),]
table(intlall$Citizenship)

intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] <- "China"

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(color="black", aes(fill=Total)) +
  coord_map("mercator")

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(color="black", aes(fill=Total)) +
  coord_map("ortho", orientation=c(20, 30, 0))
