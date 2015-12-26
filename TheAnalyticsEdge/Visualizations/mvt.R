mvt <- read.csv("mvt.csv", stringsAsFactors=FALSE)
str(mvt)

mvt$Date <- strptime(mvt$Date, "%m/%d/%y %H:%M")
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour

weekday_counts <- as.data.frame(table(mvt$Weekday))

library(ggplot2)

ggplot(aes(x=Var1, y=Freq), data=weekday_counts) +
  geom_line(aes(group=1))

weekday_counts$Var1 <- factor(weekday_counts$Var1, ordered=TRUE,
                              levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                       "Thursday", "Friday", "Saturday"))

ggplot(aes(x=Var1, y=Freq), data=weekday_counts) +
  geom_line(aes(group=1)) +
  xlab("Day of the Week") +
  ylab("Total Motor Vehicle Thefts")

ggplot(aes(x=Var1, y=Freq), data=weekday_counts) +
  geom_line(aes(group=1), alpha=0.3) +
  xlab("Day of the Week") +
  ylab("Total Motor Vehicle Thefts")

table(mvt$Weekday, mvt$Hour)

day_hour_counts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
str(day_hour_counts)

day_hour_counts$Hour <- as.numeric(as.character(day_hour_counts$Var2))

ggplot(aes(x = Hour, y = Freq), data=day_hour_counts) +
  geom_line(aes(group=Var1, color=Var1), size=2) 

day_hour_counts$Var1 <- factor(day_hour_counts$Var1, ordered=TRUE,
                               c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                 "Saturday", "Sunday"))

ggplot(aes(x = Hour, y = Var1), data=day_hour_counts) +
  geom_tile(aes(fill=Freq)) +
  scale_fill_gradient(name="Total MV Thefts", low="white", high="red") +
  theme(axis.title.y = element_blank())

install.packages("maps")
install.packages("ggmap")

library(maps)
library(ggplot2)
library(ggmap)

chicago <- get_map(location="chicago", zoom=11)
ggmap(chicago)
ggmap(chicago) + 
  geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))

latloncounts <- as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
latloncounts$Long <- as.numeric(as.character(latloncounts$Var1))
latloncounts$Lat <- as.numeric(as.character(latloncounts$Var2))

ggmap(chicago) +
  geom_point(data=latloncounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) +
  scale_color_gradient(low="yellow", high="red")

ggmap(chicago) +
  geom_tile(data=latloncounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")

latloncounts2 <- subset(latloncounts, Freq > 0)

ggmap(chicago) +
  geom_tile(data=latloncounts2, aes(x=Long, y=Lat, alpha=Freq), fill="red")

dim(latloncounts) - dim(latloncounts2)

