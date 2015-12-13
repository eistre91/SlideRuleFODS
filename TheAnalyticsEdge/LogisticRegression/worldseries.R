mb <- read.csv("baseball.csv")
str(mb)

table(mb$Year)
length(table(mb$Year))

mb <- subset(mb, Playoffs == 1)
str(mb)
table(mb$Year)
table(table(mb$Year))

playofftable <- table(mb$Year)
names(playofftable)
playofftable[c("1990", "2001")]

mb$NumCompetitors <- playofftable[as.character(mb$Year)]
dim(subset(mb, NumCompetitors == 8))

mb$WorldSeries <- as.numeric(mb$RankPlayoffs == 1)
dim(subset(mb, WorldSeries == 0))
table(mb$WorldSeries)

mod1 <- glm(WorldSeries ~ Year, data = mb, family = binomial)
mod2 <- glm(WorldSeries ~ RS, data = mb, family = binomial)
mod3 <- glm(WorldSeries ~ RA, data = mb, family = binomial)
mod4 <- glm(WorldSeries ~ W, data = mb, family = binomial)

mod5 <- glm(WorldSeries ~ OBP, data = mb, family = binomial)
mod6 <- glm(WorldSeries ~ SLG, data = mb, family = binomial)
mod7 <- glm(WorldSeries ~ BA, data = mb, family = binomial)
mod8 <- glm(WorldSeries ~ RankSeason, data = mb, family = binomial)

mod9 <- glm(WorldSeries ~ OOBP, data = mb, family = binomial)
moda <- glm(WorldSeries ~ OSLG, data = mb, family = binomial)
modb <- glm(WorldSeries ~ NumCompetitors, data = mb, family = binomial)
modc <- glm(WorldSeries ~ League, data = mb, family = binomial)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

summary(mod5)
summary(mod6)
summary(mod7)
summary(mod8)

summary(mod9)
summary(moda)
summary(modb)
summary(modc)

model <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = mb, family = binomial)
summary(model)

cor(mb$Year, mb$RA)
cor(mb$Year, mb$RankSeason)
cor(mb$Year, mb$NumCompetitors)
cor(mb$RA, mb$RankSeason)
cor(mb$RA, mb$NumCompetitors)
cor(mb$RankSeason, mb$NumCompetitors)

cor(mb[c("Year", "RA", "RankSeason", "NumCompetitors")])

#mod1 ~ Year
#mod3 ~ RA
#mod8 ~ RankSeason
#modb ~ NumCompetitors

mod_yr <- glm(WorldSeries ~ Year + RA, data = mb, family = binomial)
mod_ys <- glm(WorldSeries ~ Year + RankSeason, data = mb, family = binomial)
mod_yc <- glm(WorldSeries ~ Year + NumCompetitors, data = mb, family = binomial)

mod_sr <- glm(WorldSeries ~ RankSeason + RA, data = mb, family = binomial)
mod_cr <- glm(WorldSeries ~ NumCompetitors + RA, data = mb, family = binomial)
mod_sc <- glm(WorldSeries ~ RankSeason + NumCompetitors, data = mb, family = binomial)

summary(mod1)
summary(mod3)
summary(mod8)
summary(modb)

summary(mod_yr)
summary(mod_ys)
summary(mod_yc)
summary(mod_sr)
summary(mod_cr)
summary(mod_sc)
