options(scipen=999)

setwd("/R")

data <- read.csv("memory.csv")
data

str(data)
head(data)

# ANOVA

memory.aov <- with(data,
                   aov(score ~ lang + cond + trial + (lang * cond) + (lang * trial) + (cond * trial)))

confint(memory.aov)

memory.aov2 <- with(data,
                   aov(score ~ lang + cond))

pairwise.t.test(data$score, data$lang, p.ajd = "none")
pairwise.t.test(data$score, data$cond, p.ajd = "none")

summary(memory.aov)
summary(memory.aov2)

data.t1 <- read.csv("memory.t1.csv")
data.t1
memory.aov3 <- with(data.t1,
                    aov(score ~ lang + cond))
summary(memory.aov3)

data.t2 <- read.csv("memory.t2.csv")
data.t2
memory.aov4 <- with(data.t2,
                    aov(score ~ lang + cond))
summary(memory.aov4)

data.t3 <- read.csv("memory.t3.csv")
data.t3
memory.aov5 <- with(data.t3,
                    aov(score ~ lang + cond))
summary(memory.aov5)

data.t4 <- read.csv("memory.t4.csv")
data.t4
memory.aov6 <- with(data.t4,
                    aov(score ~ lang + cond))
summary(memory.aov6)

data.t5 <- read.csv("memory.t5.csv")
data.t5
memory.aov7 <- with(data.t5,
                    aov(score ~ lang + cond))
summary(memory.aov5)

# t-test for all trials in KR cnd
data.t1.KR <- read.csv("memory.t1.KR.csv")
data.t1.KR

head(data.t1.KR)

art <- subset(data.t1.KR, cond == "art", score,
                drop = TRUE)
tap <- subset(data.t1.KR, cond == "tap", score,
              drop = TRUE)

t.test(art, tap, paired = TRUE)

data.t2.KR <- read.csv("memory.t2.KR.csv")
data.t3.KR <- read.csv("memory.t3.KR.csv")
data.t4.KR <- read.csv("memory.t4.KR.csv")
data.t5.KR <- read.csv("memory.t5.KR.csv")
