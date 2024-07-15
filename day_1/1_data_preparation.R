# JLU Giessen Mixed Models Workshop

# Part 1: R Basics for Data Preparation

# variables
x <- 5

y <- c(1, 3, "psychology", "math", exp(1))

y[2]

z <- list(x, y)

z[[2]][2]

# basic functions mean, sd, se (function), 

u <- rnorm(100, 0, 1)

mean(u)
sd(u)

se <- function(vector){
  sd(vector)/sqrt(length(vector))
}

se(u)

# Installing packages
install.packages("dplyr")

# Loading packages
library(dplyr)

# Setting working directory
getwd()

setwd("~/Documents/workshop-giessen")

# Importing data
norming <- read.csv("datasets/Norming.csv")

# Check the variables
str(norming)


### Select only necessary data
norming2 <- norming[,2:59]

norming3 <- norming2 %>% select(1:5)

norming4 <- norming %>% select(-1)

# Rename variables
norming <-  norming %>% rename(gender = Cinsiyetiniz.,
                               age = Yaşınız.,
                               education = Eğitim.Düzeyiniz.)


# Recode
norming$gender <- norming$gender %>% recode(Erkek = "male", Kadın = "female")

norming$education <- norming$education %>% recode(Lise = "High School",
                                                  Lisans = "Bachelor",
                                                  `Yüksek Lisans` = "Master")


## Filtering

### Method 1: tidyverse-way
norming_age <- norming %>% filter(age < 25)

### Method 2: Classical R-way
norming_age2 <- norming[norming$age < 25,]


# Summarize the data (Descriptive statistics)
norming %>% summarize(mean_age = mean(age),
                      sd_age = sd(age))

norming$education <- as.factor(norming$education)
norming$education <- factor(norming$education, levels = c("High School", "Bachelor", "Master"))

# Grouping
norming %>% group_by(education) %>% summarize(Count = n())

# Wide-to-Long format
norming <- norming[, -c(1, 2)]


install.packages("tidyr")
library(tidyr)

norming_long <- norming %>% pivot_longer(!c(age, gender, education), names_to = "sentence", values_to = "rating")

norming_long %>% group_by(education) %>% summarize(mean_rating = mean(rating),
                                                   sd_rating = sd(rating))


# Save the data
save(norming_long, file = "datasets/norming_long.Rda")


# t-test
dat <- read.csv("~/Documents/workshop-giessen/datasets/grodnergibson05data.txt", sep="")

hist(dat$rawRT)

dat <- dat %>% mutate(logRT = log(rawRT))

dat$logRT <- log(dat$rawRT)


hist(dat$logRT)

dagg <- aggregate(logRT ~ subject + condition, data = dat, FUN = mean)

dagg$condition <- as.factor(dagg$condition)

contrasts(dagg$condition) <- 0.5 * contr.sum(2)

ttest <- t.test(logRT ~ condition, dagg, paired = TRUE)

ttest$statistic
ttest$p.value
























