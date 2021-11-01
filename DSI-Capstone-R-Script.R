
# Loading data
library(readr)
ospi <- read_csv("~/GitHub/Misk-DSI-CS-OSPI-Yousif Alsinan/online_shoppers_intention.csv")

#Loading libraries
library(dplyr)
library(explore)
library(ggplot2)
library(tidyverse)
library(caret)

# Getting familiar with the data
ospi %>%
  explore_all()

ncol(ospi)

nrow(ospi)

head(ospi)

str(ospi)

summary(ospi)

summary(ospi$Revenue)

ospi <- ospi %>%
  mutate(RevenueBi = ifelse(Revenue == "FALSE",0,1))

hist(ospi$RevenueBi)

summary(ospi$RevenueBi)

colSums(is.na(ospi))

colSums(ospi == "")

# Exploratory Data Analysis

# 1. Numrical

summary(ospi$Administrative)

ospi %>% 
  ggplot() +
  aes(x = Administrative) +
  geom_bar() +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$Administrative_Duration)

ospi %>% 
  ggplot() +
  aes(x = Administrative_Duration) +
  geom_histogram(bins = 50) +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$Informational)

ospi %>% 
  ggplot() +
  aes(x = Informational) +
  geom_bar() +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$Informational_Duration)

ospi %>% 
  ggplot() +
  aes(x = Informational_Duration) +
  geom_histogram(bins = 50) +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$ProductRelated)

ospi %>% 
  ggplot() +
  aes(x = ProductRelated) +
  geom_bar() +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$ProductRelated_Duration)

ospi %>% 
  ggplot() +
  aes(x = ProductRelated_Duration) +
  geom_histogram(bins = 100) +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$BounceRates)

ospi %>% 
  ggplot() +
  aes(x = BounceRates) +
  geom_histogram(bins = 100) +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$ExitRates)

ospi %>% 
  ggplot() +
  aes(x = ExitRates) +
  geom_histogram(bins = 100) +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$PageValues)

ospi %>% 
  ggplot() +
  aes(x = PageValues) +
  geom_histogram(bins = 50) +
  facet_grid(Revenue ~ .,
    scales = "free_y")

summary(ospi$SpecialDay)

ospi %>% 
  ggplot() +
  aes(x = SpecialDay) +
  geom_bar() +
  facet_grid(Revenue ~ .,
    scales = "free_y")

# 2. Categorical

ospi %>% 
  ggplot() +
  aes(x = Month, Revenue = ..count../nrow(ospi), fill = Revenue) +
  geom_bar() + 
  ylab("relative frequency")

month_table <- table(ospi$Month, ospi$Revenue)
month_tab <- as.data.frame(prop.table(month_table, 2))
colnames(month_tab) <-  c("Month", "Revenue", "perc")

ggplot(data = month_tab, aes(x = Month, y = perc, fill = Revenue)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) + 
  xlab("Month")+
  ylab("Percent")

