
library(readr)
ospi <- read_csv("~/GitHub/Misk-DSI-CS-OSPI-Yousif Alsinan/online_shoppers_intention.csv")

library(dplyr)
library(explore)
library(ggplot2)
library(tidyverse)
library(caret)

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
