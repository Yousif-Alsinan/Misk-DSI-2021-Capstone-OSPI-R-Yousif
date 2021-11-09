
# Loading data
library(readr)
ospi <- read_csv("~/GitHub/Misk-DSI-CS-OSPI-Yousif Alsinan/data/online_shoppers_intention.csv")

#Loading libraries
library(dplyr)
library(explore)
library(ggplot2)
library(tidyverse)
library(caret)
library(plyr)
library(cluster)
library(rpart)

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

ospi$OperatingSystems <- factor(ospi$OperatingSystems, order = TRUE, levels = c(6,3,7,1,5,2,4,8))
ospi$Browser <- factor(ospi$Browser, order = TRUE, levels = c(9,3,6,7,1,2,8,11,4,5,10,13,12))
ospi$Region <- factor(ospi$Region, order = TRUE, levels = c(8,6,3,4,7,1,5,2,9))
ospi$TrafficType <- factor(ospi$TrafficType, order = TRUE, levels = c(12,15,17,18,13,19,3,9,1,6,4,14,11,10,5,2,20,8,7,16))

ospi$Month <- factor(ospi$Month, order = TRUE, levels =c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'))
ospi$Month_Numeric <-mapvalues(ospi$Month, from = c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'), to = c(1,2,3,4,5,6,7,8,9,10))


ospi$VisitorType <- factor(ospi$VisitorType, order = TRUE, levels = c('Returning_Visitor', 'Other', 'New_Visitor'))
ospi$VisitorType_Numeric <-mapvalues(ospi$VisitorType, from = c("Returning_Visitor", "Other", "New_Visitor"), to = c(1,2,3))

ospi <- ospi %>%
  mutate(Weekend_binary = ifelse(Weekend == "FALSE",0,1))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

ospi_norm <- ospi

ospi_norm$Administrative <- normalize(ospi$Administrative)
ospi_norm$Administrative_Duration <- normalize(ospi$Administrative_Duration)
ospi_norm$Informational <- normalize(ospi$Informational_Duration)
ospi_norm$Informational_Duration <- normalize(ospi$Administrative)
ospi_norm$ProductRelated <- normalize(ospi$ProductRelated)
ospi_norm$ProductRelated_Duration <- normalize(ospi$ProductRelated_Duration)
ospi_norm$BounceRates <- normalize(ospi$BounceRates)
ospi_norm$ExitRates <- normalize(ospi$ExitRates)
ospi_norm$PageValues <- normalize(ospi$PageValues)
ospi_norm$SpecialDay <- normalize(ospi$SpecialDay)

ospi_clust <- ospi_norm[-c(11,16:19)]

ospi_class <- ospi[-c(19:22)]

set.seed(123)
training <- createDataPartition(ospi_class$Revenue, p = 0.8, list=FALSE)

train_data <- ospi_class[training,]
test_data <- ospi_class[-training,]

# Clustering

summary(ospi_clust)

str(ospi_clust)

k_mean_clust <- kmeans(ospi_clust, centers = 2, iter.max = 100)

k_mean_clust$size

k_mean_clust$centers

k_mean_clust$betweenss

k_mean_clust$totss

k_mean_clust$betweenss / k_mean_clust$totss

t1 <- table(k_mean_clust$cluster, ospi_norm$Revenue)
t1

pca_clust <- prcomp(ospi_clust[c(1:10)], scale. = TRUE)
plot(pca_clust, main = "Principal Components")

shopper_components <- as.data.frame(pca_clust$x)

head(shopper_components[1:2])

plot(PC1~PC2, data=shopper_components,
     cex = .1, lty = "solid")
text(PC1~PC2, data=shopper_components, 
     labels=rownames(ospi_clust[c(1:10)]),
     cex=.8)

plot(PC1~PC2, data=shopper_components, 
     main= "OSPI: PC1 vs PC2 - K-Means Clusters",
     cex = .1, lty = "solid", col=k_mean_clust$cluster)
text(PC1~PC2, data=shopper_components, 
     labels=rownames(ospi_clust[c(1:10)]),
     cex=.8, col=k_mean_clust$cluster)

presicion_kmeans<- t1[1,1]/(sum(t1[1,]))
recall_kmeans<- t1[1,1]/(sum(t1[,1]))

presicion_kmeans
recall_kmeans

F1_kmeans<- 2*presicion_kmeans*recall_kmeans/(presicion_kmeans+recall_kmeans)
F1_kmeans

k_med_clust <- pam(x = ospi_clust, k = 2)

k_med_clust$id.med

k_med_clust$mediods

k_med_clust$objective

k_med_clust$clusinfo

t1b <- table(k_med_clust$clustering, ospi_norm$Revenue)
t1b

plot(PC1~PC2, data=shopper_components, 
     main= "OSPI: PC1 vs PC2 - K-Medoids Clusters",
     cex = .1, lty = "solid", col=k_med_clust$clustering)
text(PC1~PC2, data=shopper_components, 
     labels=rownames(ospi_clust[c(1:10)]),
     cex=.8, col=k_med_clust$clustering)

presicion_kmed<- t1b[1,1]/(sum(t1b[1,]))
recall_kmed<- t1b[1,1]/(sum(t1b[,1]))

presicion_kmed
recall_kmed

F1_kmed<- 2*presicion_kmed*recall_kmed/(presicion_kmed+recall_kmed)
F1_kmed

# Classification

summary(ospi_class)

str(ospi_class)

model_dt <- rpart(Revenue ~ . , data = train_data, method="class")
rpart.plot::rpart.plot(model_dt)

pred.train.dt <- predict(model_dt,test_data,type = "class")
mean(pred.train.dt==test_data$Revenue)

t2<-table(pred.train.dt,test_data$Revenue)
t2

presicion_dt<- t2[1,1]/(sum(t2[1,]))
recall_dt<- t2[1,1]/(sum(t2[,1]))

presicion_dt
recall_dt

F1_dt<- 2*presicion_dt*recall_dt/(presicion_dt+recall_dt)
F1_dt


