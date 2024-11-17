getwd()
setwd("D:/ANDAT")
#2
#READ YOUR DATA
epworthscore = read.csv("depression_anxiety_data.csv")
epworthscore
#CHECK THE PACKAGING
nrow(epworthscore)
ncol(epworthscore)
str(epworthscore)
#LOOK AT THE TOP AND THE BOTTOM OF YOUR DATA
head(epworthscore$depression_diagnosis)
head(epworthscore$epworth_score)
tail(epworthscore$depression_diagnosis)
tail(epworthscore$epworth_score)
#ABC: ALWAYS BE CHECKING YOUR "n"
head(table(epworthscore$epworth_score))
head(table(epworthscore$depression_diagnosis))
install.packages("dplyr")
library(dplyr)
data <- epworthscore %>%
  filter(!is.na(depression_diagnosis), !is.na(epworth_score))
head(data$depression_diagnosis)
head(data$epworth_score)
#VALIDATE WITH AT LEAST ONE EXTERNAL DATA SOURCE
summary(data$epworth_score)
quantile(data$epworth_score)
#MAKE A PLOT
library(ggplot2)
ggplot(data, aes(x = depression_diagnosis, y = epworth_score)) +
  geom_boxplot() +
  labs(title = "Boxplot of Depression Diagnosis by Epworth Score",
       x = "Depression Diagnosis",
       y = "Epworth Score") +
  theme_minimal()
#TRY THE EASY SOLUTION FIRST
data$kantuk <- ifelse(data$epworth_score >= 15, "tinggi", "rendah")
proporsi <- table(data$kantuk, data$depression_diagnosis) %>%
  prop.table(margin = 1)
proporsi
barplot(
  proporsi[, "TRUE"],
  names.arg = c("Rendah", "Tinggi"),
  main = "Proporsi Diagnosis Depresi Berdasarkan Tingkat Kantuk",
  xlab = "Kategori Tingkat Kantuk",
  ylab = "Proporsi Diagnosis Depresi",
  col = c("skyblue", "tomato")
)
#FOLLOW UP
cor(data$depression_diagnosis,data$epworth_score)
#3
#MODEL AS EKSPECTATIONS
model=lm(data$depression_diagnosis~data$epworth_score)
summary(model)
#HISTOGRAM
hist <- ggplot(data, aes(x = epworth_score, fill = depression_diagnosis)) +
  geom_histogram(aes(y = ..density..), position = "identity", binwidth = 1, alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(data$epworth_score, na.rm = TRUE), 
                                         sd = sd(data$epworth_score, na.rm = TRUE)), 
                color = "red", size = 1) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink")) +
  labs(title = "Distribusi Epworth Score dengan Kurva Normal dan Diagnosis Depresi",
       x = "epworth score",
       y = "Kepadatan",
       fill = "Diagnosis Depresi") +
  theme_minimal()
hist
