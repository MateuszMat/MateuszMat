install.packages("gmodels")
library(gmodels)
library(class)

absent<- read.csv("data/Absenteeism.csv", stringsAsFactors = F)
absent$id <- NULL

table(absent$Social.drinker)
absent$Social.drinker <- factor(absent$Social.drinker, levels = c("0", "1"),
                              labels = c("No", "Yes"))
table(absent$Social.drinker)
class(absent)
summary(absent[c("Age", "Height", "Weight", "Son", "Pet", "Seasons")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

absent_n <- as.data.frame(lapply(absent[1:5], normalize))

summary(absent_n)

absent_train <- absent_n[1:70,]
absent_test <- absent_n[71:80,]

absent_train_labels <- absent[1:70,6]
absent_test_labels <- absent[71:80,6]

predictions <- knn(train = absent_train, test = absent_test, 
                   cl = absent_train_labels, k=3)

CrossTable(predictions, absent_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

plot(absent_n)
plot(predictions)
plot(c(absent_n$Age,absent_n$Height,absent_n$Weight))
