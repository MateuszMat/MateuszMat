install.packages("C50")
install.packages("gmodels")
library (C50)

absent<- read.csv("data/Absenteeism.csv", stringsAsFactors = F)
absent$id <- NULL

absent$Social.drinker <- factor(absent$Social.drinker, levels = c("0", "1"),
                                   labels = c("No", "Yes"))

summary(absent)

set.seed(1)
absent_rand <- absent[order(runif(100)), ]

absent_train <- absent_rand[1:80,]
absent_test <- absent_rand[81:100,]

prop.table(table(absent_train$Social.drinker))
prop.table(table(absent_test$Social.drinker))

model <- C5.0(Social.drinker ~ ., data = absent_train)

summary(model)
model

prediction <- predict(model, absent_test)

library(gmodels)
CrossTable(prediction, absent_test$Social.drinker,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted drinker', 'actual drinker'))

error_cost <- matrix(c(1, 4, 1, 1), nrow = 2)
error_cost

model <- C5.0(Social.drinker ~ ., data = absent_train, costs=error_cost)

prediction <- predict(model, absent_test)

plot(model)
CrossTable(prediction, absent_test$Social.drinker,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted drinker', 'actual drinker'))