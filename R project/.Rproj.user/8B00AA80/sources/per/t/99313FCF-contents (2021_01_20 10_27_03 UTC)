Cars <- read.csv("data/Cars.csv")
Cars
summary(Cars)
plot(Cars)
abline(Cars)
scatter.smooth(x=Cars$engine.size, y=Cars$price, main="engine.size ~ price")

install.packages('caTools')
library(caTools)
set.seed(123)

split = sample.split(Cars$make, SplitRatio = 0.7)
split

training = subset(Cars, split == TRUE)
testing = subset(Cars, split == FALSE)

model <- lm(engine.size ~ make, data=Cars)

summary(model)
plot(model)

model.lm.coefficients

predictions = predict(Cars, predictions = testing)
predictions
