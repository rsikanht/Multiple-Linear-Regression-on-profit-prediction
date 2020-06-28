# Prepare a prediction model for profit of 50_startups data.
# Do transformations for getting better predictions of profit and
# make a table containing R^2 value for each prepared model.

getwd()
Startup <- read.csv("50_Startups.csv")
View(Startup)
pairs(Startup)

dataset <- subset(Startup, select = c(R.D.Spend, Administration, Marketing.Spend, Profit))
View(dataset)
cor(dataset)
pairs(dataset, upper.panel = panel.smooth, main = "Correla")

plot(density(dataset$R.D.Spend))
plot(density(dataset$Administration))
plot(density(dataset$Marketing.Spend))
plot(density(dataset$Profit))
# All the density plots show the distributions are close to normal

attach(dataset)
boxplot(R.D.Spend, horizontal = T) # No outliers, Normal distribution
boxplot(Administration, horizontal = T) # No outliers, left skewed
boxplot(Marketing.Spend, horizontal = T) # No outliers, Normal distribution
boxplot(Profit, horizontal = T) # No outliers, left skewed

trail_m <- lm(Profit~., data = dataset)
summary(trail_m)

summary(lm(Profit~Administration, data = dataset))

?vif
vif(trail_m)

??avPlots
avPlots(trail_m)

influence.measures(trail_m)
influenceIndexPlot(trail_m)
influencePlot(trail_m)

model1 <- lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend, data = dataset)
summary(model1) # R^2 = 0.9506, P value for administration and marketing spend is > 0.05

summary(lm(Profit~Administration))
summary(lm(Profit~log(Administration)))

model2<- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = dataset[-c(46,47,49,50)])
summary(model2) # R^2 = 0.9507, P value for administration and marketing spend is > 0.05

model3 <- lm(Profit~R.D.Spend+sqrt(Administration)+sqrt(Marketing.Spend), data = dataset[-c(46,47,49,50)])
summary(model3) # R^2 = 0.9505, P value for administration and marketing spend is > 0.05

Final_model <- lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend, data = dataset[-c(46,47,49,50)])
summary(Final_model)
# R^2 = 0.9506


sum(Final_model$residuals)
# 2.614797e-11 approx 0.

predict(Final_model)
confint(Final_model,level = 0.95)
