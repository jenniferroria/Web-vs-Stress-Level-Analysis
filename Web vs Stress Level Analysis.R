getwd()
setwd("/Users/jennsmacbook/Documents/HW 9")
data <- read.csv('Website on Stress Level.csv')
data


head(data$Hours.Spend.on.Website)
head(data$Stress.Test.Points..The.more.the.worse.)

summary(data$Hours.Spend.on.Website)
summary(data$Stress.Test.Points..The.more.the.worse.)

boxplot(data$Hours.Spend.on.Website, main="Hours Spend", sub=paste("Outlier rows:", boxplot.stats(data$Hours.Spend.on.Website)$out))
stress <- data[-c(214),]
boxplot(data[-c(214),]$Hours.Spend.on.Website, main="Hours Spend", sub=paste("Outlier rows:", boxplot.stats(data$Hours.Spend.on.Website)$out))
boxplot(data$Stress.Test.Points..The.more.the.worse.)

stress <- data[-c(214),]
plot(density(stress$Hours.Spend.on.Website))
stress <- data[-c(214),]
plot(density(stress$Stress.Test.Points..The.more.the.worse.))

stress <- data[-c(214),]
scatter.smooth(x=stress$Hours.Spend.on.Website, y=stress$Stress.Test.Points..The.more.the.worse., main="HoursSpendonWebsiteVs.StressTestPoints")

install.packages("caTools")
library("caTools")
split=sample.split(stress$Stress.Test.Points..The.more.the.worse., SplitRatio=1/4)
TestData=subset(stress,split==TRUE)
TrainingData=subset(stress,split==FALSE)
TrainingData

linearMod <- lm (stress$Hours.Spend.on.Website ~ stress$Stress.Test.Points..The.more.the.worse., stress=train)
print(linearMod)

summary(linearMod)

predicted <- predict(linearMod, TestData)
print(predicted)
print(TestData)

par(mfrow= c(2,2))
plot(linearMod)
