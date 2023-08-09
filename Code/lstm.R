library(tidyverse)
library(TSLSTM)

df<-read.csv("data/FWash_DJF.csv")



Y<-df$bloom_doy


# # Set the mean and standard deviation
# mean_val <- mean(Y)
# sd_val <- sd(Y)
# 
# # Generate 1000 random data points
# random_data <- rnorm(n = 1000, mean = mean_val, sd = sd_val)
# Y<-c(random_data,Y)
Y_new = (Y - min(Y)) / (max(Y) - min(Y))



x1<-df$temp
# mean_val <- mean(x1)
# sd_val <- sd(x1)
# 
# # Generate 1000 random data points
# random_data <- rnorm(n = 1000, mean = mean_val, sd = sd_val)
# x1<-c(random_data,x1)
x1_new = (x1 - min(x1)) / (max(x1) - min(x1))

x2<-df$windspeed
# mean_val <- mean(x2)
# sd_val <- sd(x2)
# 
# # Generate 1000 random data points
# random_data <- rnorm(n = 1000, mean = mean_val, sd = sd_val)
# x2<-c(random_data,x2)

x2_new = (x2 - min(x2)) / (max(x2) - min(x2))



x3<-df$dew
# # Set the mean and standard deviation
# mean_val <- mean(x3)
# sd_val <- sd(x3)
# 
# # Generate 1000 random data points
# random_data <- rnorm(n = 1000, mean = mean_val, sd = sd_val)
# x3<-c(random_data,x3)
x3_new = (x3 - min(x3)) / (max(x3) - min(x3))



xi<-cbind(x1_new,x2_new,x3_new)
TSLSTM<-ts.lstm(ts=Y_new,
                tsLag=1,
                xreg = xi,
                xregLag = 1,
                LSTMUnits=200,
                DropoutRate = 0.1,
                Epochs = 50,
                CompLoss = "mse",
                CompMetrics = "mae",
                ActivationFn = "tanh",
                SplitRatio = 0.8,
                ValidationSplit = 0.2)



 #Return function
trainFittedValue <- TSLSTM$TrainFittedValue
testPredictedValue <- TSLSTM$TestPredictedValue
accuracyTable <- TSLSTM$AccuracyTable


Result<-tail(df,8)


Result$S_pred<-testPredictedValue

Result$Prediction= (Result$S_pred * ( max(Y) - min(Y) ) + min(Y))%/%1

library(ggplot2)

ggplot(data = Result) +
  geom_line(aes(x = year, y = bloom_doy, color = "Actual"), show.legend = T) +
  geom_line(aes(x = year, y = Prediction, color = "Predicted"), show.legend = T) +
  labs(x = 'Year', y = 'Blooming Day') +
  ggtitle('Cherry Blossom Prediction') +
  theme_minimal() +
  scale_color_manual(name = "Lines",
                     values = c("Actual" = "blue", "Predicted" = "red"),
                     labels = c("Actual", "Predicted")) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 16))))+
  theme(panel.grid = element_blank())
