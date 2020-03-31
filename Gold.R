# The fundamental idea for time series analysis is to decompose the original
# time series (sales, stock market trends, etc.) into several independent components.
# Typically, business time series are divided into the following four components:
#
#   Trend –  overall direction of the series i.e. upwards, downwards etc.
#   Seasonality – monthly or quarterly patterns
#   Cycle –  long-term business cycles
#   Irregular remainder – random noise left after extraction of all the components

# these components become together to produce meaningful insights from time series.

# Gold ounce prices monthly history data from 31.12.1969 till 31.01.2020 and
# daily history from 29.12.1978 to 28.02.2020
# can be obtained from this link  https://www.gold.org/download/file/8369/Prices.xlsx
# (the webpage may require membership to download the dataset)
setwd("E:/git projects/gold")


library(tidyverse)
library(readxl)
library(astsa)
library(forecast)
library(zoo)

df_m <- read_excel("prices.xlsx", skip = 8, sheet = "Monthly_Full")
df_m <- df[1:2]
colnames(df_m) <- c("dates", "Ounce_Price")

############################################################################
##########################linear regression with monthly data ##############
exp.model <- lm(log(Ounce_Price)~dates,data = df_m) 
exp.model.df <- data.frame(x=df_m$dates,
                           y=exp(fitted(exp.model)))

ggplot(df_m, aes(x = dates, y = Ounce_Price)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'quadratic'), se= FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,3), aes(colour = 'cubic'), se = FALSE)+
  stat_smooth(data=exp.model.df, method = 'loess',aes(x,y,colour = 'exponential'), se = FALSE) 


df <- read_excel("prices.xlsx", skip = 8, sheet = "Daily")
df <- df[1:2]
colnames(df) <- c("dates", "Ounce_Price")

########################Data Regularization #############################
# Since the dataset is daily data, there were missing dates, regarding to 
# financial holidays. So I created rows for all days and fill them with the
# last previous not null values. 


df1 <- df %>% 
  complete(dates = seq(dates[1], as.POSIXct(strptime("2020-02-29", "%Y-%m-%d")), by = "1 day"),
           fill = list(Ounce_Price = NA))

df1 <- na.locf(df1)

df1$dates <- as.Date(df1$dates)



############################################################################
##########################linear regression################################

exp.model <- lm(log(Ounce_Price)~dates,data = df1) 
exp.model.df <- data.frame(x=df1$dates,
                           y=exp(fitted(exp.model)))

ggplot(df1, aes(x = dates, y = Ounce_Price)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'quadratic'), se= FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,3), aes(colour = 'cubic'), se = FALSE)+
  stat_smooth(method = 'lm', formula = y ~ poly(x,4), aes(colour = 'quartic'), se = FALSE)+
  stat_smooth(data=exp.model.df, method = 'loess',aes(x,y,colour = 'exponential'), se = FALSE) 

ggplot(df1, aes(x = dates, y = Ounce_Price)) + geom_point() +
  stat_smooth(method = 'lm', formula = y ~ poly(x,5), aes(colour = 'quintic'), se = FALSE)+
  stat_smooth(method = 'lm', formula = y ~ poly(x,6), aes(colour = 'sextic'), se = FALSE)+
  stat_smooth(method = 'lm', formula = y ~ poly(x,7), aes(colour = 'septic'), se = FALSE)+
  stat_smooth(method = 'lm', formula = y ~ poly(x,8), aes(colour = 'octic'), se = FALSE)

ggplot(df1, aes(x = dates, y = Ounce_Price)) + geom_point() +
  stat_smooth(method = 'lm', formula = y ~ poly(x,8), aes(colour = 'octic'), se = FALSE)+
  stat_smooth(method = 'lm', formula = y ~ poly(x,9), aes(colour = 'nonic'), se = FALSE)


model_octic <- lm(data = df1, Ounce_Price~poly(dates,8))
summary(model_octic)
lo <- loess(resid(model_octic) ~ fitted(model_octic), degree = 1, span=0.8)
plot(fitted(model_octic),resid(model_octic))
lines(fitted(model_octic),predict(lo), col='red', lwd=2)
abline(a=0, b=0, lty=2)

plot(model_octic, which =2)

# Although model coefficients are statistically significant, yet the model assumptions 
# still does not met in terms of Normality and homoscedastisity of variance among residuals


###########################################################################
############ ARIMA - SARIMA - SES - DES - TES ###########################3#
# First it is better to see some visualizations to try to understand if there 
# is a trend or non-stationarity in data

plot(df1, main="Daily Gold Prices by ounce, 12.29.1978 to 02.28.2020",
     ylab = "Price(USD)", xlab = "Date", type="l")
# it seems there is instability in variance and a clear upward trend on data
# so first I will take logarithm to stabilize variance
plot(log(df1$Ounce_Price), main = "Log of Daily Gold Prices by ounce",
     ylab = "Log of Price(USD)", xlab = "Days")
# now I will try to remove seasonality by taking difference of data
plot(diff(df1$Ounce_Price), main = "Differences of Daily Gold Prices by ounce",
     ylab = "Price(USD)", xlab = "Days")
# it helped to remove seasonality as expected. 
# Now I will take log-return of data
plot(diff(log(df1$Ounce_Price)), main = "Log return of Daily Gold Prices by ounce",
     ylab = "Log of Price(USD)", xlab = "Days") 

# I also tried several other approaches like taking seasonal difference etc..
# but there won't be a significant development in data, so I decided to continue
# log return, but the trials can be observed below;

plot(diff(log(df1$Ounce_Price), lag=7), main = "Log return of Weekly Gold Prices by ounce",
     ylab = "Log of Price(USD)", xlab = "Days") 

plot(diff(log(df1$Ounce_Price), lag=30), main = "Log return of Monthly Gold Prices by ounce",
     ylab = "Log of Price(USD)", xlab = "Days") 

plot(diff(log(df1$Ounce_Price),365), main = "non-seasonal and Yearly seasonal
     diff of log return")


plot(diff(sqrt(df1$Ounce_Price)), main = "diff of square root")
# The relative differences are computed using diff(vector) and the vector 
# (except its last element):
# percentage change
plot(diff(df1$Ounce_Price)/df1$Ounce_Price[-length(df1$Ounce_Price)], 
     main = "division of diff of t from t-1 to the t-1")
# (df$Ounce_Price[3] - df$Ounce_Price[2])/df$Ounce_Price[2]
plot(((df1$Ounce_Price/lag(df1$Ounce_Price) - 1) * 100), 
     main = "division of t to t-1 in terms of percentage")

plot((diff(log(df1$Ounce_Price)))/diff(log(lag(df1$Ounce_Price))), 
     main = "division of log return in t to the log return in t-1")

plot(diff(log(df1$Ounce_Price))- mean(diff(log(df1$Ounce_Price))), 
     main = "log return with zero mean") 


#####################################################################

acf(diff(log(df1$Ounce_Price)), main = "ACF of Log return of Daily Gold Prices by ounce",
    ylab = "Log of Price(USD)", xlab = "Days") 

#ø it seems there is a cyclic seasonality in the dataset fro ACF plot. 

pacf(diff(log(df1$Ounce_Price)), main = "PACF of Log return of Daily Gold Prices by ounce",
    ylab = "Log of Price(USD)", xlab = "Days") 

# the cyclic trend can also be observed from PACF plot. 

d=1
D=0

for(p in 0:5){
  for(q in 0:4){
    for(P in 0:6){
      for(Q in 0:5){
        if( p+q+P+Q+d+D <= 6){ # parcimony principle
          model <- arima(x= log(df1$Ounce_Price), order=c(p,d,q), 
                         seasonal = list(order=c(P,D,Q), frequency = 1))
          sse <- sum(resid(model)^2)
          order <- paste(p,d,q,P,D,Q)
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          cat(p,d,q,P,D,Q,1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', 
              pval$p.value,'\n')
        }
      }
    }
  }
}

# 0 1 0 5 0 0 1 AIC= -95853.45  SSE= 1.498727  p-VALUE= 0.3437408
# 2 1 0 3 0 0 1 AIC= -95853.45  SSE= 1.498727  p-VALUE= 0.3436824 
sarima(df1$Ounce_Price, 0,1,0,5,0,0,1)
sarima(df1$Ounce_Price, 2,1,0,3,0,0,1)

modelx <- arima(x=log(df1$Ounce_Price), order = c(0,1,0), 
                seasonal = list(order=c(0,0,5), period=1))
plot(forecast(modelx))
forecast(modelx, 50)

plot(forecast(modelx ,1000),xlim=c(14500,16200),ylim=c(7,8))

modelx <- arima(x=log(df1$Ounce_Price), order = c(2,1,0), 
                seasonal = list(order=c(3,0,0), period=1))
plot(forecast(modelx))
forecast(modelx, 50)

plot(forecast(modelx ,1000),xlim=c(14500,16200),ylim=c(7,8))


summary(forecast(auto.arima(log(df1$Ounce_Price), seasonal = TRUE)))

plot(forecast(auto.arima(log(df1$Ounce_Price), seasonal = TRUE),1000)
     ,xlim=c(14500,16200),ylim=c(7,8))
# ARIMA(0,1,1) with drift 
# 
# Coefficients:
#   ma1  drift
# -0.0489  1e-04
# s.e.   0.0082  1e-04
# 
# sigma^2 estimated as 9.969e-05:  log likelihood=47999.26
# AIC=-95992.53   AICc=-95992.53   BIC=-95969


df2 <- df1[13000:nrow(df1),]


plot(df2, main="Daily Gold Prices by ounce, 01.08.2014 to 02.28.2020",
     ylab = "Price(USD)", xlab = "Date", type="l")
# it seems there is instability in variance and a clear upward trend on data
# so first I will take logarithm to stabilize variance
plot(log(df2$Ounce_Price), main = "Log of Daily Gold Prices by ounce",
     ylab = "Log of Price(USD)", xlab = "Days")
# now I will try to remove trend by taking difference of data
plot(diff(df2$Ounce_Price), main = "Differences of Daily Gold Prices by ounce",
     ylab = "Price(USD)", xlab = "Days")
# it helped to remove trend as expected. 
# Now I will take log-return of data
plot(diff(log(df2$Ounce_Price)), main = "Log return of Daily Gold Prices by ounce",
     ylab = "Log of Price(USD)", xlab = "Days") 


acf(diff(log(df2$Ounce_Price)), main = "ACF of Log return of Daily Gold Prices by ounce",
    ylab = "Log of Price(USD)", xlab = "Days") 

#ø it seems there is a cyclic seasonality in the dataset fro ACF plot. 

pacf(diff(log(df2$Ounce_Price)), main = "PACF of Log return of Daily Gold Prices by ounce",
     ylab = "Log of Price(USD)", xlab = "Days") 

# ACF suggests MA(0) and SMA(>=2)
# PACF refers that AR(0) but SAR(>=2)


d=1
D=0

for(p in 0:1){
  for(q in 0:1){
    for(P in 0:4){
      for(Q in 0:5){
        if( p+q+P+Q+d+D <= 6){ # parcimony principle
          model <- arima(x= log(df2$Ounce_Price), order=c(p,d,q), 
                         seasonal = list(order=c(P,D,Q), frequency = 7))
          sse <- sum(resid(model)^2)
          order <- paste(p,d,q,P,D,Q)
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          cat(p,d,q,P,D,Q,7, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', 
              pval$p.value,'\n')
        }
      }
    }
  }
}

# 0 1 0 0 0 0 7 AIC= -14617.12  SSE= 0.09118448  p-VALUE= 0.2608558
# 0 1 0 2 0 3 7 AIC= -14615.66  SSE= 0.09080153  p-VALUE= 0.5245891 

sarima(log(df2$Ounce_Price), 0,1,0,2,0,3,7)
# from plots drawed residuals did not quite normally distributed, howewer
# from p-values generated from Ljung-Box statistic there are still significant
# autocorrelation left in residuals. 
# So I need to change my model.

sarima(log(df2$Ounce_Price), 0,1,0,0,0,0,7)
# from plots drawed, residuals did not quite normally distributed, howewer
# from p-values generated from Ljung-Box statistic there are still significant
# autocorrelation left in residuals. 
# So I need to change my model.

modelx <- arima(x=log(df2$Ounce_Price), order = c(0,1,0), 
                seasonal = list(order=c(2,0,3), period=7))
plot(forecast(modelx))
forecast(modelx, 50)

plot(forecast(modelx ,100),xlim=c(1800,2200),ylim=c(7,8))

auto.arima(log(df2$Ounce_Price), seasonal = TRUE)
# ARIMA(0,1,0) 
# 
# sigma^2 estimated as 4.431e-05:  log likelihood=7395.47
# AIC=-14788.94   AICc=-14788.94   BIC=-14783.31


auto.arima(log(df2$Ounce_Price), start.P = 2, start.Q = 2, max.order = 6, 
           d=1, ic="aic", trace=TRUE, stepwise = FALSE)

sarima(log(df2$Ounce_Price), 4,1,1)


##################Decompose Trend and Seasonality 


comps <- decompose(ts(log(df2$Ounce_Price), deltat= 1/365))
plot(comps)
summary(comps)
# I realized after looking at the plots that the seasonality occurs in every 5 
# time slots, so from now on I determined seasonality equal to 5. 

x <- log(df2$Ounce_Price) - comps$seasonal
df1_stationary <- diff(x, differences = 1)
plot(df1_stationary)

acf(df1_stationary, lag.max = 50)
pacf(df1_stationary, lag.max = 50)

d=1
D=0

for(p in 0:1){
  for(q in 0:1){
    for(P in 0:5){
      for(Q in 0:4){
        if( p+q+P+Q+d+D <= 6){ # parcimony principle
          model <- arima(x= log(df2$Ounce_Price), order=c(p,d,q), 
                         seasonal = list(order=c(P,D,Q), frequency = 5))
          sse <- sum(resid(model)^2)
          order <- paste(p,d,q,P,D,Q)
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          cat(p,d,q,P,D,Q,5, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', 
              pval$p.value,'\n')
        }
      }
    }
  }
}

# 0 1 0 0 0 0 5 AIC= -14617.12  SSE= 0.09118448  p-VALUE= 0.2608558 
# 0 1 0 3 0 2 5 AIC= -14615.68  SSE= 0.09080062  p-VALUE= 0.5201874

sarima(df2$Ounce_Price, 0,1,0,3,0,2,5)
sarima(df2$Ounce_Price, 0,1,0,0,0,0,5)


# from ACF and PACF plots p,q,P,Q determined manually

model2 <- arima(x=log(df2$Ounce_Price), order = c(0,1,0), 
                seasonal = list(order=c(4,0,3), period=5), method = "ML")
summary(model2)
#  aic = -14637.75
plot(forecast(model2),xlim=c(1900,2100),ylim=c(7.2,7.5))

Box.test(model2$residuals)
sarima(log(df2$Ounce_Price), 0,1,0,4,0,3,5)
# still there is autocorrelation occurs according to p-values 


#######################################################################
################ TES - Triple Exponential Smoothing ###################
# since the dataset has both trend and seasonality I used Triple Exponential
# Smoothing method to forecast 

hw <- HoltWinters(ts(df2$Ounce_Price, frequency = 7))
hw
plot(forecast(hw,100),xlim=c(250,310),ylim=c(1400,1850))


nrow(df2)
alpha <- hw$alpha
beta <- hw$beta
gamma <- hw$gamma

time_diff <- as.POSIXct(strptime(max(df2$dates), "%Y-%m-%d"))-as.POSIXct(
  strptime("2020-06-01", "%Y-%m-%d"))

time_diff

# Now my dataset has prices for 2038 days.
# And I want to forecast 1st of June 2020, which is 94 days 
# later from the last day of the dataset. 

2038+94
94%%7

# So I can manually calculate it by using model coefficients above. 
# Price at day 2132(2038+94) = Coef(a) + forecast_day*coef(b) + coef(s3)
# 
# I took into account coef of s3 by calculating the result of 94's modulo 
# at base 7 that is 3. Since I determined 7 as the frequency of time series, 
# it generates a circle of 7 day periods, so I just tried to find exact location 
# of day 94 in that 7 day period process. 


fc2132 <- 1610.2717875 + (94*0.8984085) + (-0.6778132)

# Forecast of my fitted model will become 1694.044 USD for 
# Ounce price at 1st of June 2020.

forecast(hw,94)

# The result is same as calculated. 

######## STLF function ########

?ets

# Usually a three-character string identifying method using the
# framework terminology of Hyndman et al. (2002) and Hyndman 
# et al. (2008). 
# The first letter denotes the error type ("A", "M" or "Z"); 
# the second letter denotes the trend type ("N","A","M" or "Z");
# the third letter denotes the season type ("N","A","M" or "Z"). 
# In all cases, "N"=none, "A"=additive, "M"=multiplicative 
# and "Z"=automatically selected. 
# So, for example, "ANN" is simple exponential smoothing 
# with additive errors, "MAM" is multiplicative Holt-Winters'
# method with multiplicative errors, and so on.



modelstl <- stlf(ts(df2$Ounce_Price, frequency = 7), 
           etsmodel="AAN",damped=NULL)

modelstl
plot(forecast(modelstl,100),xlim=c(275,300),ylim=c(1450,1700))

modelstl <- stlf(ts(df2$Ounce_Price, frequency = 7), 
           etsmodel="ZZN",damped=NULL)

modelstl
plot(forecast(modelstl,100),xlim=c(275,300),ylim=c(1450,1800))

modelstl <- stlf(ts(df2$Ounce_Price, frequency = 7), 
           etsmodel="MMN",damped=NULL)

modelstl
plot(forecast(modelstl,100),xlim=c(275,300),ylim=c(1450,1800))


# from forecasts it seems that the model predicts an upward trend with small
# seasonality. 

stlm(ts(df2$Ounce_Price, frequency = 7), method = "arima", robust = T)


