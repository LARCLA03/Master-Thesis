rm(list = ls())
#Importing data
library(readxl)
library(plotmo)
library(dplyr)
library(lubridate)
library(leaps)
library(MARX)
library(ggplot2)
library(QRM)
library(quantmod)
library(tseries)
library(fUnitRoots)
library(quantmod)
library(corrplot)
library(xlsx)
library(stargazer)
library(corrplot)

#DATA TABLES

BTC <- read.table("BTC_dtt_daily.csv",header = TRUE, sep=";")



#Modelling BTC using MARX

data <- na.omit(BTC)
y <- as.matrix(diff(log(data[,2])))
x <- as.matrix(log(data[2:nrow(data),3:ncol(data)]) - log(data[1:(nrow(data)-1),3:ncol(data)])) 
plot(y, type = "l", xlab = "Observations", ylab = "BTC price", main = "BTC returns from the begginning of 2015 to December 2021")


#Performing augmented dickey fuller test to test for stationnarity of the TS of BTC
adf.test(y)

#Using MARX package to model BTC 

#Modelling taking into consideration exogeneous factors 

marx(y, x,10, 0.05)

#Modelling without exogenous factors creating a MAR model


marx(y, NULL, ceiling(log(nrow(data))), 0.05)


#Precise selection

#1. Pseudo causal

#Information criteria


acf(na.omit(y), lag.max = ceiling(log(nrow(data))),  main = "ACF for BTC returns") # actually no significant autocorrelation existing, no wonder that no lags are chosen in MARX
pacf(na.omit(y), lag.max = ceiling(log(nrow(data))),  main = "PACF for BTC returns")

#Computing information criteria

val <- aic(y, x, ceiling(log(nrow(data))))
val2 <- bic(y, x, ceiling(log(nrow(data))))
val3 <- hq(y, x, ceiling(log(nrow(data))))

val <- aic(y, x, 10)
val2 <- bic(y, x,10)
val3 <- hq(y, x, 10)


output.IC <- as.data.frame(rbind(val$values, val2$values, val3$values))

row.names(output.IC) <- c("AIC", "BIC", "HQ")

#pseudo causal model identification


pseudo.model <- pseudo(y, x, 7)

#testing for normality of the residuals

ks.test(pseudo.model$residuals, "pnorm")

selection.lag(y,NULL , 7)

summary(pseudo)



#final model selection

result_MLE <- selection.lag.lead(y, x, 10)
mixed_res <- mixed(y, x, 7 , 2)
final <- summary(mixed_res)

acf(mixed_res$residuals, lag.max = 10)

forecasts <- forecast.marx(y, x, 5,5, h=10)

plot(forecasts, type="l")

#----------------------------------------------------------------------------------------------------------------------------------------------



#--- check 1:  no significant autocorrelation in y

lm(y[-1]~y[-length(y)])



# run marx(x[,2] for info

#--- check 2: to match the t distribution assumption in MARX, we can test: H0: y in t dist or H0: y=b*x+e, e in t dist 

y.tfit<-fit.st(y)

ks.test(y/y.tfit$par.ests[3] - y.tfit$par.ests[2],"pt",df=y.tfit$par.ests[1])

modelYonX = lm(y[2:length(y)]~x[1:(nrow(x)-1),])

eps = modelYonX$residuals

eps.tfit<-fit.st(eps)

ks.test(eps/eps.tfit$par.ests[3] - eps.tfit$par.ests[2],"pt",df=eps.tfit$par.ests[1])

# we do not reject the residuals in t dist so marx() is still valid



#Distribution of the returns

d <- density(y)
plot(d)

h<-hist(y, breaks=10, col="red", xlab="Returns of BTC ",
        main="Histogram of BTC returns")

#Forecast BTC

data_out <- read.table("BTC_oos.csv",header = TRUE, sep=";")
y_out <- as.matrix(diff(log(data_out[,2])))


abs <- c(1:31)
y_out <- data.frame(abs, y_out)
names <- c("abs", "tv")
colnames(y_out) <- names



result_forecasts <- forecast.marx(y, NULL, p_C =5, p_NC=5,h=31)
write.xlsx(result_forecasts, "F:/mémoire/DATA/Tables/forecasts_BTC.xlsx")



result_ff <- data.frame(abs, result_forecasts)
names2 <- c("abs", "forecasted")
colnames(result_ff) <- names2

#Performance
res <- y_out$tv - result_ff$forecasted
som.lignes <- Reduce("+", res)
MSE_BTC <- (1/31)*((som.lignes)^2)


#Plot

ggplot()+
  geom_line(data = result_ff, mapping = aes(x =abs, y=forecasted), color="blue")+
  geom_point(data = result_ff, mapping = aes(x =abs, y=forecasted), color="blue")+
  geom_line(data = y_out , mapping = aes(x = abs, y=tv), color="red")+
  geom_point(data = y_out , mapping = aes(x = abs, y=tv), color="red")+
  labs(x="Days", y="Rate of return", title ="Comparison between true values and forecasted values of BTC")


