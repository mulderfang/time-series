###################################
#         Lecture 16             #
###################################
#----------Time Series------------#
###################################

library(forecast)

#ts(vector, start=, end=, frequency=)
ts(1:20, frequency = 4, start = 2019) #freq=1, year; =12, month; =4, quarter...
ts(1:20, frequency = 12, start = c(2019,4)) #start from April, 2019

temp<-c(39,37,61,58,18,56,82,27,41,69,49,66,54,42,90,66)
ts(temp, frequency = 12, start = c(2019,1)) 

install.packages("fpp")
library(fpp)
class(a10)  # a10 data: Monthly anti-diabetic drug sales in Australia from 1992 to 2008.
plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")
# strong seasonal pattern that increases in size as the level of the series increases. 
# The sudden drop at the end of each year.
# trend and seasonal pattern exist.

ts.plot(a10) #same as plot()

#outlier detect
install.packages('ggfortify')
library(ggfortify)
install.packages('changepoint')
library(changepoint)
autoplot(cpt.meanvar(ts(a10))) #identify changes in mean and var
#can sep into 2 parts to fit models

#--------------Decomposition-----------------
#STL is a method for decomposing your data into the three components
#(i.e. exploratory data analysis and visualization) 

fit_a <- stl(log(a10), s.window="periodic")  #stl() only for addictive model
# s.window：控制季節效應變化的速度, periodic 季節效應每年一樣
# t.window：控制趨勢變化的速度,越小的值表示越快的變化速度 
fit_a
plot(fit_a)

seasonal <- fit_a$time.series[,1]
trend <- fit_a$time.series[,2]

# another way
fit_b <- decompose(a10, type="mult") #another way to decompose
#since the data here, increase as time and has a seasonal effect, use mult
plot(fit_b)

trend_b <- fit_b$trend
seasonal_b<- fit_b$seasonal

ggplot(a10,aes(x=c(1:204))) +
  geom_line(aes(y=a10)) +
  geom_line(aes(y=trend_b,colour='fit_b$trend')) +
  geom_line(aes(y=seasonal_b,colour='fit_b$seasonal'))

# forecast
reg <- tslm(a10~season+trend)
summary(reg)
fcat <- forecast(reg, h=24)
plot(fcat)
#--------------------------------------------------------
#------------------Forecast----------------------------
library(tidyverse)
fit_a %>% forecast(method='naive') %>% #can add h=XX after method
  autoplot() + xlab("Year")

#another way: forecasts of stl objects
a10 %>% stlf(method='naive') %>%
  autoplot() 



#----take out seasonal effect----
data <- decompose(a10,type='mult')
data2 <- a10/data$seasonal   #if additive, a10-data$seasonal
plot(a10)
plot(data2)

#can also use
data2 <- a10/fit_a$time.series[,1]

#another way: seasonal adjuted
sadj <- seasadj(fit_a)
plot(naive(sadj))

#If the seasonal component is removed from the original data, 
#the resulting values are called the "seasonally adjusted" data.

fit_a %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("ETS forecasts of seasonally adjusted data")



#--another way to check the data--
ggAcf(a10)
#The slowly decaying ACF indicates trend. 
#The ACF peaks at lags 12, 24, 36, . . . ,indicate seasonality of length 12.

#trend: the autocorrelations for small lags tend to be large and positive.
#seasonal: the autocorrelations will be larger at the seasonal lags (i.e., at multiples of the seasonal frequency)
#(here)trended and seasonal: you see a combination of these effects.

#-----------Seasonal Pattern----------------------------
seasonplot(a10,ylab="$ million", xlab="Year", 
           main="Seasonal plot: antidiabetic drug sales",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)
#large jump in sales in January each year. 
#there was an unusually low number of sales in March 2008 

ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) + ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

ggseasonplot(a10, polar=TRUE) + ylab("$ million")


monthplot(a10,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1,at=1:12,labels=month.abb,cex=0.8)
#The horizontal lines indicate the means for each month

ggsubseriesplot(a10) + ylab("$ million") + ggtitle("Subseries plot: antidiabetic drug sales")


#-----------------White noise----------------------------------
#--if data is white noise, no need to fit the model------------
Box.test(a10,12,"Ljung") 
# not white noise

acf(a10) #check if obs are random and independent in this interval
ggAcf(a10)
#If all autocorrelation coefficients lie within the limits +-1.96/sqrt(n), confirming
#that the data are white noise. 

##white noise example 
x<-rnorm(1000)
tsx=ts(x)
plot(tsx)
acf(tsx)
Box.test(tsx,12,"Ljung")

#-----------------Autocorrelation-----------------------------
data2 <- window(a10, start=1992, end=2006)
lag.plot(data2, lags=9, do.lines=FALSE)
Acf(data2)


#------------------Stationary-----------------------------
adf.test(a10) #stationary. 
#If not sig. means not stationary
kpss.test(a10)
adf.test(nhtemp) #another dataset; not stationary
acf(nhtemp) 
dnhtemp <- diff(nhtemp)
adf.test(dnhtemp)
acf(dnhtemp)

diff(nhtemp,difference=2) #lag2

#For non-stationary data, the value of r1 is often large and positive.

#------------------Example: AR(1)----------------------------------------
wn<-rnorm(1100)
x<-0
x[1]<-0
for (i in 2:1100){
  x[i]<-0.5*x[i-1]+wn[i]
}
ts<-ts(x[101:1100])

plot(ts)
Pacf(ts)


#--------------Fit model---------------------
acf(dnhtemp) #ma model
Pacf(dnhtemp) 
tsdisplay(dnhtemp)

(ma = auto.arima(nhtemp)) # auto fitted

predict(ma, 5) # predict the first 5
p=forecast(ma,5)
plot(p)

ma %>% forecast %>% autoplot

#---------------Model diagnostics-------------------------------
qqnorm(ma$residuals)
qqline(ma$residuals)

acf(residuals(ma))$acf

Box.test(ma$resid,lag=1,type="Ljung") #residuals are not correlated
tsdiag(ma,gof=5,omit.initial=F) #gof is the maximum number of lags in the acf function used in the model diagnostics.

accuracy(ma)
checkresiduals(ma)
#-----------------Basic Forecasting---------------------------------
## Average method
meanf(nhtemp,36) #36 future 36 months
## Naïve method
naive(nhtemp,36) 
rwf(nhtemp,36)
## Seasonal naïve method
snaive(nhtemp,36)
## Drift method
rwf(nhtemp,36, drift=TRUE)

forecast(nhtemp,h=36)

# create test data
data2 = window(nhtemp, end=1960)
plot(nhtemp,xlim=c(1912,1971))
lines(meanf(data2,h=12)$mean,col=4)
lines(rwf(data2,h=12)$mean,col=2)
lines(rwf(data2,h=12,drift=TRUE)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))

## Evaluating forecast accuracy
data3 = window(nhtemp, start=1961)
accuracy(meanf(data2,h=12), data3) #can get RMSE MAE MAPE MASE...
accuracy(rwf(data2,h=12), data3)
accuracy(rwf(data2,h=12,drift=TRUE), data3)
#pick the smallest; the third one

res <- residuals(naive(data2))
Acf(res) #should within the bonds

checkresiduals(naive(data2))

#----------interactive plot-----------------------------
devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyAxis("y", label = "Temp (F)", valueRange = c(40, 60)) %>%
  dyOptions(fillGraph = TRUE, drawGrid = FALSE) %>%
  dyRangeSelector()


devtools::install_github("RamiKrispin/TSstudio")
library(TSstudio)

class(a10)
ts_plot(a10)
ts_seasonal(a10,type="all")
ts_heatmap(a10)
ts_surface(a10)
ts_lags(a10)
ts_acf(a10)
ts_pacf(a10)


##FYI: Exponential Smoothing
temp = HoltWinters(a10,beta=F,gamma=F,optim.start = c(alpha = 0.3))
fcast = forecast(temp, h=24)
forecast:::forecast.HoltWinters(temp, h=24) #or u can use this
plot(fcast)
