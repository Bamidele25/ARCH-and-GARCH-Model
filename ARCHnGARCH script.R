#other template for reading the daily time series set 
myts <- ts(rnorm(length(inds)),     # random data
           start = c(2014, as.numeric(format(inds[1], "%j"))),
           frequency = 365)
#read the first regime of theh daily data set 
ex <- read.csv (file.choose (), header = T)
ex<- read.csv("C:\\Users\\LENOVO PC\\Documents\\tom 1.csv")
head(ex)
#load in zoo package, for reading of the daily time series data set 
ext <- zoo (ex, seq(from = as.Date("2012-01-03"),to = as.Date("2015-12-31"), by = "day"))
head(ext)
#read in the second regime of the daily data set 
ex2<- read.csv("C:\\Users\\LENOVO PC\\Documents\\tom 2.csv")
#creat a daily dates object 
inds <- seq(from = as.Date("2016-01-04"),to = as.Date("2018-12-31"), by = "day")
#create the zoo object using zoo packages
ext2<- zoo(ex2, inds)
head(ext2)
tail(ext2)
summary(ext)
summary(ext2)
plot.ts(ext)
plot.ts(ext2)
plot.ts(ext$Naira.GBP, pch= 16, main = "Naira/GBP", xlab = "Year", ylab = "EXR", type = "o", col ="red")
skewness(ext)
pckg(fgrach,forecast,car,rugarch,moments,urca,tseries,quantmod,zoo,parallel,xts)
kurtosis(ext)
kurtosis(ext2)
skewness(ext)
skewness(ext2)
plot.ts(ext$Naira.USD, pch= 16, main = "Naira/USD", xlab = "Year", ylab = "EXR Naira/USD", type = "o", col ="red")
plot.ts(ext$Naira.Yuan, pch= 16, main = "Naira/Yuan", xlab = "Year", ylab = "EXR Naira/Yuan", type = "o", col ="red")
#jarque bera test for the first regrimes (2012-2015)
jarque.bera.test(ext$Naira.GBP)
jarque.bera.test(ext$Naira.USD)
jarque.bera.test(ext$Naira.Yuan)
#jarque bera test for the second regrimes (2016-2018)
jarque.bera.test(ext2$Naira.GBP)
jarque.bera.test(ext2$Naira.USD)
jarque.bera.test(ext2$Naira.Yuan)
sd(ext$Naira.GBP)
sd(ext$Naira.USD)
sd(ext$Naira.Yuan)
sd(ext2$Naira.GBP)
sd(ext2$Naira.USD)
sd(ext2$Naira.Yuan)
par(mfrow=c(2,2), pch = 16)
#time series plot for the first regime of the series (2012-2015)
plot.ts(ext$Naira.GBP, pch= 16, main = "Naira/GBP", xlab = "Time", ylab = "EXR Naira/GBP", type = "o", col ="red")
plot.ts(ext$Naira.USD, pch= 16, main = "Naira/USD", xlab = "Time", ylab = "EXR Naira/USD", type = "o", col ="red")
plot.ts(ext$Naira.Yuan, pch= 16, main = "Naira/Yuan", xlab = "Time", ylab = "EXR Naira/Yuan", type = "o", col ="red")
par(mfcol=c(2,1), pch = 16)
#time series plot for the second regime of the series (2016-2019)
plot.ts(ext2$Naira.GBP, pch= 16, main = "Naira/GBP", xlab = "Time", ylab = "EXR Naira/GBP", type = "o", col ="red")
plot.ts(ext2$Naira.USD, pch= 16, main = "Naira/USD", xlab = "Time", ylab = "EXR Naira/USD", type = "o", col ="red")
plot.ts(ext2$Naira.Yuan, pch= 16, main = "Naira/Yuan", xlab = "Time", ylab = "EXR Naira/Yuan", type = "o", col ="red")
par(mfrow=c(2,2), pch = 16)
plot(ext2$Naira.GBP, xlab = "Time", ylab = "EXR Naira/GBP",main = "EXR Naira/GBP", pch = 16)
plot(ext2$Naira.USD, xlab = "Time", ylab = "EXR Naira/USD",main = "EXR Naira/USD", pch = 16)
plot(ext2$Naira.Yuan, xlab = "Time", ylab = "EXR Naira/Yuan",main = "EXR Naira/Yuan", pch = 16)
plot(ext2$Naira.GBP, xlab = "Time", ylab = "EXR Naira/GBP",main = "EXR Naira/GBP", pch = 16
#Autocorrelation FUnction of the first regimes 
par(mfrow = c(2,2), pch = 16)
acf(diff(ext$Naira.GBP), lag.max = 25,type = "partial", main = "ACF of Naira/GBP")
acf(diff(ext$Naira.USD), lag.max = 25,type = "partial", main = "ACF of Naira/USD")
acf(diff(ext$Naira.Yuan), lag.max = 25,type = "partial", main = "ACF of Naira/Yuan")
#Autocorrelation Function of the Second regimes 
par(mfrow = c(2,2), pch = 16)
acf(diff(ext2$Naira.GBP), lag.max = 25,type = "correlation", main = "ACF of Naira/GBP")
acf(diff(ext2$Naira.USD), lag.max = 25,type = "correlation", main = "ACF of Naira/USD")
acf(diff(ext2$Naira.Yuan), lag.max = 25,type = "correlation", main = "ACF of Naira/Yuan")
#Stationarity of the series for the first regime 
adf.test(ext$Naira.GBP, k = 1),
adf.test(ext$Naira.USD, k = 1),
adf.test(ext$Naira.Yuan),
#Stationarity of the series for the second regime 
adf.test(ext2$Naira.GBP, k = 1)
adf.test(ext2$Naira.USD, k = 1)
adf.test(ext2$Naira.Yuan, k=1)
#Difference of the series 
dGBP<-diff(ext$Naira.GBP)
dUSD = diff(ext$Naira.USD)
dYuan = diff(ext$Naira.Yuan)
head(dGBP),
#stationary at 1st difference for first regime
adf.test(dGBP, k= 1)
adf.test(dUSD, k= 1)
adf.test(dYuan, k=1)
#Difference of the series 2nd regime 
dGBP2<-diff(ext2$Naira.GBP)
dUSD2 = diff(ext2$Naira.USD)
dYuan2 = diff(ext2$Naira.Yuan)
#Stationary at 1st difference for second regime 
adf.test(dGBP2, k= 1)
adf.test(dUSD2, k= 1)
adf.test(dYuan2, k= 1)
#stationary for PP for first regime
pp.test(ext$Naira.GBP,lshort=TRUE)
pp.test(ext$Naira.USD,lshort=TRUE)
pp.test(ext$Naira.Yuan,lshort=TRUE)
#difference stationary for first regime
pp.test(dGBP, lshort = TRUE)
pp.test(dUSD, lshort = TRUE)
pp.test(dYuan, lshort = TRUE)
#stationary for pp for second regime
pp.test(ext2$Naira.GBP,lshort=TRUE)
pp.test(ext2$Naira.USD,lshort=TRUE)
pp.test(ext2$Naira.Yuan,lshort=TRUE)
#differences stationary for second regime pp
pp.test(dGBP2,lshort = TRUE)
pp.test(dUSD2,lshort = TRUE)
pp.test(dYuan2,lshort = TRUE)
plot.ts (dGBP, main = "Difference Naira/GBP", pch =16, type = "l",col = "red", xlab = "Time", ylab = "diff Naira/GBP")
#plot after difference for the first regime 
par(mfrow = c(2,2))
plot.ts (dGBP, main = "Difference Naira/GBP", pch =16, type = "l",col = "red", xlab = "Time", ylab = "diff Naira/GBP")
plot.ts (dUSD, main = "Difference Naira/USD", pch =16, type = "l",col = "red", xlab = "Time", ylab = "diff Naira/USD")
plot.ts (dYuan, main = "Difference Naira/Yuan", pch =16, type = "l",col = "red", xlab = "Time", ylab = "diff Naira/Yuan")
#plot after difference for the Second regime 
par(mfrow = c(2,2))
plot.ts (dGBP2, main = "Difference Naira/GBP", pch =16, type = "l",col = "red", xlab = "Time", ylab = "diff Naira/GBP")
plot.ts (dUSD2, main = "Difference Naira/USD", pch =16, type = "l",col = "red", xlab = "Time", ylab = "diff Naira/USD")
plot.ts (dYuan2, main = "Difference Naira/Yuan", pch =16, type = "l",col = "red", xlab = "Time", ylab = "diff Naira/Yuan")
#testing the present of ARCH effect in the ARIMA model for the first regime in the series 
fit1<-auto.arima (lyuan, trace =TRUE, test="kpss", ic = "aic")
par(mfrow =c(1,2))
acf(lyuan, type = "partial", lag.max = 25,main = "Series diff(lyuan)")
lusd <- log(ex2$Naira.USD)
lgbp <- log(ex2$Naira.GBP)
lyuan <- log(ex$Naira.Yuan)
fit1
tmod<- FitARMA(ex$Naira.GBP, c(1,1,1))
tmod
summary(tmod)
attributes(tmod)
tmod<- FitARMA(dGBP, c(2,0,1))
tmod<- FitARMA(dGBP, c(3,0,1))
dex2 <- diff(ex$Naira.USD)
dex3<-diff(ex$Naira.Yuan)
dst2<- auto.arima(dex2, trace = T,test = "kpss", ic = "aic")
dst2
dst3<-auto.arima(dex3, trace = T,test = "kpss", ic = "aic")
dst3
dtm<- diff(ex2$Naira.GBP)
dtm2<- diff(ex2$Naira.USD)
dtm3<- diff(ex2$Naira.Yuan)
dtmod<- auto.arima(dtm, trace = T, test = "kpss",ic = "aic")
dtmod
dtmod2<- auto.arima(dtm2,trace = T,test = "kpss", ic = "aic")
dtmod2
dtmod3<- auto.arima(dtm3, trace = T,test = "kpss", ic = "aic")
dtmod3
Box.test(dtmod$residuals^2, type = "Ljung-Box", lag = 10)
Box.test(dtmod2$residuals^2, type = "Ljung-Box", lag = 10)
Box.test(dtmod3$residuals^2, type = "Ljung-Box", lag = 10)
plot(dtmod3$residuals,col = "red")
#step 1 gbp regime 1
x = ugarchspec(variance.model = list(garchOrder = c(0,1)),mean.model = list(armaOrder = c(0,0)))
x_fit = ugarchfit(x,data = dtm)
x_fit
re_d = x_fit@fit$residuals
acf(re_d, lag.max = 25, type = "correlation", main = "ACF of \nARCH(1) Residual")
acf(re_d, lag.max = 25, type = "partial", main = "PACF of \nARCH(1) Residual")
jarque.bera.test(re_d)

plot(x_fit)
2
acf(dtmod3$residuals, lag.max = 20)
y = ugarchspec(variance.model = list(garchOrder = c(0,1)),mean.model = list(armaOrder = c(1,1)))
y_fit = ugarchfit(y,data = dG)
z = ugarchspec(variance.model = list(garchorder = c(1,2),mean.model = list(armaorder = c(1,1)))
z_fit = ugarchfit(z,data = dGBP)
z_fit
#GBP regime 2
x1 = ugarchspec(variance.model = list(garchorder = c(1,0)),mean.model = list(armaorder = c(1,1)))
x1_fit = ugarchfit(x1,data = ex$Naira.GBP)
x1_fit
plot(x_fit)
xfit_f = ugarchforecast(x1_fit,n.head = 90)

ug_res = x1_fit@fit$residuals
head(ug_res)
jarque.bera.test(ug_res)
y1 = ugarchspec(variance.model = list(garchorder = c(1,1)),mean.model = list(armaorder = c(0,0)))
y1_fit = ugarchfit(y1,data = ex$Naira.GBP)
y1_fit
z1 = ugarchspec(variance.model = list(garchorder = c(1,2)),mean.model = list(armaorder = c(1,1)))
z1_fit = ugarchfit(z1,data = dGBP2)
z1_fit
#USD regime 1
a = ugarchspec(variance.model = list(garchOrder = c(1,1)),mean.model = list(armaOrder = c(0,0)))
a_fit = ugarchfit(a,data =log(ext2$Naira.USD))
plot(a_fit)
1
2
b = ugarchspec(variance.model = list(garchorder = c(0,1)),mean.model = list(armaorder = c(0,0)))
b_fit = ugarchfit(b,data = dUSD2)
b_fit
c = ugarchspec(variance.model = list(garchorder = c(1,2)),mean.model = list(armaorder = c(1,1)))
c_fit = ugarchfit(c,data = dUSD2)
c_fit
#USD regime 2
a1 = ugarchspec(variance.model = list(garchorder = c(1,1)),mean.model = list(armaorder = c(1,1)))
a1_fit = ugarchfit(a1,data = ex2$Naira.USD)
a1_fit
ug1_res = a1_fit@fit$residuals
head(ug1_res)
jarque.bera.test(ug1_res)
b1 = ugarchspec(variance.model = list(garchorder = c(0,1)),mean.model = list(armaorder = c(0,0)))
b1_fit = ugarchfit(b1,data = dUSD2)
b1_fit
c1 = ugarchspec(variance.model = list(garchorder = c(1,2)),mean.model = list(armaorder = c(1,1)))
c1_fit = ugarchfit(c1,data = dUSD2)
c1_fit
#Yuan regime 1
ya = ugarchspec(variance.model = list(garchOrder = c(0,1)),mean.model = list(armaOrder = c(1,1)))
ya_fit<-ugarchfit(ya, data = dYuan)
ya_fit
ya1 = ugarchspec(variance.model = list(garchorder = c(0,1)),mean.model = list(armaorder = c(1,1)))
ya1_fit = ugarchfit(ya1, data = dYuan)
ya1_fit
ya2 = ugarchspec(variance.model = list(garchorder = c(1,2)),mean.model = list(armaorder = c(1,1)))
ya2_fit = ugarchfit(ya2,data = dYuan)
ya2_fit
#yuan regime 2
yu = ugarchspec(variance.model = list(garchorder = c(1,1)),mean.model = list(armaorder = c(1,1)))
yu_fit<-ugarchfit(yu, data = dYuan2)
yu_fit
ug2_res = yu_fit

sqr_ug2 = ug2_res^2
head(ug2_res)
jarque.bera.test(ug2_res)
yu1 = ugarchspec(variance.model = list(garchorder = c(0,1)),mean.model = list(armaorder = c(0,0)))
yu1_fit = ugarchfit(yu1, data = dYuan2)
yu1_fit
yu2 = ugarchspec(variance.model = list(garchorder = c(1,2)),mean.model = list(armaorder = c(1,1)))
yu2_fit = ugarchfit(yu2,data = dYuan2)
yu2_fit
nys = garch(dGBP, order = c(0,1))
summary(nys)
ug_res = x_fit@fit$residuals
ug_coef = x_fit@fit$coef
#Autocorrelation FUnction for the residual 
par(mfrow = c(2,2), pch = 16)
acf(ug_res, lag.max = 25,type = "partial", main = "PACF residuals\nof GARCH (1,1)")
acf(ug1_res, lag.max = 25,type = "partial", main = "PACF ACF residuals\nof GARCH (1,1)")
acf(ug2_res, lag.max = 25,type = "partial", main = "PACF ACF residuals\nof GARCH (1,1)")
Box.test(ug2_res^2,lag = 10,type = "Ljung-Box")
l_dYuan = log(ext2$Naira.Yuan)
dY = diff(l_dYuan)
plot(dY, type = "l",pch = 16)
par(mfrow = c(2,2))
acf(dGBP2,lag.max = 25, type = "partial", main = "PACF residuals\nof GARCH (1,1)")
acf(dUSD2,lag.max = 25, type = "partial", main = "PACF residuals\nof GARCH (1,1)")
acf(dY,lag.max = 25, type = "partial", main = "PACF residuals\nof GARCH (1,1)")
#forecasting value
xfit_f = ugarchforecast(x1_fit,n.head = 365)
yfit_f = ugarchforecast(a1_fit, n.head = 365)
zfit_f = ugarchforecast(yu1_fit, n.head = 365)
print(xfit_f,yfit_f,zfit_f)
print.data.frame(xfit_f,yfit_f,zfit_f)
?print.data.frame
print(xfit_f)
par(mfrow = c(1,1))
plot(xfit_f, type = "l")
lines(xfit_f, col= "0range")
lines(zfit_f, col = "green")