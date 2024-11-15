Data1<-read.csv(file=choose.files(),stringsAsFactors=FALSE)
str(Data1)
data.ts<-ts(Data1)
plot(data.ts)
str(data.ts)
plot(Data1)
#4 
# Assuming Data1 contains your data values
y1 = ts(data.ts,end = 2022,frequency = 4)
plot(y1)

y2 = ts(data.ts,end = 2022,frequency = 1)
plot(y2)


# Display quarterly time series plot
#5
install.packages('forecast')
library(forecast)

dat.lm1 = tslm(data.ts ~ trend)
summary(dat.lm1)

accuracy(dat.lm1)
plot(data.ts)
lines(dat.lm1$fitted)
# 6
install.packages('ggplot2')

library(ggplot2)

data.ts <- ts(Data1,end = 2022)

autoplot(data.ts) + autolayer(ma(data.ts, 3), series = "3 month" ) + autolayer(ma(data.ts, 6), series = "6 month" ) + ggtitle("graph Q6") 

#7
fc1 = ses(data.ts, h = 1, alpha = 0.1)
fc1$mean

fc2 = ses(data.ts, h = 1, alpha = 0.5)
fc2$mean

fc3 = ses(data.ts, h = 1, alpha = 0.9)
fc3$mean

fc4 = ses(data.ts, h = 1)
summary(fc4)
fc4$mean

autoplot(data.ts) + autolayer(ma(data.ts, 3), series = "3 month" ) + autolayer(ma(data.ts, 6), series = "6 month" ) + autolayer(data.ts, h = 1) + ggtitle("graph Q7")


###8 

DT_5 = data.ts[1:115,]
# DT_5 = ts(DT_5)


end_5<-c(2022,10)
ts_5 = ts(data = DT_5,end = end_5,frequency = 12)
fit_1<-HoltWinters(ts_5,alpha = 0.1, beta = 0.1 , gamma = 0.1,seasonal = "multiplicative")
forc_1<-forecast(fit_1,h = 5)
forc_1

fit_2<-HoltWinters(ts_5,alpha = 0.9, beta = 0.9 , gamma = 0.9,seasonal = "multiplicative")
forc_2<-forecast(fit_2,h = 5)
forc_2

fit_op<-HoltWinters(ts_5,seasonal = "multiplicative")
forc_3 <-forecast(fit_op,h=5,prediction.interval = FALSE)
accuracy(forc_3)
