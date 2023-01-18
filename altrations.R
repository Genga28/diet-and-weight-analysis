a<-read.csv("E:\\Sem 3\\Applied statistics for business decisions\\Lab\\Project\\diet n weight analysis\\DietData.csv")
print(a)

#Markov Chain Monte Carlo
m<-0
s<-1
set.a<-1
sample<-rnorm(10000,m,s)
mean(sample)
summary(replicate(1000,mean(rnorm(10000,m,s))))

#CUMSUM
cummean <- function(x)
  cumsum(x) / seq_along(x)
plot(cummean(sample), type="l", xlab="Sample", ylab="Cumulative mean",
     panel.first=abline(h=0, col="red"), las=1)

# TIMESERIES
t<-read.csv("E:\\AAA Semester\\Sem 3\\Applied statistics for business decisions\\Lab\\Project\\diet n weight analysis\\DIET AND WEIGHT TS.csv")
print(t)
mt <- c(t$Initial_weight,t$Final_weight)
mts <- ts(mt, frequency = 365.25 / 7)
print(mts)
is.ts(mts)
plot(mts)

#auto correlation
acf(mts)

#auto regression
AR <- arima(mts, order = c(1,0,0))
print(AR)


