input<-read.csv("C://Users//dhars//Documents//cat//dataset.csv")
print(input)
sharpesmodel<-(input$EXPECTED.RETURN-input$RISK.FREE.RATE)/input$STANDARD.DEVIATION
print(sharpesmodel)
barplot(
  sharpesmodel,
  main="Outliers",
  xlab="Sharpe's Model",
  names.arg = c(input$ï..Companies)
  
)
treynermodel<-(input$EXPECTED.RETURN-input$RISK.FREE.RATE)/input$BETA
print(treynermodel)
OneWayAnova <- aov(input$EXPECTED.RETURN ~ input$STANDARD.DEVIATION)
summary(OneWayAnova)
TwoWayAnova <- aov(input$EXPECTED.RETURN ~ input$STANDARD.DEVIATION+input$RISK.FREE.RATE)
summary(TwoWayAnova)
Regression <- lm(input$EXPECTED.RETURN ~ input$STANDARD.DEVIATION +input$RISK.FREE.RATE)
print(Regression)
cat("# # # # The Coefficient Values # # # ","\n")
a <- coef(Regression)[1]
print(a)
XSTANDARDDEVIATION <- coef(Regression)[2]
XRISKFREERATE <- coef(Regression)[3]
print(XRISKFREERATE,NA.RM=TRUE)
print(XSTANDARDDEVIATION,NA.RM=TRUE)
x1<-40
x2<-0.290833333
Y<-a+(XRISKFREERATE*x1)+(XSTANDARDDEVIATION*x2)
print(Y)

timese <- read.csv("C://Users//dhars//Documents//cat//TimeSeries.csv")
print(timese)
mt <- c(timese$Open.Price,timese$Close.Price)
mts <- ts(mt, frequency = 365.25 / 7)
plot(mts, xlab ="Daily Data",
     ylab ="Price ",
     main ="TCS",
     col.main ="darkgreen")
