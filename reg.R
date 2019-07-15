library(tidyverse)
library(magrittr)
library(xts)
library(quantmod)

today <- Sys.Date()


getSymbols("FB",from="2019-04-01",to=today)
getSymbols("AAPL",from="2019-04-01",to=today)
getSymbols("AMZN",from="2019-04-01",to=today)


# 回帰・重回帰 ------------------------------------------------------------------

stk.xts<- merge(AAPL$AAPL.Adjusted,FB$FB.Adjusted,AMZN$AMZN.Adjusted)
stk.df <- as.data.frame(coredata(stk.xts))


#基本文法はこう(説明変数、被説明変数の違いに注意)
res1 <- lm(formula = AAPL.Adjusted~AMZN.Adjusted,
          data = stk.df)


#summaryとplot→回帰係数をよく見て！
summary(res1)

plot(stk.df$AMZN.Adjusted,stk.df$AAPL.Adjusted)
abline(res1)



data.frame(res1$fitted.values,
           res1$residuals,
           stk.df)


#1~5
plot(res1,which=1)
plot(res1,which=2)
plot(res1,which=5)



#対数をとるとこうなる
res2 <- lm(formula = log(AAPL.Adjusted)~log(AMZN.Adjusted),
          data = stk.df)

summary(res2)


#図示のみであれば
ggplot(stk.df,aes(x=AMZN.Adjusted,y=AAPL.Adjusted))+
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

#glmであれば
ggplot(stk.df,aes(x=AMZN.Adjusted,y=AAPL.Adjusted))+
  geom_point() +
  geom_smooth(method = "glm", 
            method.args = list(family = poisson(link = "logit")),se = FALSE)

# 非線形回帰 -------------------------------------------------------------------
library(mgcv)
library(MASS)

USbondssymbols <- paste0("DGS",c(1,2,3,5,7,10,20,30))

ust.xts <- xts()
for (i in 1:length( USbondssymbols ) ) {
  ust.xts <- merge( 
    ust.xts,
    getSymbols( 
     USbondssymbols[i],
     auto.assign = FALSE,
     src = "FRED",
     from="2019-04-01",
     to="2019-07-12"
    )
  )
}

ust.xts <- ust.xts["2019-04-01::"]

na.exclude(ust.xts)

#smooth.spline
#ksmooth
#suspmu
#lowess


install.packages('mpath')
install.packages('tidyquant')
