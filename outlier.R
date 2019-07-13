library(outliers)
library(depth)
library(DMwR)

  

mpg %>% funModeling::df_status()

data <- mpg[,c("displ","hwy")]

par(mfrow=c(1,1))

data %>% plot_histogram()
data %$% plot(displ,hwy)
data %>% plot_scatterplot(by="hwy")


#±3σ
data %>% 
  filter(abs(displ - mean(displ)) / sd(displ) <= 3)


#Smirnov-Grubbs test  
grubbs.test(data$displ,11)
grubbs.test(data$hwy,11)


#depth approach
depth::isodepth(data,mustdith = TRUE)
outlier <- depth::isodepth(data,mustdith = TRUE,output = TRUE)


#LOF
lof.scores <- DMwR::lofactor(data,k = 10)
hist(lof.scores)

#スコアの閾値を決めて、それ以上のものを抽出
is.ol <- lof.scores >= 2

#それだけを取り出す(index番号で取り出している)
data[is.ol,]





library(OutlierDetection)

#このライブラリだけでよさげ！