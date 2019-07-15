library(infotheo)
library(discretization)


#切り捨ててカテゴリ化
diamonds %>% 
  mutate(price = as.factor((floor(price/100))*100)) %>% 
  janitor::tabyl(price)



#等間隔あるいは等頻度に分割
ew.data <- discretize(iris, disc="equalwidth",nbins = 3)
ef.data <- discretize(USArrests, disc="equalfreq",nbins=3)


# トップダウンアルゴリズムの実行(method引数 1:CAIM 2:CACC 3:Ameva)
cm <- disc.Topdown(iris, method=1)


#カイマージ、カテゴリ列が最後に必要。
chiM.iris <- chiM(iris, alpha=0.05)


#最小記述長原理を用いた離散化(Minimum Description Length Principle, MDLP)
mdlp.iris <- mdlp(iris)