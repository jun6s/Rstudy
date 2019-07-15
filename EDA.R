library(tidyverse)
library(DataExplorer)
library(skimr)
library(magrittr)
library(GGally)

options(scipen=50)


# 最初 ----------------------------------------------------------------------

diamonds %>% 
  glimpse()

diamonds %>% 
  funModeling::df_status()



# 短変量分析 -------------------------------------------------------------------

#factors
diamonds %>% 
  plot_bar()

diamonds %>%
  select_if(is.factor) %>% 
  map(janitor::tabyl)



#continuous
diamonds %>% 
  plot_histogram()

diamonds %>% 
  psych::describe()

diamonds %>% 
  funModeling::profiling_num()




# 群間比較(1変数固定) -------------------------------------------------------------

#離散値キーで離散値分類→テーブル
diamonds %$% table(cut,color)
diamonds %$% table(cut,clarity)

diamonds %>% 
  janitor::tabyl(cut,color)


#離×離を図示するとこう
diamonds %>% 
  ggplot(.,aes(x=color,fill=cut))+
  geom_bar(position = "dodge") #position = fill,stack,dodge


#離散値キーで連続値の分布測定
diamonds %>% 
　plot_boxplot(by = "cut")


#連続値キーで分布測定→scatter一択です
diamonds %>% 
  plot_scatterplot(by="price")



# 相関の一括プロット ------------------------------------------------------------------

install.packages('minerva')

diamonds %$% minerva::mine(x,price)

#相関ならplot_correlation
split_columns(diamonds)$continuous %>% 
  plot_correlation()

#ggpairs一択
diamonds %>% 
  select_if(is.numeric) %>% 
  head(1000) %>% 
  ggpairs()

#色付け
iris %>% 
  ggpairs(aes(colour = Species, shape=Species,pointsize = 3))





# 回帰線 ---------------------------------------------------------------------

#performanceanalysticgがいうて一発でひいてくれる
iris %>% 
  select_if(is.numeric) %>% 
  PerformanceAnalytics::chart.Correlation()


#個別にやるならgeom_smoothかstat_smooth




# 確率分布推定 ------------------------------------------------------------------

#qqプロット


#正規性テスト
shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width)
shapiro.test(iris$Petal.Length)



# 主成分 ---------------------------------------------------------------------

#plot_prcompが便利


split_columns(diamonds)$continuous %>% 
  plot_prcomp(
  　　variance_cap = 0.95,
    　prcomp_args = list(scale. = TRUE),  #標準化
      ncol = 2, nrow = 2
    )



# Tips --------------------------------------------------------------------

#けっこう便利な関数
diamonds %>% 
  split_columns()

#ちなみにこう渡さなくちゃいけない（でも一行で書けてエレガント）
split_columns(diamonds)$continuous




