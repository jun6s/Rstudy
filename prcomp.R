library(tidyverse)
library(DataExplorer)


#主成分の可視化
PCA_VARIANCE_CAP <- 0.9 # 寄与率累積の閾値

split_columns(diamonds)$continuous %>% 
  plot_prcomp(
  　　variance_cap = PCA_VARIANCE_CAP,
    　prcomp_args = list(scale. = TRUE),  #標準化
      ncol = 2, nrow = 2
    )


split_columns(diamonds)$continuous %>% 
  prcomp(scale=T) -> pca

#図示
biplot(pca)
　#→どう解釈すんの？

plot(pca$rotation[,1])

summary(pca)

pca$x






# kernel PCA --------------------------------------------------------------

install.packages('kernlab')
