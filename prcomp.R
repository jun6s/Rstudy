library(DataExplorer)


PCA_VARIANCE_CAP <- 0.9 # 寄与率累積の閾値

split_columns(diamonds)$continuous %>% 
  plot_prcomp(
  　　variance_cap = PCA_VARIANCE_CAP,
    　prcomp_args = list(scale. = TRUE),  #標準化
      ncol = 2, nrow = 2
    )
