library(tidyverse)
library(corrr)

#sample data
set.seed(12345)
store_sales_tbl <- tibble(
  date = rep(seq(ymd("2019-01-01"), ymd("2019-12-31"), by = "1 week"), 5),
  store_name = c(rep("A", 53), rep("B", 53), rep("C", 53), rep("D", 53), rep("E", 53)),
  sales = rpois(53 * 5, 10)
)




#transmute(.data, ...) 指定した列以外を保持しない版 mutate() 。

#変化率を計算する関数
cr <- function(x) {
  (x - lag(x)) / lag(x) * 100
}


store_sales_cor_df <- store_sales_tbl %>%
  spread(store_name, sales) %>%
  transmute_if(is.integer, cr) %>%
  correlate()

store_sales_cor_df

store_sales_cor_df %>%
  rplot(print_cor = TRUE)
