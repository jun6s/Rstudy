library(tidyquant)
library(magrittr)

ls("package:tidyquant") %>% print()

FANG %>% 
  print()


FANG %>% 
  group_by(symbol) %>% 
  nest()

tq_get_options()
tq_transmute_fun_options()
tq_performance_fun_options()
tq_mutate_fun_options()
tq_exchange_options()

# get data ----------------------------------------------------------------


#stock data
X <- tq_get("AAPL", 
  get = "stock.prices", 
  from = "2010-01-01") %T>% print()



df<- tq_get("EUR/USD",get="exchange.rates",
            from = "2019-07-01",
            to   = "2019-07-31")



my_bloomberg_data <- c('SPX Index','ODMAX Equity') %>%
    tq_get(get         = "Rblpapi",
           rblpapi_fun = "bdh",
           fields      = c("PX_LAST"),
           options     = c("periodicitySelection" = "WEEKLY"),
           from        = "2016-01-01",
           to          = "2016-12-31")





eur_usd <- tq_get("EUR/USD", 
                  get = "exchange.rates", 
                  from = "2019-04-01")


#便利
X_xts %>% {
  head(.) %>% print() 
  class(.) %>% print()
}





# core functions ----------------------------------------------------------

tq_transmute()
tq_transform()
tq_performance()
tq_mutate()




eur_usd


# data取得 ------------------------------------------------------------------

#data取得元
tq_get_options()

#FRED
#treasury

USbondssymbols <- paste0("DGS",c(1,2,3,5,7,10,20,30))

t <-  tq_get(USbondssymbols,
             get = "economic.data",
             from = "2017-01-01",
             to = "2017-03-31")

eur_usd <- tq_get("EUR/USD", 
                  get = "exchange.rates", 
                  from = Sys.Date() - lubridate::days(10))


#bloomberg
my_bloomberg_data <- c('SPX Index','ODMAX Equity') %>%
    tq_get(get         = "Rblpapi",
           rblpapi_fun = "bdh",
           fields      = c("PX_LAST"),
           options     = c("periodicitySelection" = "WEEKLY"),
           from        = "2016-01-01",
           to          = "2016-12-31")





# リターンの計算 -----------------------------------------------------------------

#→債券はどうする？

#日次対数収益率
log_returns <- TOPIX100 %>% 
  group_by(symbol) %>%
  tq_mutate(
    select = close, 
    mutate_fun = periodReturn,
    period = "daily",
    type = "log",
    col_rename = "log_return"
  )

#対数リターン可視化
log_returns %>% 
  filter(grepl("^YJ7", symbol)) %>%
  ggplot(aes(x = date, y = log_return, colour = symbol)) +
    geom_line() +
    theme_tq() +
    scale_color_tq() +
    facet_wrap(~ symbol, ncol = 3, scales = "free_y")

log_returns %>% 
  filter(grepl("^YJ7", symbol)) %>%
  ggplot(aes(x = log_return, fill = symbol)) +
    geom_histogram(binwidth = 0.005) +
    theme_tq() +
    scale_color_tq() +
    facet_wrap(~ symbol, ncol = 3, scales = "free_y")


#リターンとボラの計算
return_vol <- log_returns %>%
   group_by(symbol) %>%
   summarise(return = mean(log_return), vol = sd(log_return))

return_vol

tq_transmute_fun_options()




# ポートフォリオのリターンとリスク----------------------------------------------------------


#銘柄とウェイトを決定
stocks <- c("YJ3382.T", "YJ4755.T", "YJ6954.T", "YJ7974.T", "YJ9983.T", "YJ9984.T")
weights <- c(rep(0.15, 4), rep(0.2, 1))

log_returns %>% 
   filter(symbol %in% stocks) %>% 
   tq_portfolio(assets_col = symbol,
                returns_col = log_return,
                weights = weights) 


# modeling ----------------------------------------------------------------


