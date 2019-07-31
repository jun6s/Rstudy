library(tidyquant)
library(magrittr)

ls("package:tidyquant") %>% print()

FANG %>% print()

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


X_xts <- getSymbols("AAPL", auto.assign = FALSE)

my_bloomberg_data <- c('SPX Index','ODMAX Equity') %>%
    tq_get(get         = "Rblpapi",
           rblpapi_fun = "bdh",
           fields      = c("PX_LAST"),
           options     = c("periodicitySelection" = "WEEKLY"),
           from        = "2016-01-01",
           to          = "2016-12-31")

eur_usd <- tq_get("EUR/USD", 
                  get = "exchange.rates", 
                  from = Sys.Date() - lubridate::days(10))

tq_


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

