library(prophet)
#論文：https://peerj.com/preprints/3190.pdf


# quick start -------------------------------------------------------------------

m <- prophet(res)


#prophetオブジェクトの作成
m <- prophet(res)

#未来をどれくらいまで予測するのか
future <- make_future_dataframe(m, freq = 60,periods = 100)

#モデルへのfit
forecast <- predict(m, future)



#図示はかんたん
plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)


#?
prophet::plot_forecast_component(m,forecast)




# tuning parameters ----------------------------------------------------------

?prophet

#growth引数→"linear" "logistic"を選択できる
#changepoints → 日付のベクトルを指定。ex. changepoints=c("2019-07-26")
#n.changepoints
#changepoints.range
#holidays →data.frameを渡す


#xxx.seasonality TRUE/FALSE


# 外部変数を利用するケース ------------------------------------------------------------

# 空の予測モデルを作る。
m <- prophet(weekly.seasonality = FALSE, daily.seasonality = FALSE)
 
# 外部変数として列名"nichigin"を設定
m <- add_regressor(m, "nichigin")
 
# prophetモデルの元データとして上で作成したdfを設定
m <- fit.prophet(m, df)
 
# 予測したい日数をprophetライブラリ同梱のmake_future_dataframe関数を使って作る。
# そしてこちらにも予測に使うべき外部変数として列"nichigin"を設定する。
future <- make_future_dataframe(m, periods = 368) %>%
        mutate(ds = parse_date(ds)) %>%
        left_join(nichigin_tbl, by = "ds")
 
# 予測
forecast <- predict(m, future)


# SHF 実装 ------------------------------------------------------------------

#Simulated Historical Forecasts
SHF <- function(input, n.td = 365 * 3, max.horizon = 180, step = 90, log = FALSE){
        if(log){
                dataset <- input %>%
                  filter(y != 0) %>%
                  mutate(y = log(y))
        }else{
                dataset <- input
        }
        dataset <- dataset %>%
          mutate(ds = as.Date(ds)) %>%
          arrange(ds)
 
        cutoffs <- seq(min(dataset$ds) + n.td, max(dataset$ds), by = step)
        fcst <- data.frame()
        for(i in cutoffs){
                training.data <- dataset %>% filter(ds < i, ds >= i - n.td)
                m <- prophet(training.data)
                future <- make_future_dataframe(m, periods = max.horizon)
                fcst <- rbind(fcst,
                              predict(m, future) %>%
                                filter(ds >= i) %>%
                                mutate(h = seq(1, length(ds)), cutoff = min(ds))
                              )
        }
 
        if(log){
                fcst$yhat <- exp(fcst$yhat)
                dataset$y <- exp(dataset$y)
        }
 
        fcst <- fcst %>%
          inner_join(dataset, by = "ds") %>%
          select(ds, yhat, y, h, cutoff) %>%
          mutate(error = yhat - y) %>%
          mutate(relative.error = error/y)
        fcst.horizon <- fcst %>%
          group_by(h) %>%
          summarise(mape = mean(abs(relative.error)), vpe = var(relative.error))
        return(list(forecast = fcst, horizon = fcst.horizon))
}



# randomized SHF ----------------------------------------------------------

fcst_fixed_horizon <- function(input, horizon = 50, n.trial = 2, n.training = 365 * 3, step = 10){
        if(nrow(input) - n.training < n.trial){
                print("Error: the observations of input data is smaller than training data.")
                return(NULL)
        }
 
        min.date <- min(input$ds) + n.training + 1
        max.date <- max(input$ds) - horizon
        learning.seq <- sample(input$ds[input$ds > min.date & input$ds < max.date], n.trial)
        out <- data.frame()
        for(di in learning.seq){
                td <- input %>% filter(ds < di, ds >= di - n.training)
                m <- prophet(td)
                future <- make_future_dataframe(m, periods = horizon)
                forecast <- predict(m, future) %>% tail(horizon) %>% mutate(h = seq(1, horizon))
                out <- rbind(out, tail(forecast, step)) #予測した日に対応する実績が無いかもしれないので幅をもたせる
        }
        return(out)
}
rSHF <- function(input, step = 7, min.fcst = 1, max.fcst = 180, n.trial = 2, n.training = 365 * 3, log = FALSE){
        if(log){
                dataset <- input %>%
                  filter(y != 0) %>%
                  mutate(y = log(y))
        }else{
                dataset <- input %>% mutate(ds = as.Date(ds))
        }
        dataset <- dataset %>%
          mutate(ds = as.Date(ds)) %>%
          arrange(ds)
 
        fcst <- data.frame()
        for(i in seq(min.fcst, max.fcst, by = step)){
                fcst <- rbind(fcst, fcst_fixed_horizon(dataset, horizon = i, n.trial = n.trial, n.training = n.training, step = step))
        }
 
        if(log){
                fcst$yhat <- exp(fcst$yhat)
                dataset$y <- exp(dataset$y)
        }
        fcst <- fcst %>%
          inner_join(dataset, by = "ds") %>%
          select(ds, yhat, y, h) %>%
          mutate(error = yhat - y) %>%
          mutate(relative.error = error/y)
        fcst.horizon <- fcst %>%
          group_by(h) %>%
          summarise(mape = mean(abs(relative.error)), vpe = var(relative.error))
 
        return(list(forecast = fcst, horizon = fcst.horizon))
}
