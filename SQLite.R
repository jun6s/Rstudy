# インストールしてから
# ライブラリの読み込み
library("tidyverse")
library("RSQLite")


# SQLite に接続する
con = dbConnect(SQLite(), "test.db", synchronous="off")

input_file = "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031849356&fileKind=1"
read_data <- read_csv(input_file, locale=locale(encoding="CP932"))

# データフレームをDBに投入する
con %>% dbWriteTable("seika", read_data, row.names=F)


#追加するとき
con %>% dbWriteTable("seika", read_data, row.names=F,append=TRUE)

# dplyr 的な操作で抜く
tmp <- con %>% tbl("seika") %>%   # 接続→テーブル名 という感じで指定
filter(品目名 == "だいこん") %>%
arrange(日) %>%
collect()   # ここでクエリ投げて、データ取得

# dplyr 的な操作で抜く
tmp <- con %>% tbl("seika") %>%   # 接続→テーブル名 という感じで指定
filter(品目名 == "だいこん") %>%
arrange(日) %>%
show_query() # どんなSQL投げているかを返してくれる

# == 実行結果 ==
# <SQL>
# SELECT *
# FROM `seika`
# WHERE (`品目名` = 'だいこん')
# ORDER BY `日`


# クエリ書いて条件絞って抜くならこれ
tmp <- con %>% dbGetQuery( "SELECT * FROM seika" )

# フェッチで投げる
rs <- con %>% dbSendQuery( "SELECT * FROM seika" )
tmp <- rs %>% dbFetch()
rs %>% dbClearResult()