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




library("RSQLite")

con = dbConnect(SQLite(), "pp.db", synchronous="off")
#指定した名前の.dbにつなぐ。なければ作る。

dbSendQuery(con, "create table hoge(hinmei text, nedan int)")
#テーブルをつくる。なければはじかれる

dbListTables(con)
dbListFields(con, "hoge")
#テーブルがあることを確認。デーブルの定義がされていることを確認。

dbSendQuery(con, "insert into hoge values('りんご', 500)")
dbSendQuery(con, "insert into hoge values('みかん', 300)")
dbGetQuery(con, "select * from hoge")
#送ったものが入っているか確認
dbSendQuery(con, "update hoge set nedan=200 where hinmei='みかん'")
#中の値をupdateしてみる
dbGetQuery(con, "select * from hoge")
dbReadTable(con, "hoge")
#中にはいっているか確認