library(mlr)
library(tidyverse)


# 計算並列化 -------------------------------------------------------------------
library(doParallel)
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

#コア数はプロンプトでset NUMBER_OF_PROCESSORS
 



## 1) タスクの定義 ----
# ここではデータと応答変数、問題の種類を定義する。
task = makeClassifTask(data = iris, target = "Species")

## 2) 学習器の定義 ----
# アルゴリズムを選択する。
lrn = makeLearner("classif.lda")

# (データを訓練セットとテストセットに分割する)
n = nrow(iris)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

## 3) 訓練 ----
# 訓練セットのランダムなサブセットを用いて学習器を訓練し、モデルを作成
model = train(lrn, task, subset = train.set)

## 4) 予測 ----
# モデルに基いてテストセットの入力変数に対応する出力を予測する
pred = predict(model, task = task, subset = test.set)

## 5) 評価 ----
# 誤分類率並びに精度を計算する
performance(pred, measures = list(mmce, acc))



yeast <- getTaskData(yeast.task)

glimpse(yeast)

yeast <- as_tibble(yeast)
yeast

labels = colnames(yeast)[1:14]
yeast.task = makeMultilabelTask(id = "multi", data = df, target = labels)

head(listLearners()[c("class", "package")])
