library(caret)
#http://topepo.github.io/caret/index.html


# 計算並列化 -------------------------------------------------------------------
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)



# data --------------------------------------------------------------------

set.seed(0)
N <- 1500
x1 <- runif(N, min = -5, max = 5)
x2 <- runif(N, min = -10, max = 10)
x3 <- runif(N, min = 0, max = 10)
y <- (sin(x1 *  pi/2)*4 + x1*0.5) * x2 + x3*2 + (x3^2)*0.4 + rnorm(N, mean = 0, sd = 1)

dataRegression <- data.frame(
  y = y,
  x1 = x1,
  x2 = x2,
  x3 = x3
)

# テスト用と学習用にデータを分ける
dataRegressionTrain <- dataRegression[1:1000,]
dataRegressionTest <- dataRegression[1001:1500,]





indexIris <- which(1:nrow(iris)%%3 == 0)
# テスト用と学習用にデータを分ける
 irisTrain <- iris[-indexIris,]
 irisTest <- iris[indexIris,]

# train -------------------------------------------------------------------

#第一引数はformula
  #→y ~(.)^2もしくはy~(.)がよい
#data,method,preprocessはそのまま
  #preProcess = c(‘center’, ‘scale’)が基本

#trControl
  #trControl = trainControl(method = “cv”) クロスバリデーション
  #trControl = trainControl(method = “boot”) ブートストラップ法

#tuneLength/tuneGrid
  #ハイパーパラメータの選択
　#tuneLength = 4
  #tuneGrid = expand.grid(size=c(1:10), decay=seq(0.1, 1, 0.1)), とすると細かく設定できる




# 回帰学習 ----------------------------------------------------------------------

set.seed(0)
modelNnet <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain,
  method = "nnet", 
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  tuneGrid = expand.grid(size=c(1:10), decay=seq(0.1, 1, 0.1)),
  linout = TRUE
)



# 分類学習 --------------------------------------------------------------------

#formulaに注意。カテゴリ変数~(.)^2 になる

# ランダムフォレスト（RandomForestパッケージ）

set.seed(0)
irisRF <- train(
  Species ~ (.)^2, 
  data = irisTrain, 
  method = "rf", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)


# 予測 ----------------------------------------------------------------------

predNnet <- predict(modelNnet, dataRegressionTest)
predIrisRF <- predict(irisRF, irisTest)


confusionMatrix(data = predIrisRF, irisTest$Species)











# xgboost -----------------------------------------------------------------

set.seed(0)
modelXgboostLinear <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain,
  method = "xgbLinear", 
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  tuneLength = 4
)



# 勾配ブースティングによる予測 xgboost:Treeモデルの結合
set.seed(0)
modelXgboostTree <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain,
  method = "xgbTree", 
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  tuneLength = 4
)

predxgBoostLinear <- predict(modelXgboostLinear, dataRegressionTest)
predxgBoostTree <- predict(modelXgboostTree, dataRegressionTest)

#回帰の予測性能テスト
 sqrt( sum((dataRegressionTest$y - predxgBoostLinear)^2) / 500 )
 sqrt( sum((dataRegressionTest$y - predxgBoostTree)^2) / 500 )
