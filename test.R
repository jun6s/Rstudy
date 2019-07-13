
library(ggplot2)
library(purrr)
library(dplyr)
#library(lawstat)

library(brunnermunzel)


#Studentのt検定：正規性と等分散性を仮定
#Welchのt検定：不等分散であってもよいが正規性を仮定
#Mann-WhitneyのU検定：正規性は仮定しないが不等分散の状況では精度が落ちる
#Brunner-Munzel検定：正規性も等分散性の仮定もない

#正規性も等分散性も成り立たない状況では
#平均値の比較：Welchのt検定
#中央値の比較：Brunner-Munzel検定が検定精度がよい傾向にある。

#https://oku.edu.mie-u.ac.jp/~okumura/stat/brunner-munzel.html


# Normal distribution
test_norm <- function(R = 2000, N = c(30, 30),
                      mu = c(0, 0), sigma = c(1, 1)) {
  y <- purrr::map(seq_len(R), function(i) {
    x1 <- rnorm(N[1], mean = mu[1], sd = sigma[1])
    x2 <- rnorm(N[2], mean = mu[2], sd = sigma[2])
    wl <- wilcox.test(x1, x2)$p.value
    bm <- brunner.munzel.test(x1, x2)$p.value
    c(wl, bm)
  })
  df <- data.frame(p_value = purrr::simplify(y),
                   method = rep(c("Wilcoxon", "BM"), R))
  plot <- ggplot(df, aes(p_value, fill = method)) +
    geom_histogram(binwidth = 0.05, boundary = -0.5,
                   position = "dodge")
  print(plot)
  summary <- df %>%
    dplyr::mutate(lt5 = if_else(p_value < 0.05, 1 / (n() / 2), 0)) %>%
    dplyr::group_by(method) %>%
    dplyr::summarize_at("lt5", sum)
  print(summary)
}

set.seed(20180316)
test_norm(R = 5000, N = c(15, 45), mu = c(0, 0), sigma = c(1, 4))
test_norm(R = 5000, N = c(30, 30), mu = c(0, 0), sigma = c(1, 4))
test_norm(R = 5000, N = c(45, 15), mu = c(0, 0), sigma = c(1, 4))


# Gamma distribution
shape <- c(2, 4)
scale <- c(2, 1)
ggplot(data.frame(x = seq(0, 10, 0.01)), aes(x)) +
  stat_function(col = "red", fun = dgamma,
                args = list(shape = shape[1], scale = scale[1])) +
  stat_function(col = "blue", fun = dgamma,
                args = list(shape = shape[2], scale = scale[2]))

test_gamma <- function(R = 2000, N = c(30, 30),
                       shape = c(2, 4), scale = c(2, 1)) {
  y <- purrr::map(seq_len(R), function(i) {
    x1 <- rgamma(N[1], shape = shape[1], scale = scale[1])
    x2 <- rgamma(N[2], shape = shape[2], scale = scale[2])
    wl <- wilcox.test(x1, x2)$p.value
    bm <- brunner.munzel.test(x1, x2)$p.value
    c(wl, bm)
  })
  df <- data.frame(p_value = purrr::simplify(y),
                   method = rep(c("Wilcoxon", "BM"), R))
  plot <- ggplot(df, aes(p_value, fill = method)) +
    geom_histogram(binwidth = 0.05, boundary = -0.5,
                   position = "dodge")
  print(plot)
  summary <- df %>%
    dplyr::mutate(lt5 = if_else(p_value < 0.05, 1 / (n() / 2), 0)) %>%
    dplyr::group_by(method) %>%
    dplyr::summarize_at("lt5", sum)
  print(summary)
}

set.seed(20180316)
test_gamma(R = 5000, N = c(45, 15), shape = shape, scale = scale)
test_gamma(R = 5000, N = c(30, 30), shape = shape, scale = scale)
test_gamma(R = 5000, N = c(15, 45), shape = shape, scale = scale)

##
set.seed(20180318)
gamma_median <- function(R = 2000, N = 100, shape = 2, scale = 2) {
  map_dbl(seq_len(R), function(i) {
  rgamma(N, shape = shape, scale = scale) %>%
      median()
  })
}
gamma_median(shape = 2, scale = 2) %>% summary()
gamma_median(shape = 4, scale = 1) %>% summary()

