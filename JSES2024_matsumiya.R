#------------------------------------------------------------
#This R script was presented at the 76th Annual Conference of the Japan Society of Educational Sociology held on September 13, 2024.
#The title was "Gender Differences in Teacher Recruitment."
#If you need a translation of the following description, which is in Japanese, please feel free to contact me, and I will provide you with an English translation.
#------------------------------------------------------------

# ------------------------------------------------------------
# 準備
# ------------------------------------------------------------
# Rのバージョン確認
R.version

# ワークスペースのクリア: オブジェクト一括削除
rm(list = ls())

# データの取り込み
df <- read.csv(file.choose(), stringsAsFactors = FALSE)
## 結果を確認
head(df, 5)

# ------------------------------------------------------------
# パッケージの読み込み
# ------------------------------------------------------------
library(tidyverse)
library(survey)
library(WeightIt)
library(survey)
library(ggplot2)
library(dplyr)
library(EValue)

# ------------------------------------------------------------
# 前処理：データフレームの作成
# ------------------------------------------------------------
# 指定された変数のみを含むデータフレームを作成
selected_df <- df[, c("id", "sq1", "sq2", "sq5", "q1_1", "q1_2", "q1_3", "q7", "q8", "q9",
                      "q11_9" )]

# 各変数の要約統計量と頻度分布を確認
for(col in names(selected_df)[-1]) {  # idを除外
  cat("\n変数:", col, "\n")
  
  # 数値型の場合は要約統計量を表示
  if(is.numeric(selected_df[[col]])) {
    print(summary(selected_df[[col]]))
  }
  
  # カテゴリカル変数の場合は頻度分布を表示
  else {
    print(table(selected_df[[col]], useNA = "ifany"))
  }
  
  # 欠損値の数を表示
  cat("欠損値の数:", sum(is.na(selected_df[[col]])), "\n")
}

# ------------------------------------------------------------
# 前処理：変数の作成
# ------------------------------------------------------------
# sq1（性別）を0（男性）と1（女性）のダミー変数に変換
selected_df$gender_dummy <- ifelse(selected_df$sq1 == 2, 1, 0)
## 新しい変数の確認
table(selected_df$sq1, selected_df$gender_dummy, useNA = "ifany")
## 元の変数と新しい変数の対応を確認
print(table(selected_df$sq1, selected_df$gender_dummy, useNA = "ifany"))

# sq2（年齢）を数値に変換したのち，世代に変換
## ルックアップテーブルの作成
age_lookup <- c(
  "2" = 25, "3" = 26, "4" = 27, "5" = 28, "6" = 29, "7" = 30, "8" = 31, "9" = 32,
  "10" = 33, "11" = 34, "12" = 35, "13" = 36, "14" = 37, "15" = 38, "16" = 39,
  "17" = 40, "18" = 41, "19" = 42, "20" = 43, "21" = 44, "22" = 45, "23" = 46,
  "24" = 47, "25" = 48, "26" = 49, "27" = 50, "28" = 51, "29" = 52, "30" = 53,
  "31" = 54, "32" = 55, "33" = 56, "34" = 57, "35" = 58, "36" = 59
)
## 新しい年齢変数の作成
selected_df$age_numeric <- age_lookup[as.character(selected_df$sq2)]
## 変換結果の確認
table(selected_df$sq2, selected_df$age_numeric, useNA = "ifany")
## 要約統計量の確認
summary(selected_df$age_numeric)
## 欠損値の確認
sum(is.na(selected_df$age_numeric))
## 年齢を出生年に変換
selected_df$birth_year <- 2020 - selected_df$age_numeric
## 変換結果の確認
head(data.frame(age = selected_df$age_numeric, birth_year = selected_df$birth_year), 10)
## 要約統計量の確認
summary(selected_df$birth_year)
## 出生年の分布を確認
table(selected_df$birth_year, useNA = "ifany")
## 欠損値の確認
sum(is.na(selected_df$birth_year))

# 採用試験受験の小・中・高について，以下のように変換
# 1（最初に受験した年に合格）と2（その後合格）　を1
# 3（合格しなかった）　を0
# 4（受験したことがない）　を9
## 変換関数の定義
recode_exam <- function(x) {
  ifelse(x %in% c(1, 2), 1,
         ifelse(x == 3, 0,
                ifelse(x == 4, 9, NA)))
}
## 変数の変換
selected_df$q1_1_recoded <- recode_exam(selected_df$q1_1)
selected_df$q1_2_recoded <- recode_exam(selected_df$q1_2)
selected_df$q1_3_recoded <- recode_exam(selected_df$q1_3)
## 変換結果の確認
for (var in c("q1_1", "q1_2", "q1_3")) {
  cat("\n", var, "の変換結果:\n")
  print(table(selected_df[[var]], selected_df[[paste0(var, "_recoded")]], useNA = "ifany"))
}
## 学校種ごとのサブグループの確認
cat("\n小学校受験者数:", sum(selected_df$q1_1_recoded %in% c(0, 1)))
cat("\n中学校受験者数:", sum(selected_df$q1_2_recoded %in% c(0, 1)))
cat("\n高等学校受験者数:", sum(selected_df$q1_3_recoded %in% c(0, 1)))
## 複数学校種の受験状況を確認
selected_df$exam_types <- rowSums(sapply(selected_df[, c("q1_1_recoded", "q1_2_recoded", "q1_3_recoded")], function(x) x %in% c(0, 1)))
table(selected_df$exam_types)

# q7（中3時成績）の変換（逆転）
selected_df$q7_recoded <- ifelse(selected_df$q7 == 1, 3,
                                 ifelse(selected_df$q7 == 2, 2,
                                        ifelse(selected_df$q7 == 3, 1, NA)))
## 変換結果の確認
cat("元のq7と変換後のq7_recodedのクロス表:\n")
print(table(selected_df$q7, selected_df$q7_recoded, useNA = "ifany"))
## 新しい変数の分布を確認
cat("\n変換後のq7_recodedの分布:\n")
print(table(selected_df$q7_recoded, useNA = "ifany"))
## 要約統計量の確認
cat("\n変換後のq7_recodedの要約統計量:\n")
print(summary(as.factor(selected_df$q7_recoded)))
## 欠損値の確認
cat("\n欠損値の数:", sum(is.na(selected_df$q7_recoded)), "\n")

# q8（卒業した大学）について，以下のように変換
# 1と2　を1，それ以外を0（国公立ダミー）
# 3　　 を1, それ以外を0（難関私立ダミー）
# 4　　 を1, それ以外を0（それ以外の私立ダミー）
##ダミー変数の作成
selected_df$national_public_dummy <- ifelse(selected_df$q8 %in% c(1, 2), 1, 0)
selected_df$top_private_dummy <- ifelse(selected_df$q8 == 3, 1, 0)
selected_df$other_private_dummy <- ifelse(selected_df$q8 == 4, 1, 0)
## 変換結果の確認
cat("元のq8と新しいダミー変数のクロス表:\n")
print(table(selected_df$q8, 
            national_public = selected_df$national_public_dummy,
            top_private = selected_df$top_private_dummy,
            other_private = selected_df$other_private_dummy,
            useNA = "ifany"))
## 各ダミー変数の分布を確認
for (var in c("national_public_dummy", "top_private_dummy", "other_private_dummy")) {
  cat("\n", var, "の分布:\n")
  print(table(selected_df[[var]], useNA = "ifany"))
}
## 欠損値の確認
cat("\n各ダミー変数の欠損値の数:\n")
print(sapply(selected_df[c("national_public_dummy", "top_private_dummy", "other_private_dummy")], function(x) sum(is.na(x))))
## 元のq8の分布確認
cat("\n元のq8の分布:\n")
print(table(selected_df$q8, useNA = "ifany"))

# q9（卒業した大学の学部）について，以下のように変換
# 1（教員養成系の教育学部）を1，それ以外を0（教員養課程成ダミー）
## 教員養成課程ダミー変数の作成
selected_df$teacher_training_dummy <- ifelse(selected_df$q9 == 1, 1, 0)
## 変換結果の確認
cat("元のq9と新しい教員養成学部ダミー変数のクロス表:\n")
print(table(selected_df$q9, selected_df$teacher_training_dummy, useNA = "ifany"))
## 新しいダミー変数の分布を確認
cat("\n教員養成学部ダミー変数の分布:\n")
print(table(selected_df$teacher_training_dummy, useNA = "ifany"))
## 要約統計量の確認
cat("\n教員養成学部ダミー変数の要約統計量:\n")
print(summary(as.factor(selected_df$teacher_training_dummy)))
## 欠損値の確認
cat("\n欠損値の数:", sum(is.na(selected_df$teacher_training_dummy)), "\n")
## 元のq9の分布確認
cat("\n元のq9の分布:\n")
print(table(selected_df$q9, useNA = "ifany"))

# q11_9（大学時代の行動：教員採用試験の対策を熱心に行っていた）について，以下のように変換(逆転)
# 4を1
# 3を2
# 2を3
# 1を4
##ダミー変数の作成
selected_df$q11_9_recoded <- ifelse(selected_df$q11_9 == 4, 1,
                                    ifelse(selected_df$q11_9 == 3, 2,
                                           ifelse(selected_df$q11_9 == 2, 3,
                                                  ifelse(selected_df$q11_9 == 1, 4, NA))))
##変換結果の確認
cat("元のq11_9と変換後のq11_9_recodedのクロス表:\n")
print(table(selected_df$q11_9, selected_df$q11_9_recoded, useNA = "ifany"))
## 新しい変数の分布を確認
cat("\n変換後のq11_9_recodedの分布:\n")
print(table(selected_df$q11_9_recoded, useNA = "ifany"))
## 要約統計量の確認
cat("\n変換後のq11_9_recodedの要約統計量:\n")
print(summary(as.factor(selected_df$q11_9_recoded)))
## 欠損値の確認
cat("\n欠損値の数:", sum(is.na(selected_df$q11_9_recoded)), "\n")
## 元のq11_9の分布確認
cat("\n元のq11_9の分布:\n")
print(table(selected_df$q11_9, useNA = "ifany"))

#------------------------------------------------------------
#記述的分析
#------------------------------------------------------------
# 関数の定義
calculate_descriptive_stats <- function(df, group_var) {
  df %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),  # サンプルサイズを追加
      合否_合格率 = mean(q1_1_recoded == 1 | q1_2_recoded == 1 | q1_3_recoded == 1, na.rm = TRUE),
      国公立大学_割合 = mean(national_public_dummy, na.rm = TRUE),
      難関私立大学_割合 = mean(top_private_dummy, na.rm = TRUE),
      その他私立大学_割合 = mean(other_private_dummy, na.rm = TRUE),
      教員養成課程_割合 = mean(teacher_training_dummy, na.rm = TRUE),
      中3時成績_平均 = mean(q7_recoded, na.rm = TRUE),
      世代_平均 = mean(birth_year, na.rm = TRUE),
      学生生活_平均 = mean(q11_9_recoded, na.rm = TRUE)
    )
}

# 小学校受験者の記述統計量
df_elementary <- selected_df %>% filter(q1_1_recoded %in% c(0, 1))
elementary_stats <- calculate_descriptive_stats(df_elementary, "gender_dummy")
print("小学校受験者の記述統計量:")
print(elementary_stats)

# 中学校受験者の記述統計量
df_juniorhigh <- selected_df %>% filter(q1_2_recoded %in% c(0, 1))
juniorhigh_stats <- calculate_descriptive_stats(df_juniorhigh, "gender_dummy")
print("中学校受験者の記述統計量:")
print(juniorhigh_stats)

# 高等学校受験者の記述統計量
df_highschool <- selected_df %>% filter(q1_3_recoded %in% c(0, 1))
highschool_stats <- calculate_descriptive_stats(df_highschool, "gender_dummy")
print("高等学校受験者の記述統計量:")
print(highschool_stats)

#------------------------------------------------------------
#分析: IP Weighting for Marginal Structural Models(学校歴の影響，小学校受験者)
#------------------------------------------------------------
#A（学校歴）→Y（合否），V（ジェンダー），L（中3時の成績，世代）
## 小学校受験者のみを選択
df_elementary <- selected_df %>%
  filter(q1_1_recoded %in% c(0, 1))
## 対象集団のサイズを確認
cat("対象集団のサイズ:", nrow(df_elementary), "\n")
## 傾向スコアの計算関数
calculate_ps <- function(treatment, df) {
  formula <- as.formula(paste(treatment, "~ gender_dummy + q7_recoded + birth_year"))
  glm(formula, data = df, family = binomial())
}
## 各学校歴変数の傾向スコアを計算
ps_models <- list(
  national_public = calculate_ps("national_public_dummy", df_elementary),
  top_private = calculate_ps("top_private_dummy", df_elementary),
  other_private = calculate_ps("other_private_dummy", df_elementary),
  teacher_training = calculate_ps("teacher_training_dummy", df_elementary)
)
## 傾向スコアをデータフレームに追加
df_elementary <- df_elementary %>%
  mutate(
    ps_national_public = predict(ps_models$national_public, type = "response"),
    ps_top_private = predict(ps_models$top_private, type = "response"),
    ps_other_private = predict(ps_models$other_private, type = "response"),
    ps_teacher_training = predict(ps_models$teacher_training, type = "response")
  )
## IP Weightingの計算
df_elementary <- df_elementary %>%
  mutate(
    ipw_national_public = 1 / ifelse(national_public_dummy == 1, ps_national_public, 1 - ps_national_public),
    ipw_top_private = 1 / ifelse(top_private_dummy == 1, ps_top_private, 1 - ps_top_private),
    ipw_other_private = 1 / ifelse(other_private_dummy == 1, ps_other_private, 1 - ps_other_private),
    ipw_teacher_training = 1 / ifelse(teacher_training_dummy == 1, ps_teacher_training, 1 - ps_teacher_training)
  )
## IP Weightingの要約統計量を確認
summary(df_elementary[, c("ipw_national_public", "ipw_top_private", "ipw_other_private", "ipw_teacher_training")])
## MSMの構築関数
build_msm <- function(treatment, ipw, df) {
  formula <- as.formula(paste("q1_1_recoded ~", treatment, "* gender_dummy"))
  design <- svydesign(ids = ~1, weights = df[[ipw]], data = df)
  svyglm(formula, design = design, family = quasibinomial())
}
## 各学校歴変数に対してMSMを構築
msm_models_schoolcareer_elementary <- list(
  national_public = build_msm("national_public_dummy", "ipw_national_public", df_elementary),
  top_private = build_msm("top_private_dummy", "ipw_top_private", df_elementary),
  other_private = build_msm("other_private_dummy", "ipw_other_private", df_elementary),
  teacher_training = build_msm("teacher_training_dummy", "ipw_teacher_training", df_elementary)
)
## 結果の表示
for (name in names(msm_models_schoolcareer_elementary)) {
  cat("\nMSM結果:", name, "\n")
  print(summary(msm_models_schoolcareer_elementary[[name]]))
}

#------------------------------------------------------------
#分析: IP Weighting for Marginal Structural Models(学校歴の影響，中学校受験者)
#------------------------------------------------------------
#A（学校歴）→Y（合否），V（ジェンダー），L（中3時の成績，世代）
## 中学校受験者のみを選択
df_juniorhigh <- selected_df %>%
  filter(q1_2_recoded %in% c(0, 1))
## 対象集団のサイズを確認
cat("対象集団のサイズ:", nrow(df_juniorhigh), "\n")
## 傾向スコアの計算関数
calculate_ps <- function(treatment, df) {
  formula <- as.formula(paste(treatment, "~ gender_dummy + q7_recoded + birth_year"))
  glm(formula, data = df, family = binomial())
}
## 各学校歴変数の傾向スコアを計算
ps_models <- list(
  national_public = calculate_ps("national_public_dummy", df_juniorhigh),
  top_private = calculate_ps("top_private_dummy", df_juniorhigh),
  other_private = calculate_ps("other_private_dummy", df_juniorhigh),
  teacher_training = calculate_ps("teacher_training_dummy", df_juniorhigh)
)
## 傾向スコアをデータフレームに追加
df_juniorhigh <- df_juniorhigh %>%
  mutate(
    ps_national_public = predict(ps_models$national_public, type = "response"),
    ps_top_private = predict(ps_models$top_private, type = "response"),
    ps_other_private = predict(ps_models$other_private, type = "response"),
    ps_teacher_training = predict(ps_models$teacher_training, type = "response")
  )
## IP Weightingの計算
df_juniorhigh <- df_juniorhigh %>%
  mutate(
    ipw_national_public = 1 / ifelse(national_public_dummy == 1, ps_national_public, 1 - ps_national_public),
    ipw_top_private = 1 / ifelse(top_private_dummy == 1, ps_top_private, 1 - ps_top_private),
    ipw_other_private = 1 / ifelse(other_private_dummy == 1, ps_other_private, 1 - ps_other_private),
    ipw_teacher_training = 1 / ifelse(teacher_training_dummy == 1, ps_teacher_training, 1 - ps_teacher_training)
  )
## IP Weightingの要約統計量を確認
summary(df_juniorhigh[, c("ipw_national_public", "ipw_top_private", "ipw_other_private", "ipw_teacher_training")])
## MSMの構築関数
build_msm <- function(treatment, ipw, df) {
  formula <- as.formula(paste("q1_2_recoded ~", treatment, "* gender_dummy"))
  design <- svydesign(ids = ~1, weights = df[[ipw]], data = df)
  svyglm(formula, design = design, family = quasibinomial())
}
## 各学校歴変数に対してMSMを構築
msm_models_schoolcareer_juniorhigh <- list(
  national_public = build_msm("national_public_dummy", "ipw_national_public", df_juniorhigh),
  top_private = build_msm("top_private_dummy", "ipw_top_private", df_juniorhigh),
  other_private = build_msm("other_private_dummy", "ipw_other_private", df_juniorhigh),
  teacher_training = build_msm("teacher_training_dummy", "ipw_teacher_training", df_juniorhigh)
)
## 結果の表示
for (name in names(msm_models_schoolcareer_juniorhigh)) {
  cat("\nMSM結果:", name, "\n")
  print(summary(msm_models_schoolcareer_juniorhigh[[name]]))
}

#------------------------------------------------------------
#分析: IP Weighting for Marginal Structural Models(学校歴の影響，高等学校受験者)
#------------------------------------------------------------
#A（学校歴）→Y（合否），V（ジェンダー），L（中3時の成績，世代）
## 高校受験者のみを選択
df_highschool <- selected_df %>%
  filter(q1_3_recoded %in% c(0, 1))
## 対象集団のサイズを確認
cat("対象集団のサイズ:", nrow(df_highschool), "\n")
# 傾向スコアの計算関数
calculate_ps <- function(treatment, df) {
  formula <- as.formula(paste(treatment, "~ gender_dummy + q7_recoded + birth_year"))
  glm(formula, data = df, family = binomial())
}
## 各学校歴変数の傾向スコアを計算
ps_models <- list(
  national_public = calculate_ps("national_public_dummy", df_highschool),
  top_private = calculate_ps("top_private_dummy", df_highschool),
  other_private = calculate_ps("other_private_dummy", df_highschool),
  teacher_training = calculate_ps("teacher_training_dummy", df_highschool)
)
## 傾向スコアをデータフレームに追加
df_highschool <- df_highschool %>%
  mutate(
    ps_national_public = predict(ps_models$national_public, type = "response"),
    ps_top_private = predict(ps_models$top_private, type = "response"),
    ps_other_private = predict(ps_models$other_private, type = "response"),
    ps_teacher_training = predict(ps_models$teacher_training, type = "response")
  )
## IP Weightingの計算
df_highschool <- df_highschool %>%
  mutate(
    ipw_national_public = 1 / ifelse(national_public_dummy == 1, ps_national_public, 1 - ps_national_public),
    ipw_top_private = 1 / ifelse(top_private_dummy == 1, ps_top_private, 1 - ps_top_private),
    ipw_other_private = 1 / ifelse(other_private_dummy == 1, ps_other_private, 1 - ps_other_private),
    ipw_teacher_training = 1 / ifelse(teacher_training_dummy == 1, ps_teacher_training, 1 - ps_teacher_training)
  )
## IP Weightingの要約統計量を確認
summary(df_highschool[, c("ipw_national_public", "ipw_top_private", "ipw_other_private", "ipw_teacher_training")])
## MSMの構築関数
build_msm <- function(treatment, ipw, df) {
  formula <- as.formula(paste("q1_3_recoded ~", treatment, "* gender_dummy"))
  design <- svydesign(ids = ~1, weights = df[[ipw]], data = df)
  svyglm(formula, design = design, family = quasibinomial())
}
## 各学校歴変数に対してMSMを構築
msm_models_schoolcareer_highschool <- list(
  national_public = build_msm("national_public_dummy", "ipw_national_public", df_highschool),
  top_private = build_msm("top_private_dummy", "ipw_top_private", df_highschool),
  other_private = build_msm("other_private_dummy", "ipw_other_private", df_highschool),
  teacher_training = build_msm("teacher_training_dummy", "ipw_teacher_training", df_highschool)
)
## 結果の表示
for (name in names(msm_models_schoolcareer_highschool)) {
  cat("\nMSM結果:", name, "\n")
  print(summary(msm_models_schoolcareer_highschool[[name]]))
}

#------------------------------------------------------------
#分析: IP Weighting for Marginal Structural Models(学生生活の影響，小学校受験者)
#------------------------------------------------------------
#A（学生生活）→Y（合否），V（ジェンダー），L（中3時の成績，世代）
## 小学校受験者のみを選択
df_elementary <- selected_df %>%
  filter(q1_1_recoded %in% c(0, 1))
## 変数の分布を確認
summary(df_elementary[c("q11_9_recoded", "gender_dummy", "q7_recoded", "birth_year", "q1_1_recoded")])
## q11_9_recodedの分布を視覚化
hist(df_elementary$q11_9_recoded, main = "Distribution of q11_9_recoded", xlab = "q11_9_recoded")
## 対象集団のサイズを確認
cat("対象集団のサイズ:", nrow(df_elementary), "\n")
## 線形回帰モデルを使用して予測値を計算
ps_model <- lm(q11_9_recoded ~ gender_dummy + q7_recoded + birth_year, data = df_elementary)
## 予測値をデータフレームに追加
df_elementary$predicted_q11_9 <- predict(ps_model)
## 予測値の分布を確認
summary(df_elementary$predicted_q11_9)
hist(df_elementary$predicted_q11_9, main = "Distribution of Predicted q11_9_recoded", xlab = "Predicted Value")
## IP Weightingの計算
df_elementary <- df_elementary %>%
  mutate(
    residual = q11_9_recoded - predicted_q11_9,
    sd_residual = sd(residual),
    density = dnorm(residual, mean = 0, sd = sd_residual),
    ipw = 1 / density
  )
## IP Weightingの要約統計量を確認
summary(df_elementary$ipw)
## 極端な重みをトリミング
quantiles <- quantile(df_elementary$ipw, probs = c(0.01, 0.99))
df_elementary$ipw_trimmed <- pmin(pmax(df_elementary$ipw, quantiles[1]), quantiles[2])
## トリミング後の重みの分布を確認
summary(df_elementary$ipw_trimmed)
## MSMの構築
design_elementary <- svydesign(ids = ~1, weights = ~ipw_trimmed, data = df_elementary)
msm_model_studentlife_elementary <- svyglm(q1_1_recoded ~ q11_9_recoded * gender_dummy, 
                    design = design_elementary, family = quasibinomial())
## 結果の表示
summary(msm_model_studentlife_elementary)
## オッズ比の計算（連続変数の場合、1単位の変化に対するオッズ比）
exp(coef(msm_model_studentlife_elementary))
exp(confint(msm_model_studentlife_elementary))

#------------------------------------------------------------
#分析: IP Weighting for Marginal Structural Models(学生生活，中学校受験者)
#------------------------------------------------------------
#A（学生生活）→Y（合否），V（ジェンダー），L（中3時の成績，世代）
## 中学校受験者のみを選択
df_juniorhigh <- selected_df %>%
  filter(q1_2_recoded %in% c(0, 1))
## 変数の分布を確認
summary(df_juniorhigh[c("q11_9_recoded", "gender_dummy", "q7_recoded", "birth_year", "q1_2_recoded")])
## q11_9_recodedの分布を視覚化
hist(df_juniorhigh$q11_9_recoded, main = "Distribution of q11_9_recoded (Junior High)", xlab = "q11_9_recoded")
## 対象集団のサイズを確認
cat("対象集団のサイズ（中学校受験者）:", nrow(df_juniorhigh), "\n")
## 線形回帰モデルを使用して予測値を計算
ps_model_jh <- lm(q11_9_recoded ~ gender_dummy + q7_recoded + birth_year, data = df_juniorhigh)
## 予測値をデータフレームに追加
df_juniorhigh$predicted_q11_9 <- predict(ps_model_jh)
## 予測値の分布を確認
summary(df_juniorhigh$predicted_q11_9)
hist(df_juniorhigh$predicted_q11_9, main = "Distribution of Predicted q11_9_recoded (Junior High)", xlab = "Predicted Value")
## IP Weightingの計算
df_juniorhigh <- df_juniorhigh %>%
  mutate(
    residual = q11_9_recoded - predicted_q11_9,
    sd_residual = sd(residual),
    density = dnorm(residual, mean = 0, sd = sd_residual),
    ipw = 1 / density
  )
## IP Weightingの要約統計量を確認
summary(df_juniorhigh$ipw)
## 極端な重みをトリミング
quantiles <- quantile(df_juniorhigh$ipw, probs = c(0.01, 0.99))
df_juniorhigh$ipw_trimmed <- pmin(pmax(df_juniorhigh$ipw, quantiles[1]), quantiles[2])
## トリミング後の重みの分布を確認
summary(df_juniorhigh$ipw_trimmed)
## MSMの構築（中学校）
design_juniorhigh <- svydesign(ids = ~1, weights = ~ipw_trimmed, data = df_juniorhigh)
msm_model_studentlife_juniorhigh <- svyglm(q1_2_recoded ~ q11_9_recoded * gender_dummy, 
                       design = design_juniorhigh, family = quasibinomial())
## 結果の表示
summary(msm_model_studentlife_juniorhigh)
## オッズ比の計算
exp(coef(msm_model_studentlife_juniorhigh))
exp(confint(msm_model_studentlife_juniorhigh))

#------------------------------------------------------------
#分析: IP Weighting for Marginal Structural Models(学生生活，高校受験者)
#------------------------------------------------------------
#A（学生生活）→Y（合否），V（ジェンダー），L（中3時の成績，世代）
## 高校受験者のみを選択
df_highschool <- selected_df %>%
  filter(q1_3_recoded %in% c(0, 1))
## 変数の分布を確認
summary(df_highschool[c("q11_9_recoded", "gender_dummy", "q7_recoded", "birth_year", "q1_3_recoded")])
## q11_9_recodedの分布を視覚化
hist(df_highschool$q11_9_recoded, main = "Distribution of q11_9_recoded (High School)", xlab = "q11_9_recoded")
## 対象集団のサイズを確認
cat("対象集団のサイズ（高校受験者）:", nrow(df_highschool), "\n")
## 線形回帰モデルを使用して予測値を計算
ps_model_hs <- lm(q11_9_recoded ~ gender_dummy + q7_recoded + birth_year, data = df_highschool)
## 予測値をデータフレームに追加
df_highschool$predicted_q11_9 <- predict(ps_model_hs)
## 予測値の分布を確認
summary(df_highschool$predicted_q11_9)
hist(df_highschool$predicted_q11_9, main = "Distribution of Predicted q11_9_recoded (High School)", xlab = "Predicted Value")
## IP Weightingの計算
df_highschool <- df_highschool %>%
  mutate(
    residual = q11_9_recoded - predicted_q11_9,
    sd_residual = sd(residual),
    density = dnorm(residual, mean = 0, sd = sd_residual),
    ipw = 1 / density
  )
## IP Weightingの要約統計量を確認
summary(df_highschool$ipw)
## 極端な重みをトリミング
quantiles <- quantile(df_highschool$ipw, probs = c(0.01, 0.99))
df_highschool$ipw_trimmed <- pmin(pmax(df_highschool$ipw, quantiles[1]), quantiles[2])
## トリミング後の重みの分布を確認
summary(df_highschool$ipw_trimmed)
## MSMの構築（高校）
design_highschool <- svydesign(ids = ~1, weights = ~ipw_trimmed, data = df_highschool)
msm_model_studentlife_highschool <- svyglm(q1_3_recoded ~ q11_9_recoded * gender_dummy, 
                       design = design_highschool, family = quasibinomial())
##結果の表示
summary(msm_model_studentlife_highschool)
##オッズ比の計算
exp(coef(msm_model_studentlife_highschool))
exp(confint(msm_model_studentlife_highschool))

#------------------------------------------------------------
#MSM結果のグラフ化(学校歴の効果)
#------------------------------------------------------------
# 学校歴の効果を抽出する関数
extract_schoolcareer_effects <- function(msm_models, school_type) {
  results <- data.frame()
  for (name in names(msm_models)) {
    coef <- coef(msm_models[[name]])
    ci <- confint(msm_models[[name]])
    main_effect_index <- 2
    interaction_index <- grep(":", names(coef))
    
    results <- rbind(results, data.frame(
      school = school_type,
      variable = name,
      effect_type = c("主効果", "効果修飾"),
      estimate = c(coef[main_effect_index], coef[interaction_index]),
      lower = c(ci[main_effect_index, 1], ci[interaction_index, 1]),
      upper = c(ci[main_effect_index, 2], ci[interaction_index, 2])
    ))
  }
  return(results)
}

# 各学校段階の結果を収集
schoolcareer_effects_elementary <- extract_schoolcareer_effects(msm_models_schoolcareer_elementary, "小学校")
schoolcareer_effects_juniorhigh <- extract_schoolcareer_effects(msm_models_schoolcareer_juniorhigh, "中学校")
schoolcareer_effects_highschool <- extract_schoolcareer_effects(msm_models_schoolcareer_highschool, "高等学校")

# 変数の順序と日本語ラベルを指定（順序変更）
variable_order <- c("national_public", "top_private", "other_private", "teacher_training")
variable_labels <- c(
  "national_public" = "国公立大学",
  "top_private" = "難関私立大学",
  "other_private" = "その他の私立大学",
  "teacher_training" = "教員養成課程"
)

# グラフ作成関数
create_schoolcareer_plot <- function(data) {
  ggplot(data, aes(x = factor(variable, levels = rev(variable_order), labels = rev(variable_labels[variable_order])), 
                   y = estimate, shape = effect_type, linetype = effect_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5)) +
    labs(x = "出身大学のタイプ", y = "推定値と95%信頼区間", shape = "効果タイプ", linetype = "効果タイプ") +
    theme_minimal() +
    scale_shape_manual(values = c(16, 17)) +  # 塗りつぶした丸と三角
    scale_linetype_manual(values = c("solid", "dashed")) +  # 実線と破線
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray") +
    coord_flip()
}

# 各学校段階のグラフを作成
plot_elementary <- create_schoolcareer_plot(schoolcareer_effects_elementary)
plot_juniorhigh <- create_schoolcareer_plot(schoolcareer_effects_juniorhigh)
plot_highschool <- create_schoolcareer_plot(schoolcareer_effects_highschool)

# グラフを表示
print(plot_elementary)
print(plot_juniorhigh)
print(plot_highschool)

#------------------------------------------------------------
#MSM結果のグラフ化(学生生活の効果)
#------------------------------------------------------------
# 学生生活の効果を抽出する関数
extract_studentlife_effects <- function(msm_model, school_type) {
  coef <- coef(msm_model)
  ci <- confint(msm_model)
  
  results <- data.frame(
    school = school_type,
    variable = c("教員採用試験対策", "教員採用試験対策"),
    effect_type = c("主効果", "効果修飾"),
    estimate = c(coef["q11_9_recoded"], coef["q11_9_recoded:gender_dummy"]),
    lower = c(ci["q11_9_recoded", 1], ci["q11_9_recoded:gender_dummy", 1]),
    upper = c(ci["q11_9_recoded", 2], ci["q11_9_recoded:gender_dummy", 2])
  )
  
  return(results)
}

# 各学校段階の結果を収集
studentlife_effects_elementary <- extract_studentlife_effects(msm_model_studentlife_elementary, "小学校")
studentlife_effects_juniorhigh <- extract_studentlife_effects(msm_model_studentlife_juniorhigh, "中学校")
studentlife_effects_highschool <- extract_studentlife_effects(msm_model_studentlife_highschool, "高等学校")

# すべての結果を1つのデータフレームに結合
all_studentlife_effects <- rbind(
  studentlife_effects_elementary,
  studentlife_effects_juniorhigh,
  studentlife_effects_highschool
)

# グラフ作成関数
create_studentlife_plot <- function(data) {
  ggplot(data, aes(x = factor(school, levels = c("小学校", "中学校", "高等学校")), y = estimate, shape = effect_type, linetype = effect_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5)) +
    labs(x = "受験した学校種別", y = "推定値と95%信頼区間", shape = "効果タイプ", linetype = "効果タイプ") +
    theme_minimal() +
    scale_shape_manual(values = c(16, 17)) +  # 塗りつぶした丸と三角
    scale_linetype_manual(values = c("solid", "dashed")) +  # 実線と破線
    theme(legend.position = "bottom",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
    coord_flip()
}

# グラフを作成
plot_studentlife <- create_studentlife_plot(all_studentlife_effects)

# グラフを表示
print(plot_studentlife)

#------------------------------------------------------------
# 感度分析
#------------------------------------------------------------
# 感度分析を実行する関数
print_sensitivity_results <- function(results, title) {
  cat("\n", title, "\n")
  for (school in names(results)) {
    cat("\n", school, ":\n")
    school_result <- results[[school]]
    
    if (is.list(school_result) && length(school_result) > 0) {
      if (is.list(school_result[[1]]) && !is.null(school_result[[1]]$main)) {
        # For sensitivity_schoolcareer structure
        for (i in seq_along(school_result)) {
          cat("  Variable", i, ":\n")
          print_evalue_result(school_result[[i]])
        }
      } else {
        # For sensitivity_studentlife structure
        print_evalue_result(school_result)
      }
    } else {
      cat("  No results available or unexpected structure\n")
      print(school_result)
    }
  }
}

print_evalue_result <- function(result) {
  if (is.list(result)) {
    if (!is.null(result$main)) {
      cat("    Main effect:\n")
      cat("      Point estimate E-value:", result$main["E-values", "point"], "\n")
      cat("      CI E-values:", result$main["E-values", "lower"], "-", result$main["E-values", "upper"], "\n")
    }
    if (!is.null(result$interaction)) {
      cat("    Interaction effect:\n")
      cat("      Point estimate E-value:", result$interaction["E-values", "point"], "\n")
      cat("      CI E-values:", result$interaction["E-values", "lower"], "-", result$interaction["E-values", "upper"], "\n")
    }
  } else {
    cat("    Unexpected result structure\n")
    print(result)
  }
}

# 学校歴の影響に関する感度分析
sensitivity_schoolcareer <- list(
  elementary = lapply(c("national_public_dummy", "top_private_dummy", "other_private_dummy", "teacher_training_dummy"), 
                      function(name) perform_sensitivity_analysis(msm_models_schoolcareer_elementary[[gsub("_dummy$", "", name)]], name)),
  juniorhigh = lapply(c("national_public_dummy", "top_private_dummy", "other_private_dummy", "teacher_training_dummy"), 
                      function(name) perform_sensitivity_analysis(msm_models_schoolcareer_juniorhigh[[gsub("_dummy$", "", name)]], name)),
  highschool = lapply(c("national_public_dummy", "top_private_dummy", "other_private_dummy", "teacher_training_dummy"), 
                      function(name) perform_sensitivity_analysis(msm_models_schoolcareer_highschool[[gsub("_dummy$", "", name)]], name))
)

# 学生生活の影響に関する感度分析
sensitivity_studentlife <- list(
  elementary = perform_sensitivity_analysis(msm_model_studentlife_elementary, "q11_9_recoded"),
  juniorhigh = perform_sensitivity_analysis(msm_model_studentlife_juniorhigh, "q11_9_recoded"),
  highschool = perform_sensitivity_analysis(msm_model_studentlife_highschool, "q11_9_recoded")
)

# 結果の表示
print_sensitivity_results(sensitivity_schoolcareer, "学校歴の影響に関する感度分析")
print_sensitivity_results(sensitivity_studentlife, "学生生活の影響に関する感度分析")