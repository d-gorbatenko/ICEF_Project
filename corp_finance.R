library(forecast)
library(dplyr)

window <- 5

result <- data.frame(name = rep_len(0, ((ncol(data) - 1) / 2)),
                     w0 = rep_len(0, ((ncol(data) - 1) / 2)),
                     w1 = rep_len(0, ((ncol(data) - 1) / 2)),
                     w2 = rep_len(0, ((ncol(data) - 1) / 2)),
                     w3 = rep_len(0, ((ncol(data) - 1) / 2)),
                     w4 = rep_len(0, ((ncol(data) - 1) / 2)),
                     w5 = rep_len(0, ((ncol(data) - 1) / 2)))

options(stringsAsFactors = F)
data <- read.csv("data/Workbook1.csv")

for (w in c(0:5)) {

        window <- w
        
# parse data into different files
for (i in c(1:((ncol(data) - 1) / 2) - 1)) {
        
        assign(paste0("data_", i), data[, c(1, c(2, 3) + 2 * i)])
}

c(paste0("data_",c(1:((ncol(data) - 1) / 2) - 1)))

# calculate returns
for (i in c(paste0("data_",c(1:((ncol(data) - 1) / 2) - 1)))) {
        temp <- get(i)
        temp[, 4] <- log(temp[, 2] / lag(temp[, 2]))
        names(temp)[4] <- "reten_on_index"
        temp[, 5] <- log(temp[, 3] / lag(temp[, 3]))
        names(temp)[5] <- "reten_on_index_market"
        assign(i, temp)
}

# run OLS regressions

# 
# 
# 
# sigma_market <- var(data_0$reten_on_index_market, na.rm = T)
# sigma_residual <- var(model$residuals, na.rm = T)
# sq_diff_return <- (data_0$reten_on_index_market - mean(data_0$reten_on_index_market, na.rm = T)) ** 2
# L_1 <- nrow(data_0)
# sigma_ar <- sigma_residual + (1 / L_1) * (1 + sq_diff_return / sigma_market)
# 
# model$residuals / sigma_ar[c(2:25)]
# 
# 
# data_0[c((13-window):(13+window)), ]
# 
# colnames(data_0)[2]
# 
# temp <- get("data_0")
# # выделяем окно
# temp.window <- temp[-c(13:(13+window)), ]
# temp.window <- temp[-1, ]
# # прогоняем регрессию
# model <- lm(data = temp.window, reten_on_index ~ 1 + reten_on_index_market)
# # считаем сигму маркета и среднее
# sigma_market <- var(temp.window$reten_on_index_market, na.rm = T)
# mu_market <- mean(temp.window$reten_on_index_market, na.rm = T)
# # считаем сигму остаткоы
# sigma_residual <- var(model$residuals, na.rm = T)
# # и кол-во наблюдений
# L_1 <- nrow(temp.window)
# # прогнозируем ретерны
# a <- forecast.lm(object = model, newdata = temp[c(13:(13+window)), ])
# # получаем остатки
# residuals <- temp[c(13:(13+window)), 4] - a$mean
# # получаем дисперсию остатков
# sigma_forecast <- sigma_residual + (1 / L_1) * (1 + ((temp[c(13:(13+window)), 5] - mu_market) ** 2) / sigma_market)
# # нормализуем наши остатки и считаем срднее
# temp.var <- mean(na.rm = T, residuals / sigma_forecast)
# 
# 
# 


# result <- data.frame(name = rep_len(0, ((ncol(data) - 1) / 2)),
#                      name = rep_len(0, ((ncol(data) - 1) / 2)))

j <- 1
for (i in c(paste0("data_",c(1:((ncol(data) - 1) / 2) - 1)))) {
        # выделяем окно
        temp <- get(i)
        temp.window <- temp[-c(13:(13+window)), ]
        temp.window <- temp[-1, ]
        # прогоняем регрессию
        model <- lm(data = temp.window, reten_on_index ~ 1 + reten_on_index_market)
        # считаем сигму маркета и среднее
        sigma_market <- var(temp.window$reten_on_index_market, na.rm = T)
        mu_market <- mean(temp.window$reten_on_index_market, na.rm = T)
        # считаем сигму остаткоы
        sigma_residual <- var(model$residuals, na.rm = T)
        # и кол-во наблюдений
        L_1 <- nrow(temp.window)
        # прогнозируем ретерны
        a <- forecast.lm(object = model, newdata = temp[c(13:(13+window)), ])
        # получаем остатки
        residuals <- temp[c(13:(13+window)), 4] - a$mean
        # получаем дисперсию остатков
        sigma_forecast <- sigma_residual + (1 / L_1) * (1 + ((temp[c(13:(13+window)), 5] - mu_market) ** 2) / sigma_market)
        # нормализуем наши остатки и считаем срднее
        temp.var <- mean(na.rm = T, residuals / sigma_forecast)
        
        result[j, 1] <- colnames(temp)[2]
        result[j, window + 2] <- temp.var
        
        j <- j + 1
        colnames(temp)
        
        assign(i, temp)
}}

result <- 
        result %>% 
        mutate(w0.pvalue = pnorm(abs(w0), lower.tail = F),
               w1.pvalue = pnorm(abs(w1), lower.tail = F),
               w2.pvalue = pnorm(abs(w2), lower.tail = F),
               w3.pvalue = pnorm(abs(w3), lower.tail = F),
               w4.pvalue = pnorm(abs(w4), lower.tail = F),
               w5.pvalue = pnorm(abs(w5), lower.tail = F))

result



