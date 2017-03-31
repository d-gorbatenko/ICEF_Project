library(dplyr)
options(stringsAsFactors = F)
data <- read.csv("data/Workbook1.csv")

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
model <- lm(data = data_0, reten_on_index ~ 1 + reten_on_index_market)


sigma_market <- var(data_0$reten_on_index_market, na.rm = T)
sigma_residual <- var(model$residuals, na.rm = T)
sq_diff_return <- (data_0$reten_on_index_market - mean(data_0$reten_on_index_market, na.rm = T)) ** 2
L_1 <- nrow(data_0)
sigma_ar <- sigma_residual + (1 / L_1) * (1 + sq_diff_return / sigma_market)

window <- 5
data_0[c((13-window):(13+window)), ]

for (i in c(paste0("data_",c(1:((ncol(data) - 1) / 2) - 1)))) {
        temp <- get(i)
        temp <- temp[c((13-window):(13+window)), ]
        model <- ls(data = temp, reten_on_index ~ 1 + reten_on_index_market)
        
        sigma_market <- var(temp$reten_on_index_market, na.rm = T)
        sigma_residual <- var(model$residuals, na.rm = T)
        sq_diff_return <- (temp$reten_on_index_market - mean(temp$reten_on_index_market, na.rm = T)) ** 2
        L_1 <- nrow(temp)
        sigma_ar <- sigma_residual + (1 / L_1) * (1 + sq_diff_return / sigma_market)
        
        
        assign(i, temp)
}