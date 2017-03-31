library(dplyr)

window <- 5

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

# model <- lm(data = data_0, reten_on_index ~ 1 + reten_on_index_market)
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
# temp <- temp[c((13-window):(13+window)), ]
# result[j, 1] <- colnames(temp)[2]

result <- data.frame(name = rep_len(0, ((ncol(data) - 1) / 2)),
                     name = rep_len(0, ((ncol(data) - 1) / 2)))

j <- 1
for (i in c(paste0("data_",c(1:((ncol(data) - 1) / 2) - 1)))) {
        temp <- get(i)
        temp <- temp[c((13-window):(13+window)), ]
        model <- lm(data = temp, reten_on_index ~ 1 + reten_on_index_market)
        
        sigma_market <- var(temp$reten_on_index_market, na.rm = T)
        sigma_residual <- var(model$residuals, na.rm = T)
        sq_diff_return <- (temp$reten_on_index_market - mean(temp$reten_on_index_market, na.rm = T)) ** 2
        L_1 <- nrow(temp)
        sigma_ar <- sigma_residual + (1 / L_1) * (1 + sq_diff_return / sigma_market)
        
        temp[, 6] <- model$residuals / sigma_ar
        names(temp)[6] <- "strange_shit"
        
        # result <- data.frame(name = rep_len(0, ((ncol(data) - 1) / 2)),
        #                      name = rep_len(0, ((ncol(data) - 1) / 2)))
        
        result[j, 1] <- colnames(temp)[2]
        result[j, 2] <- mean(temp$strange_shit)
        
        j <- j + 1
        colnames(temp)
        
        assign(i, temp)
}
