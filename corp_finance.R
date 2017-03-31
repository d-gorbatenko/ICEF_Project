library(dplyr)

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
sqrt(var(model$residuals))
for (i in c(paste0("data_",c(1:((ncol(data) - 1) / 2) - 1)))) {
        temp <- get(i)
        model <- ls(data = temp, reten_on_index ~ 1 + reten_on_index_market)
        
        assign(i, temp)
}