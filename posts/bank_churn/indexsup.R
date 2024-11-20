path <- "C:/Users/brcpwmftomaz/OneDrive - Oregon Tool/Desktop/Uso Privado/Walassseee.github.io/posts/bank_churn/Bank Customer Churn Prediction.csv"

data <- read.csv2(path, sep = ",", dec = ".")

par(mfrow = c(2,2))

hist(data$credit_score,
     main = "Distribuição do Credit Score",
     xlab = "", ylab = "Distribuição")

barplot(table(data$country), 
        main = "Distribuição dos Países",
        xlab = "", ylab = "Distribuição")

barplot(table(data$gender), 
        main = "Distribuição dos Generos",
        xlab = "", ylab = "Distribuição")

hist(data$age,
     main = "Distribuição das Idades",
     xlab = "", ylab = "Distribuição")

par(mfrow = c(2,3))

barplot(table(data$tenure),
     main = "Distribuição das Fidelidades",
     xlab = "", ylab = "Distribuição")

hist(data$balance,
     main = "Distribuição dos Saldos",
     xlab = "", ylab = "Distribuição")

hist(data$estimated_salary,
     main = "Distribuição dos Salários",
     xlab = "", ylab = "Distribuição")

pie(prop.table(table(data$credit_card)),
    main = "Proporção de Cartões",
    xlab = "", ylab = "")

pie(prop.table(table(data$active_member)),
    main = "Proporção de Membros",
    xlab = "", ylab = "")

barplot(table(data$products_number),
        main = "Distribuição dos Produtos",
        xlab = "", ylab = "Distribuição")

cols <- c("credit_score","age","tenure","balance","products_number","estimated_salary")

summary(data[cols])

library(fastDummies)

dummy <- c("country", "gender")

data <- dummy_cols(data,
                   select_columns = dummy)


names(data)

reg_cols <- c("credit_score","age","tenure","balance","products_number",
              "credit_card","active_member","estimated_salary","country_France","country_Germany",
              "gender_Male", "churn")

reg_data <- data[reg_cols]

logit <- glm(churn ~ ., data = reg_data, family = binomial(link='probit'))

summary(logit)












