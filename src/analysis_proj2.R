#install.packages("PerformanceAnalytics")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path));

rm(list = ls());
library("PerformanceAnalytics");
load(file = "data/fosfor_data.Rdata");
Phosphorous$location = as.factor(Phosphorous$location)
Phosphorous
summary(Phosphorous)
levels(Phosphorous$location)

boxplot(yield ~ location, data = Phosphorous,
        ylab = "Harvest yield of barley [hkg/ha]", 
        xlab = "Field id",
        main = "Yield distributions for each field")


chart.Correlation(within(Phosphorous, rm(location)), histogram = F, 
                  method = "pearson")

K <- length(levels(Phosphorous$location))
dgt_test_errors <- rep(NA, K) 
oP_test_errors <- rep(NA, K)



#dgt_models <-rep(NA, K) 
#oP_models <- rep(NA, K)
for (k in 1:K) {
        i_P <- subset(Phosphorous, location != levels(Phosphorous$location)[k])
        k_model_dgt <- nls(yield ~ alfa * DGT/(beta + DGT) , data = i_P,
                            start = list(alfa = 80 , beta = 2))
        k_model_oP <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = i_P,
                           start = list(alfa = 80 , beta = 2))
        
        k_true <- subset(Phosphorous, location == levels(Phosphorous$location)[k])$yield
        
        
        dgt_test_errors[k] <-mean(
                (k_true[!is.na(k_true)] - 
        predict(k_model_dgt, newdata = subset(Phosphorous, location == levels(Phosphorous$location)[k]))[!is.na(k_true)]
        )^2 )
        
        oP_test_errors[k] <-mean(
                (k_true[!is.na(k_true)] - 
         predict(k_model_oP, newdata = subset(Phosphorous, location == levels(Phosphorous$location)[k]))[!is.na(k_true)]
        )^2 )

}
mean(dgt_test_errors)
mean(oP_test_errors)

par(mfrow = c(1,1))
qqnorm(dgt_test_errors-oP_test_errors)
qqline(dgt_test_errors-oP_test_errors)

#qqnorm(oP_test_errors)
#qqline(oP_test_errors)


t.test(dgt_test_errors, oP_test_errors, paired = T)

nl_model_dgt <- nls(yield ~ alfa * DGT/(beta + DGT) , data = Phosphorous,
                  start = list(alfa = 80 , beta = 2))
summary(nl_model_dgt)

nl_model_oP <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = Phosphorous,
                  start = list(alfa = 80 , beta = 2))
summary(nl_model_oP)



simple_model_dgt <- lm(yield ~ DGT, data = Phosphorous)
summary(simple_model_dgt)

simple_model_oP <- lm(yield ~ olsenP, data = Phosphorous)
summary(simple_model_oP)


model_dgt <- lm(yield ~ DGT+location, data = Phosphorous)
anova(model_dgt)
qqnorm(model_dgt$residuals)
qqline(model_dgt$residuals)

model_oP <- lm(yield ~ olsenP+location, data = Phosphorous)
anova(model_oP)
qqnorm(model_oP$residuals)
qqline(model_oP$residuals)

par(mfrow = c(1,1))

plot(Phosphorous$DGT, Phosphorous$yield,
     xlab = "DGT-measured bioavailable phosporous [µg/L]",
     ylab = "Harvest yield of barley [hkg/ha]",
     main = "Yield influence of bioavailable phosporous, measured using DGT ",
     cex = 1, col = Phosphorous$location
)
curve(coef(nl_model_dgt)[1]*x / (coef(nl_model_dgt)[2]+x), add = T, col = "red", lwd = 2, lty = "dashed", from = 0, to = 170)
for (k in 1:K) {
        i_P <- subset(Phosphorous, location != levels(Phosphorous$location)[k])
        
        k_model_dgt <- nls(yield ~ alfa * DGT/(beta + DGT) , data = i_P,
                           start = list(alfa = 80 , beta = 2))
        curve(coef(k_model_dgt)[1]*x / (coef(k_model_dgt)[2]+x), add = T, col = "grey", lty = "dashed", from = 0, to = 170)
        
        
}

legend("bottomright", c("Full (DGT, yield) Michaelis-Menten model", "Model for each fold in cross validaiton."),  col = c("red","grey"), lty = "dashed")


plot(Phosphorous$olsenP, Phosphorous$yield,
     xlab = "Olsen P-measured bioavailable phosporous [mg/100g]",
     ylab = "Harvest yield of barley [hkg/ha]",
     main = "Yield influence of bioavailable phosporous, measured using Olsen P",
     cex = 1, col = Phosphorous$location
)
curve(coef(nl_model_oP)[1]*x / (coef(nl_model_oP)[2]+x), add = T, col = "red", lwd = 2, lty = "dashed", from = 1, to = 10)

for (k in 1:K) {
        i_P <- subset(Phosphorous, location != levels(Phosphorous$location)[k])
        
        k_model_oP <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = i_P,
                          start = list(alfa = 80 , beta = 2))
        curve(coef(k_model_oP)[1]*x / (coef(k_model_oP)[2]+x), add = T, col = "grey", lty = "dashed", from = 0, to = 170)
        
        
}

legend("bottomright", c("Full (Olsen P, yield) Michaelis-Menten model", "Model for each fold in cross validaiton."),  col = c("red","grey"), lty = "dashed")





