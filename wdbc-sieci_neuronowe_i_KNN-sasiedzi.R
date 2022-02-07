# autor: Michał Gołębiewski
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(mice)
library(pROC)
library(scipy)

wdbc <- read.csv("wdbc.csv", na.strings = "NA", stringsAsFactors = TRUE)
# jeśliby nie zastosować stringsAsFactors to potrzebne by było: wdbc$Diagnosis <- as.factor(wdbc$Diagnosis)
anyNA.data.frame(wdbc) #sprawdzenie brakujących rekordów
colSums(is.na(wdbc)) #ponowne sprawdzenie braków w kolumnach
str(wdbc)
summary(wdbc)
dim(wdbc)
ggplot(wdbc, aes(x = Diagnosis, fill = Diagnosis)) + geom_bar() +  theme_bw()
print(212/357) #sprawdzenie liczby M/B uznaję bazę za wystarczająco zbilansowaną
zm <- wdbc[,c(3:32)]
Diagnosis <- wdbc[,c(2)]
# przykładowe outliery
ggplot(wdbc, aes(y = V4)) + geom_boxplot() + theme_bw()

## usuwam wszystkie outliery posługując się wynikiem standardowym: z-score
z_scores <- as.data.frame(sapply(zm, function(zm) ((zm-mean(zm))/sd(zm))))
wdbc_no <- wdbc[!rowSums(z_scores>3),]
zm_no <- wdbc_no[,c(3:32)]
Diagnosis <- wdbc_no[,c(2)]

summary(wdbc_no)
print(163/333)
ggplot(wdbc_no, aes(x = Diagnosis, fill = Diagnosis)) + geom_bar() +  theme_bw()

# stworzenie funkcji normalizującej 
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# nowa ramka danych zawierająca znormalizowane predyktory (zmienne niezależne)
zm_no_norm <- as.data.frame(lapply(zm_no, normalize))
summary(zm_norm)

wdbc_no_norm <- cbind(Diagnosis, zm_no_norm)
summary(wdbc_no_norm)
str(wdbc_no_norm)

# zbiór treningowy (75%) i zbiór testowy (25%)
sample_size <- floor(0.75*nrow(wdbc_no_norm))
set.seed(1234)
  # losowy podział ramki danych
picked = sample(seq_len(nrow(wdbc_no_norm)),size = sample_size)
wdbc_n_train <- wdbc_no_norm[picked,]
wdbc_n_test  <- wdbc_no_norm[-picked,]

#instalujemy i dodajemy pakiet neuralnet i caret
install.packages("neuralnet")
library(neuralnet)
install.packages('caret')
library(caret)

#ustawiamy ziarno dla generatora liczb pseudolosowych
set.seed(12345)

#budujemy model oparty na ANN
paste0(names(zm), collapse = ' + ')
wdbc_model <- neuralnet(formula = Diagnosis ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30,
                            data = wdbc_n_train)

#oglądamy topologię sieci
plot(wdbc_model)

#sprawdzamy działanie modelu na zbiorze testowym
model_results <- compute(wdbc_model, wdbc_n_test[2:31])

#zapisujemy przewidziane wartości wytrzymałości betonu do zmiennej
predicted_diagnosis <- model_results$net.result

#sprawdzamy jakość modelu obliczając korelację pomiędzy przewidzianą a rzeczywistą 
print('korelacja ANN - 1 warstwa')
cor(as.integer(wdbc_n_test$Diagnosis), max.col(predicted_diagnosis))
pd1 <- as.factor(max.col(predicted_diagnosis))
levels(pd1) <- c('B', 'M')
confusionMatrix(pd1, wdbc_n_test$Diagnosis)


# model oparty na ANN z 5 warstwami ukrytymi
set.seed(12345)
wdbc_model2 <- neuralnet(formula = Diagnosis ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30,
                        data = wdbc_n_train, hidden = c(5,3))
plot(wdbc_model2)
model_results2 <- compute(wdbc_model2, wdbc_n_test[2:31])
predicted_diagnosis2 <- model_results2$net.result
print('korelacja ANN - 5 warstw')
cor(as.integer(wdbc_n_test$Diagnosis), max.col(predicted_diagnosis2))
pd2 <- as.factor(max.col(predicted_diagnosis2))
levels(pd2) <- c('B', 'M')
confusionMatrix(pd2, wdbc_n_test$Diagnosis)

install.packages("class")
library(class)
pred_knn_test <- knn(wdbc_n_train[2:31], wdbc_n_test[2:31], wdbc_n_train$Diagnosis, k = 3)
print('algorytm K-najbliższych sąsiadów')
confusionMatrix(pred_knn_test, wdbc_n_test$Diagnosis)
