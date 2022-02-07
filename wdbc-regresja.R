# autor: Michał Gołębiewski

library(tidyverse) 
library(readr)
library(caret)
library(dplyr)
library(ROCR)
library(corrplot)
library(ggplot2)
library(corrgram)
library(car)



# ramka danych
wdbc <- read.csv("wdbc.csv", na.strings = "NA", stringsAsFactors = TRUE)
# DANE
# poszukiwanie brakujących rekordów
anyNA.data.frame(wdbc) #sprawdzenie brakujących rekordów
colSums(is.na(wdbc)) #ponowne sprawdzenie braków w kolumnach
raczek <- wdbc[,-(1)]
head(raczek)
# uporządkowanie kolumny Diagnosis
raczek$Diagnosis <- factor(ifelse(raczek$Diagnosis=="B","Benign","Malignant"))
head(raczek)
table(raczek$Diagnosis)
str(raczek)

# KORELACJA
# sprawdzenie korelacji
kore <- cor(raczek[,-1])
corrplot(kore, order = "hclust", tl.cex = 1, addrect = 8)
# usunięcie kolumn o korelacji powyżej 0,7
wyskor <- findCorrelation(kore, cutoff = .70)
print(wyskor)  # kolumna nr 1 - Diagnosis też się załapała
rak_cz <- raczek[, -wyskor]
rak_cz$Diagnosis <- raczek$Diagnosis
dim(rak_cz) # zostało 12 kolumn, w tym Diagnosis

## usuwam wszystkie outliery posługując się wynikiem standardowym: z-score
rak_czno <- raczek[, -wyskor]
z_scores <- as.data.frame(sapply(rak_czno, function(x) abs((x-mean(x))/sd(x))))
summary(z_scores)
rak_czno <- rak_cz[!rowSums(z_scores>3),]
dim(rak_czno) # usunięto 41 rekordów

# zbiory danych: TEST i TRAIN
# utworzenie dwóch zbiorów danych:
#       na zbiorze pełnym i zbiorze obranym z korelacji i outlierów
a <- createDataPartition(raczek$Diagnosis, p = 0.75, list=FALSE)
train <- raczek[a,]
test <- raczek[-a,]
dim(train)
dim(test)
b <- createDataPartition(rak_czno$Diagnosis,p=0.75,list=FALSE)
train_czno <- rak_czno[b,]
test_czno <- rak_czno[-b,]
dim(train_czno)
dim(test_czno)


# REGRESJA 
#wstępny model
rakreg0 <- glm(Diagnosis~.,data=train_czno,family=binomial)
summary(rakreg0)

rakreg <- glm(Diagnosis~ V24 + V29 + V11,data=train_czno,family=binomial)
summary(rakreg)
plot(rakreg)

#sprawdzenie
rak_spr <- predict(rakreg, test_czno, type = 'response') %>% bind_cols(test_czno %>% select(Diagnosis), preds= .)
rak_spr$preds[which(rak_spr$preds>0.5)] <- 'Malignant'
rak_spr$preds[which(rak_spr$preds!='Malignant')] <- 'Benign'
rak_spr$preds <- as.factor(rak_spr$preds)
str(rak_spr)
confusionMatrix(rak_spr$preds, rak_spr$Diagnosis)

