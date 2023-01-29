install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages('randomForest')


library('randomForest')
library(e1071)
library(caTools)
library(caret)
library(corrplot)
library(ggplot2)
library(dplyr)

#1. zadatak odabir podskupa, izbaceno ID, X na pocetku nez ni sta predstavlja,
potpuni_skup <- read.csv("https://raw.githubusercontent.com/Cpt-Shime/RollingStones_dataset/main/rolling_stones_spotify.csv")

skup <- potpuni_skup[, - c(1,7)]
skup <- na.omit(skup)

#Provjera za nedostajce elemente
missing_values = colSums(is.na(skup))
missing_values

#izbacivanje duplih iz skupa
skup <- skup[!duplicated(skup$id),]


# Stvaranje nove varijable za popularnost pjesme kako bi ih mogli klasificirati i fakotorizirati

skup <- skup %>% mutate(pop_class = as.numeric(case_when(
    ((popularity > 0) & (popularity < 33.33)) ~ "1",
    ((popularity >= 33.33) & (popularity < 66.66))~ "2",
    TRUE ~ "3"))
  )

summary(skup)


#podjela skupa za test i trening
indeksi <- createDataPartition(skup$pop_class, p = 0.7, list = FALSE)
skup_tren <- skup[indeksi, ]
skup_test <- skup[-indeksi, ]

set.seed(10) 
#naivni bayes
NB_model <- naiveBayes(pop_class ~ ., data = skup_tren)
predikcije<-predict(NB_model, newdata = skup_test)

confusionMatrix(factor(skup_test$pop_class), predikcije)


#randomforest
corrplot(cor(skup[6:16]), method="number")

ggplot(skup,aes(x=pop_class,fill=factor(pop_class))) +
  geom_bar(stat = "count",position = "dodge") +
  ggtitle("Distribucija popularnosti pjesma (pop_class)")


#skup$pop_class <- as.factor(ifelse(skup$pop_class == 3,"Popularna", skup$pop_class))
#skup$pop_class
#skup$pop_class <- as.factor(ifelse(skup$pop_class  ==2,"Srednja", "Nepopularna"))

skup$pop_class
kontrola <- trainControl(method = "repeatedcv",
                         number = 3,
                         repeats = 1)

model <- randomForest(pop_class ~ .,
               data = skup_tren,importance=TRUE)

model

predikcije <- predict(model, skup_test)
predikcije
predikcije <- ifelse(predikcije >2.5, 3, predikcije)
predikcije <- ifelse((predikcije <=2.5 & predikcije >1.5), 2, 1)

predikcije <-as.factor(predikcije)
#jer niti jedna predikcija nema level 3 dodajemo ga sami
levels(predikcije) <- c(levels(predikcije), 3)

confusionMatrix(predikcije, factor(skup_test$pop_class))
