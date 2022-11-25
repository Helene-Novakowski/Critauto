#vider la mémoire

rm(list = ls())


#Library
library("DescTools")# Pour la visualisation des données manquantes
library(dplyr)
library(tidyverse) #Pour ggplot
library(nnet)
library('Hmisc') #pour describe

library(MASS)




# chemin vers le répertoire courant où est placé le fichier de données
# setwd("C:/Users/ousma/Desktop/M2SEP/Gestion de Projets digitalisés/Projet CO2")


## Importation du fichier
CO2 <- read.csv2("bdd_CO2.csv", stringsAsFactors=TRUE)






####################################################################################################################################
#### Correction de la base####
####################################################################################################################################

#Faute de frappe COMBISPACE
# unique(CO2$Carrosserie)
# summary(CO2$Carrosserie)
CO2$Carrosserie[CO2$Carrosserie == "COMBISPCACE"] <- "COMBISPACE"
CO2 %>% 
  filter(!(Carrosserie == "COMBISPCACE")) -> CO2


#NA entre hc nox et hcnox
CO2 %>% 
  mutate(hc_nox = ifelse(!is.na(hc) & !is.na(nox),hc + nox,hcnox)) %>% 
  dplyr::select(-hcnox, -hc, -nox) -> CO2




#####Modif sur la base#####
CO2 %>% 
  mutate(var_co2 = ifelse(co2 < 140,"faible",ifelse(co2 < 200, "moyenne","forte"))) %>% #voir critère
  dplyr::select(-co2, -dscom, -hybride,-date_maj, -conso_mixte, -champ_v9, -lib_mod_doss, -lib_mod, -cnit, -tvv) -> CO2_mod



na.omit(CO2_mod) -> data_co2  #On retire toutes les données manquantes !
as.factor(data_co2$var_co2) -> data_co2$var_co2



###Echantillonnage sur les moyennes et fortes####

set.seed(12345) #Fixer le générateur

forte <- sample_n(data.frame(subset(data_co2, var_co2 == "forte")), 3613)
forte
nrow(forte)
moyenne <- sample_n(data.frame(subset(data_co2, var_co2 == "moyenne")), 3613)
moyenne
nrow(moyenne)
faible <- data.frame(subset(data_co2, var_co2 == "faible"))
faible
nrow(faible)

Co2data <- rbind(forte,moyenne,faible)

Co2data1 <- Co2data[sample(nrow(Co2data)),]
view(Co2data1) 


summary(Co2data1)
str(Co2data1)





#### Forêts aléatoires ####
set.seed(5678)
perm <- sample(10839,8000)
app <- Co2data1[perm,] #échantillon d'apprentissage
valid <- Co2data1[-perm,] #échantillon de validation 


## 2. Construire et analyser une forêt aléatoire
library(randomForest)
set.seed(1234)
foret <- randomForest(var_co2~., data=app) 
foret  #ntree = 500 par défaut (nombre d'arbres, ou des échantillon bootstrap)
#mtry = 7 (par défaut = partie entière de la racine carré du nombre de variables explicatives, en classif.) 
#erreur de classif estimée par la méthode OOB (Out Of Bag)

## 3. Sélectionner les paramètres de la forêt
plot(foret)
tail(foret$err.rate) # les 6 dernieres erreurs de classif, peut de différence entre ces valeurs, donc le choix ntree = 500 est suffisant. 
grille.mtry <- data.frame(mtry=seq(from=1,to=15, by = 3)) #grille des choix du paramètre mtry
library(caret)
ctrl <- trainControl(method="oob") #method oob : Out Of Bag
set.seed(1234)
sel.mtry <- train(var_co2~., data=app, method="rf", trControl=ctrl, tuneGrid=grille.mtry)
sel.mtry
sel.mtry$bestTune
plot(sel.mtry)
####La forêt Optimale

set.seed(1234)
foret_opt <- randomForest(var_co2~., data=app, mtry=as.numeric(sel.mtry$bestTune)) 
foret_opt
plot(foret_opt)

tail(foret_opt$err.rate)


## 4. Faire de la prévision

set.seed(1234)
foret_opt <- randomForest(var_co2~., data=app, mtry=as.numeric(sel.mtry$bestTune))

prev.valid <- predict(foret_opt, newdata=valid)
prev.valid[1:30]

prob.valid <- predict(foret_opt, newdata=valid, type="prob")

prob.valid[1:30,]

## 5. Estimer les performances de la forêt
#on compare les deux forêts mtry = 7 (la foret par defaut) et mtry = 13 (la foret selectionnée ci-dessus) 
set.seed(1234)
foret <- randomForest(var_co2~., data=app, xtest=valid[,-15], ytest=valid[,15], keep.forest=TRUE) #mtry = 7 
set.seed(1234)
foret_opt <- randomForest(var_co2~., data=app, mtry=as.numeric(sel.mtry$bestTune), 
                          xtest=valid[,-15], ytest=valid[,15], keep.forest=TRUE)
foret
foret_opt

###library(pROC) (à voir)
'prev <- predict(foret, newdata=valid, type="prob")[,2]
roc <- multiclass.roc(response= valid$var_co2, predictor = prev)
prev_opt <- predict(foret_opt, newdata=valid, type="prob")[,2]
roc_opt <- roc(valid$type, prev_opt)
plot(roc, print.auc=TRUE, print.auc.cex=0.5, print.auc.x=0.4, print.auc.y=0.3)
plot(roc_opt, add=TRUE, col="red", print.auc=TRUE, print.auc.cex=0.5, 
     print.auc.col="red", print.auc.x=0.4, print.auc.y=0.2)'####


sensitivity(valid$var_co2, predicted)








nrow(valid)

###Tests Hélène
valid$predicted <- predict(foret_opt, valid)
View(valid)

actual <- valid$var_co2
pred <- valid$predi

library(caret)
#F score
table(actual, pred)
confusionMatrix(pred, actual, mode = "everything", positive="1")


1-0.9954 #erreur de classif






