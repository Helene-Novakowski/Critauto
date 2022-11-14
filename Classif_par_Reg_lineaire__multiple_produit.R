
####Entête####
##Fichier : Classif_par_Reg_lineaire__multiple_produit.R
##Desc : Toutes les instructions relatives au Projet/Produit
## Produit Critauto - M2 SEP - 2022-2023
##Date : 30/10/22
##Auteur : Hélène NOVAKOWSKI (helene.novakowski@etudiant.univ-reims.fr)


#vider la mémoire
rm(list = ls())


#Library
library("DescTools")# Pour la visualisation des données manquantes
library(dplyr)
library(tidyverse) #Pour ggplot
library(nnet)
library('Hmisc') #pour describe
library(MASS)
library(glmnet)


## Importation du fichier
CO2 <- read.csv2("bdd_CO2.csv", stringsAsFactors=TRUE)



##### Modèle de régression logistique binomial pour la classification binaire #####

#vérifications
str(CO2)
View(CO2)
nrow(CO2)
ncol(CO2)
#quelques statistiques
summary(CO2)




# calculating the product of dimensions of dataframe 
totalcells_base = prod(dim(CO2)) #nombre total de cellule est de 1431144

# calculating the number of cells with na
missingcells = sum(is.na(CO2)) #nombre de cellule na est de 58307

# calculating percentage of missing values
(missingcells * 100 )/(totalcells_base) #pourcentage de na est de 4.07





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


#Correction date ???????
str(CO2)




# calculating the product of dimensions of dataframe 
totalcells = prod(dim(CO2)) #nombre total de cellule est de 1321056

# calculating the number of cells with na
missingcells = sum(is.na(CO2)) #nombre de cellule na est de 3148

# calculating percentage of missing values
(missingcells * 100 )/(totalcells) #pourcentage de na est de 0.24




####################################################################################################################################
#### Modèle de régression logistique binomial####
####################################################################################################################################
CO2 %>% 
  mutate(var_co2 = ifelse(co2 > 200,"pollution","correct")) %>%
  dplyr::select(-co2, -dscom, -hybride, -date_maj) -> don_bin



# nrow(don_bin)
na.omit(don_bin) -> don_bin  #On retire toutes les données manquantes !
as.factor(don_bin$var_co2) -> don_bin$var_co2



#faire une régression logistique de la variable binaire co2 en fonction des variables (explicatives) 
#de la bd CO2 : 
# modele.RL <- glm(formula = var_co2 ~ lib_mrq+lib_mod_doss+lib_mod+cnit+tvv+cod_cbr+puiss_admin_98+puiss_max+
#                    typ_boite_nb_rapp+conso_urb+conso_exurb+conso_mixte+co_typ_1+hcnox+ptcl+masse_ordma_min+
#                    masse_ordma_max+champ_v9+date_maj+Carrosserie+gamme,  data = don_bin, family = binomial) #Trop grand
# print(modele.RL)
# summary(modele.RL)
# attributes(modele.RL)
# View(CO2)

modele.RL <- glm(formula = var_co2 ~ cod_cbr+puiss_admin_98+typ_boite_nb_rapp+conso_exurb+Carrosserie+gamme, family = binomial,  data = don_bin)
print(modele.RL)
summary(modele.RL)
attributes(modele.RL)





####################################################################################################################################
#### Modification de la base pour multinomial
####################################################################################################################################

summary(CO2)

CO2 %>% 
  mutate(var_co2 = ifelse(co2 < 140,"faible",ifelse(co2 < 200, "moyenne","forte"))) %>% #voir critère
  dplyr::select(-co2, -dscom, -hybride,-date_maj, -conso_mixte, -champ_v9, -lib_mod_doss, -lib_mod, -cnit, -tvv) -> CO2_mod


str(CO2_mod)
colnames(CO2_mod)
miss <- data.frame(CO2_mod)
colnames(miss) <- c("la marque", "le type de carburant",
                    "la puissance administrative","la puissance maximale (en kW)",
                    "le type de boîte de vitesse et le nombre de rapports",
                    "consommation urbaine de carburant (en l/100km)",
                    "consommation extra urbaine de carburant (en l/100km)",
                    "le résultat d’essai de CO type I",
                    "les résultats d’essai HC+NOX",
                    "le résultat d’essai de particules",
                    "la masse en ordre de marche mini",
                    "la masse en ordre de marche maxi",
                    "Carrosserie","gamme",
                    "Statut de pollution par émission de CO2 (en g/km)")
# View(miss)

####Données manquantes :
length(which(is.na(miss)))
# PlotMiss(miss, main = "Données manquantes")
#en baton rouge c'est une données manquantes

na.omit(CO2_mod) -> data_co2  #On retire toutes les données manquantes !
as.factor(data_co2$var_co2) -> data_co2$var_co2
# colnames(data_co2)
ncol(data_co2)
ncol(CO2)
# View(data_co2)
# describe(data_co2)
nrow(data_co2)
nrow(CO2)

totalcells_mod = prod(dim(data_co2)) #nombre total de cellule est de 993322
((totalcells_base -totalcells_mod )*100)/ totalcells_base #on a retiré 45.1% de la base initial

totalcells = prod(dim(CO2_mod)) #nombre total de cellule est de 993322
((totalcells -totalcells_mod )*100)/ totalcells #on a retiré 4.9% par sélection de lignes sans données manquantes sur la base conennant certaines colonnes




####################################################################################################################################
#### Modèle de régression logistique multinomial pour la classification multi-groupe ####
####################################################################################################################################

#Transformation de la base pour considérer chaque modalité
XX <- model.matrix(var_co2 ~ ., data = data_co2)[,-1] #Cette fonction construit la matrice de design en remplaçant 
#chacune des variables qualitatives par les indicatrices 
#de ses modalités (la première modalité est supprimée)
#on supprime la première colonne correspondant à l'intercept
# View(XX)
data_co2 <- cbind(as.data.frame(XX), var_co2 = as.factor(data_co2[,"var_co2"])) #bd constituée que de variables explicatives numériques 
# View(data_co2)





# Nous avons un grand nombre de variable explicatives
#Nous allons faire une pré- sélection avec le lasso avant d'utiliser des méthodes de sélection de modèle


#### Régression logistique multinomial Lasso ####
reg.lasso <- glmnet(x = scale(data_co2[, !(colnames(data_co2) == "var_co2")]), 
                    y = data_co2[, "var_co2"], family = "multinomial", 
                    type.multinomial = "grouped", alpha = 1)

# par(mfrow = c(1,1))
plot(reg.lasso, label = TRUE)
plot(reg.lasso, xvar = "lambda", label = TRUE, lwd = 2)
reg.cvlasso <- cv.glmnet(x = scale(data_co2[, !(colnames(data_co2) == "var_co2")]), 
                         y = data_co2[, "var_co2"], family = "multinomial",
                         type.measure = "class", 
                         type.multinomial = "grouped", 
                         nflods = nrow(data_co2), alpha = 1)

bestlam <- reg.cvlasso$lambda.min
bestlam
plot(reg.cvlasso)
min(reg.cvlasso$cvm) #erreur de classification du modèle lasso optimal 0.006206791
#coef(reg.cvlasso)

indices <- !(coef(reg.cvlasso)[[2]]  == 0)
indices.var.select <- indices[2:length(indices)]
#les variables explicatives sélectionnées par le lasso
noms.var.lasso <- colnames(data_co2[, !(colnames(data_co2) == "var_co2")])[indices.var.select]
# "lib_mrqCADILLAC"                "lib_mrqLEXUS"                   "lib_mrqMERCEDES"               
# [4] "lib_mrqNISSAN"                  "cod_cbrFE"                      "cod_cbrGO"                     
# [7] "puiss_admin_98"                 "typ_boite_nb_rappA 6"           "typ_boite_nb_rappA 9"          
# [10] "typ_boite_nb_rappV 0"           "conso_urb"                      "conso_exurb"                   
# [13] "CarrosserieBREAK"               "CarrosserieTS TERRAINS/CHEMINS" "gammeLUXE"                     
# [16] "gammeMOY-INFER"                 "gammeSUPERIEURE"                "hc_nox"
ncol(data_co2)
# 18 var explicatives sur 100

data_co2 %>% 
  dplyr::select(lib_mrqCADILLAC, lib_mrqLEXUS, lib_mrqMERCEDES, lib_mrqNISSAN, cod_cbrFE, cod_cbrGO, puiss_admin_98,
                `typ_boite_nb_rappA 6`, `typ_boite_nb_rappA 9`, `typ_boite_nb_rappV 0`, conso_urb, conso_exurb,
                CarrosserieBREAK, `CarrosserieTS TERRAINS/CHEMINS`, gammeLUXE, `gammeMOY-INFER`, gammeSUPERIEURE, hc_nox, var_co2) -> co2.data2

# co2.data2 <- data_co2[, noms.var.lasso]
# co2.data2 <- as.data.frame(cbind(co2.data2, var_co2 = data_co2$var_co2))
ncol(co2.data2)
str(co2.data2)

View(co2.data2)
summary(co2.data2)
# colnames(co2.data)


# #variable réponse : var_co2
# #variables explicatives (numériques) : 
# mrqCADILLAC, lib_mrqLEXUS, lib_mrqMERCEDES, lib_mrqNISSAN, cod_cbrFE, cod_cbrGO,puiss_admin_98, 
# typ_boite_nb_rappA 6, typ_boite_nb_rappA 9, typ_boite_nb_rappV 0, conso_urb, conso_exurb, CarrosserieBREAK, 
# CarrosserieTS TERRAINS/CHEMINS, gammeLUXE, gammeMOY-INFER, gammeSUPERIEURE, hc_nox

cor(co2.data2[, !(colnames(co2.data2)=="var_co2")]) -> cor_var
# View(cor_var)


#la fonction multinom() calcule les estimateurs des paramètres,
#du model de RegLogMultinomial, par maximum de vraisemblance;
#maxit : est le nombre maximal d'itérations pour le calcul des estimateurs MV
modele.complet <- multinom(formula = var_co2 ~ ., data = co2.data2, model = TRUE, maxit = 1000)
print(modele.complet)
print(summary(modele.complet))
attributes(modele.complet)
modele.complet$edf #donne le nombre de paramètres du modèle de RegLogMultinomial
formula(modele.complet$model)

# var_co2 ~ `typ_boite_nb_rappA 5` + `typ_boite_nb_rappA 6` + conso_exurb + 
#   CarrosserieCOMBISPACE + CarrosserieMINIBUS + `CarrosserieTS TERRAINS/CHEMINS` + 
#   gammeLUXE

modele.trivial <- multinom(formula = var_co2 ~ 1, data = co2.data2, model = TRUE, maxit = 3000)
print(modele.trivial)
print(summary(modele.trivial))
modele.trivial$edf
attributes(modele.trivial)
formula(modele.trivial$model)

# var_co2 ~ 1



################################################################################################################"
#### Sélection de modèles (de variables) selon le critère AIC par algorithmes de recherche pas-à-pas ####


#la méthode backward elimination
modele.back <- step(object = modele.complet,
                    scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                    direction = "backward") #par défaut critère AIC
modele.back
formula(modele.back$model) #le modèle optimal obtenu

#le meilleur modèle est
# var_co2 ~ lib_mrqLEXUS + lib_mrqMERCEDES + cod_cbrFE + cod_cbrGO + 
#   `typ_boite_nb_rappA 6` + conso_urb + conso_exurb + CarrosserieBREAK + 
#   `CarrosserieTS TERRAINS/CHEMINS` + gammeLUXE + `gammeMOY-INFER`


#la méthode forward selection
modele.forward <- step(object = modele.trivial,
                       scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                       direction = "forward")
modele.forward
formula(modele.forward$model) #le modèle optimal obtenu

#on cherche la var qui fait baisser le + le critère AIC
# on a le modèle :
# var_co2 ~ conso_urb + conso_exurb + cod_cbrGO + lib_mrqMERCEDES + 
#   cod_cbrFE + `gammeMOY-INFER` + lib_mrqLEXUS + `typ_boite_nb_rappA 6` + 
#   `CarrosserieTS TERRAINS/CHEMINS` + CarrosserieBREAK + gammeLUXE



#la méthode bidirectional elimination / combine backward et forward
modele.bidirect.elim <- step(object = modele.complet,
                             scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                             direction = "both")
modele.bidirect.elim
formula(modele.bidirect.elim$model) #le modèle optimal obtenu

# on a le modèle:
# lib_mrqLEXUS + lib_mrqMERCEDES + cod_cbrFE + cod_cbrGO + 
#   `typ_boite_nb_rappA 6` + conso_urb + conso_exurb + CarrosserieBREAK + 
#   `CarrosserieTS TERRAINS/CHEMINS` + gammeLUXE + `gammeMOY-INFER`

#la méthode bidirectional selection / on commence par le modèle trivial, ascendante
modele.bidirect.select <- step(object = modele.trivial,
                               scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                               direction = "both")
modele.bidirect.select
formula(modele.bidirect.select$model) #le modèle optimal obtenu

# Modèle 
# var_co2 ~ conso_urb + conso_exurb + cod_cbrGO + lib_mrqMERCEDES + 
#   cod_cbrFE + `gammeMOY-INFER` + lib_mrqLEXUS + `typ_boite_nb_rappA 6` + 
#   `CarrosserieTS TERRAINS/CHEMINS` + CarrosserieBREAK + gammeLUXE

#les 4 algorithmes précédents donne le même modèle  complet!



#### Sélection de modèles (de variables) selon le critère BIC par algorithmes de recherche pas-à-pas ####
n <- nrow(co2.data2) #nombre d'observation

#la méthode backward elimination
modele.back <- step(object = modele.complet,
                    scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                    direction = "backward", k = log(n))
modele.back
formula(modele.back$model) #le modèle optimal obtenu

# Modèle
# var_co2 ~ lib_mrqLEXUS + lib_mrqMERCEDES + cod_cbrFE + cod_cbrGO + 
#   `typ_boite_nb_rappA 6` + conso_urb + conso_exurb + CarrosserieBREAK + 
#   `CarrosserieTS TERRAINS/CHEMINS` + gammeLUXE + `gammeMOY-INFER`


#la méthode forward selection
modele.forward <- step(object = modele.trivial,
                       scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                       direction = "forward", k = log(n))
modele.forward
formula(modele.forward$model) 

#le modèle optimal obtenu
# AIC=558.87
# var_co2 ~ conso_urb + conso_exurb + cod_cbrGO + lib_mrqMERCEDES + 
#   cod_cbrFE + `gammeMOY-INFER` + lib_mrqLEXUS + `typ_boite_nb_rappA 6`

#la méthode bidirectional elimination
modele.bidirect.elim <- step(object = modele.complet,
                             scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                             direction = "both", k = log(n))
modele.bidirect.elim
formula(modele.bidirect.elim$model) #le modèle optimal obtenu

# Modèle :
# var_co2 ~ lib_mrqLEXUS + lib_mrqMERCEDES + cod_cbrFE + cod_cbrGO + 
#   `typ_boite_nb_rappA 6` + conso_urb + conso_exurb + CarrosserieBREAK + 
#   `CarrosserieTS TERRAINS/CHEMINS` + gammeLUXE + `gammeMOY-INFER`

#la méthode bidirectional selection
modele.bidirect.select <- step(object = modele.trivial,
                               scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                               direction = "both", k = log(n))
modele.bidirect.select
formula(modele.bidirect.select$model) #le modèle optimal obtenu

# var_co2 ~ conso_urb + conso_exurb + cod_cbrGO + lib_mrqMERCEDES + 
#   cod_cbrFE + `gammeMOY-INFER` + lib_mrqLEXUS + `typ_boite_nb_rappA 6`

# Les deux critères AIC et BIC donne le même modèle. (à l'exception du BIC selection)




#################################################################################
#Test#
################################################################################################################"


#### Tests d'hypothèses par maximum de vraisemblance dans le modèle de RegLog Multinom ####
#Nous allons considérer le modèle optimal sélectionné selon le critère BIC selection (8var) et le modèle selon AIC (11var)
#on crée d'abord le sous-ensemble de données correspondantù

# View(data_co2)
# ncol(data_co2)
#pour le modèle complet à 11 variables
data_co2 %>%
  dplyr::select(var_co2, conso_urb, conso_exurb, cod_cbrGO, lib_mrqMERCEDES, 
                cod_cbrFE, `gammeMOY-INFER`, lib_mrqLEXUS, `typ_boite_nb_rappA 6`,
                `CarrosserieTS TERRAINS/CHEMINS`, CarrosserieBREAK, gammeLUXE) -> opt.data
View(opt.data)
ncol(opt.data)

#### Test de validité du modèle global : H_0 : w_2 = w_3 = (0,0,0) ####
modele <-  multinom(formula = var_co2 ~  ., data = opt.data, maxit = 3000)  #déviance du modèle global
modele.reduit <- multinom(formula = var_co2 ~ 1, data = opt.data, maxit = 3000)  #déviance du modèle réduit

Sn <- modele.reduit$deviance-modele$deviance #la statistique du rapport de vraisemblance
print(Sn) #51287.8
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
#nb de paramère du modèle, 3 var +1 + 2 modalité donc 8 -2=6
#d différence des 2 dimensions (=nb de paramètre)

pvalue <- pchisq(q = Sn, df = d, lower.tail = F)
print(pvalue) #on obtient 0, le modèle optimal est très significatif






#pour le modèle exception à 8 variables

data_co2 %>%
  dplyr::select(var_co2, conso_urb, conso_exurb, cod_cbrGO, lib_mrqMERCEDES, 
                cod_cbrFE, `gammeMOY-INFER`, lib_mrqLEXUS, `typ_boite_nb_rappA 6`) -> opt.data2
View(opt.data2)
ncol(opt.data2)
# opt.data2 <- data.frame(co2.data2)

#### Test de validité du modèle global : H_0 : w_2 = w_3 = (0,0,0) ####
modele <-  multinom(formula = var_co2 ~  ., data = opt.data2, maxit = 3000)  #déviance du modèle global
modele.reduit <- multinom(formula = var_co2 ~ 1, data = opt.data2, maxit = 3000)  #déviance du modèle réduit

Sn <- modele.reduit$deviance-modele$deviance #la statistique du rapport de vraisemblance
print(Sn) #51287.8
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
#nb de paramère du modèle, 3 var +1 + 2 modalité donc 8 -2=6
#d différence des 2 dimensions (=nb de paramètre)

pvalue <- pchisq(q = Sn, df = d, lower.tail = F)
print(pvalue) #on obtient 0, le modèle optimal est très significatif









#################################################################################################

#### Erreurs de classification, évaluées par validation croisées, 
#pour le modèle optimal selon BIC (réduit aux trois variables Sepal.Width, Petal.Length et Petal.Width),  
#et pour le modèle complet utilisant les quatre variables #### 
indices <- 1:nrow(opt.data)
#la fonction évaluant l'erreur de classification des deux modèles, pour une partition donnée
err_classif <- function(l){
  #on partage le tableau en deux parties : par exemple (l-1)/l pour apprentissage et 1/l pour le test
  indices.ensemble.test <- sample(indices, trunc(length(indices)/l), replace = FALSE)
  ensemble.test <- opt.data[indices.ensemble.test, ]
  ensemble.apprentissage <- opt.data[-indices.ensemble.test, ]
  modele.BIC <- multinom(formula = var_co2 ~ conso_urb + conso_exurb + cod_cbrGO + lib_mrqMERCEDES + 
                                        cod_cbrFE + `gammeMOY-INFER` + lib_mrqLEXUS + `typ_boite_nb_rappA 6`,
                         data = ensemble.apprentissage, maxit = 3000) # le modèle optimal
  modele.complet <- multinom(formula = var_co2 ~ ., 
                             data = ensemble.apprentissage, maxit = 3000) # le modèle complet
  pred.moda.modele.BIC <- predict(object = modele.BIC, newdata = ensemble.test)
  pred.moda.modele.complet <- predict(object = modele.complet, newdata = ensemble.test)
  erreur.modele.BIC <- mean(!(pred.moda.modele.BIC == ensemble.test$var_co2)) #pourcentage des indiv sur l'ensemble des test pour lesquels on s'est trompé
  erreur.modele.complet <- mean(!(pred.moda.modele.complet == ensemble.test$var_co2))   
  return(c(erreur.modele.BIC, erreur.modele.complet))
}  
# l=3
print(err_classif(3)) #premier est pour le modèle réduit, l'autre le modèle complet
# 0.001088576 0.001145869

#il faut le faire plusieurs fois ici 100

#on applique la fonction précédente M = 100 fois, à l'aide de la fonction ``replicate'', 
#et on met les résultats dans le tableau resultats, de dimension 2xM,
#cela évite l'utilisation de boucles for. 
M <- 100 #nombre de réplications
resultats <- replicate(M, err_classif(3))
resutats.moyens <- apply(resultats,1,mean) #on calcule la moyenne par colonne
err.classif.modele.BIC <- resutats.moyens[1]
err.classif.modele.complet <- resutats.moyens[2]
err.classif.modele.BIC #0.001084565
err.classif.modele.complet #0.001208319


#la plus faible erreur parmi les 2 nous donne le modèle qui prédit le mieux
#différence de 0.0124 %
err.classif.modele.BIC - err.classif.modele.complet
#on obtient des résultats comparables, pour cet exemple, l'erreur de classification est de 4% en moyenne pour les deux modèles,
#le modèle optimal est quand même mieux, car utilise peu de variables, par rapport au modèle complet, et prédit avec efficacité comparable ...

















#################################################################################
#var la + significative est celle dont la p-value est la + faible
################################################################################################################"


#### Tester si la variable conso_exurb n'est pas significative dans le modèle : H_0 : w_{2,1} = w_{3,1} = 0 ####
#H0 la variable x_1 n'est pas significative dans le modèle
#le modèle réduit exclu la variable x1
modele <-  multinom(formula = var_co2 ~  ., data = opt.data2, maxit = 3000) 
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="conso_exurb")], 
                          maxit = 2000) #le modèle réduit
Sn.conso_exurb <- modele.reduit$deviance - modele$deviance
print(Sn.conso_exurb)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.conso_exurb <- pchisq(q = Sn.conso_exurb, df = d, lower.tail = F)
print(pvalue.conso_exurb)


#Tester si la variable conso_urb n'est pas significative : H_0 = w_{2,2} = w_{3,2} = 0
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="conso_urb" )], 
                          maxit = 2000) #le modèle réduit
Sn.conso_urb <- modele.reduit$deviance - modele$deviance
print(Sn.conso_urb)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.conso_urb <- pchisq(q = Sn.conso_urb, df = d, lower.tail = F)
print(pvalue.conso_urb)


#Tester si la variable `typ_boite_nb_rappA 6` n'est pas significative : H_0 = w_{2,2} = w_{3,2} = 0
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="typ_boite_nb_rappA 6" )], 
                          maxit = 2000) #le modèle réduit
Sn.typ_boite_nb_rappA_6 <- modele.reduit$deviance - modele$deviance
print(Sn.typ_boite_nb_rappA_6)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.typ_boite_nb_rappA_6 <- pchisq(q = Sn.typ_boite_nb_rappA_6, df = d, lower.tail = F)
print(pvalue.typ_boite_nb_rappA_6)

#Tester si la variable cod_cbrGO n'est pas significative : H_0 = w_{2,4}=w_{3,4}=0
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="cod_cbrGO")], 
                          maxit = 2000) #le modèle réduit
Sn.cod_cbrGO <- modele.reduit$deviance - modele$deviance
print(Sn.cod_cbrGO)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.cod_cbrGO <- pchisq(q = Sn.cod_cbrGO, df = d, lower.tail = F)
print(pvalue.cod_cbrGO)


#Tester si la variable lib_mrqMERCEDES n'est pas significative : H_0 = w_{2,5}=w_{3,5}=0
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="lib_mrqMERCEDES")], 
                          maxit = 2000) #le modèle réduit
Sn.lib_mrqMERCEDES <- modele.reduit$deviance - modele$deviance
print(Sn.lib_mrqMERCEDES)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.lib_mrqMERCEDES <- pchisq(q = Sn.lib_mrqMERCEDES, df = d, lower.tail = F)
print(pvalue.lib_mrqMERCEDES)


#Tester si la variable gammeMOY-INFER n'est pas significative : H_0 = w_{2,5}=w_{3,5}=0
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="gammeMOY-INFER")], 
                          maxit = 2000) #le modèle réduit
Sn.gammeMOY_INFER <- modele.reduit$deviance - modele$deviance
print(Sn.gammeMOY_INFER)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.gammeMOY_INFER <- pchisq(q = Sn.gammeMOY_INFER, df = d, lower.tail = F)
print(pvalue.gammeMOY_INFER)

#Tester si la variable cod_cbrFE n'est pas significative : H_0 = w_{2,5}=w_{3,5}=0
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="cod_cbrFE")], 
                          maxit = 2000) #le modèle réduit
Sn.cod_cbrFE <- modele.reduit$deviance - modele$deviance
print(Sn.cod_cbrFE)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.cod_cbrFE <- pchisq(q = Sn.cod_cbrFE, df = d, lower.tail = F)
print(pvalue.cod_cbrFE)

#Tester si la variable lib_mrqLEXUS n'est pas significative : H_0 = w_{2,5}=w_{3,5}=0
modele.reduit <- multinom(formula = var_co2 ~ ., 
                          data = opt.data2[, !(colnames(opt.data2)=="lib_mrqLEXUS")], 
                          maxit = 2000) #le modèle réduit
Sn.lib_mrqLEXUS <- modele.reduit$deviance - modele$deviance
print(Sn.lib_mrqLEXUS)
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
pvalue.lib_mrqLEXUS <- pchisq(q = Sn.lib_mrqLEXUS, df = d, lower.tail = F)
print(pvalue.lib_mrqLEXUS)






##+ var significative + Sn est grand
#### Classer les variables ####
pvalues <- c(pvalue.conso_exurb, pvalue.conso_urb, pvalue.typ_boite_nb_rappA_6, pvalue.lib_mrqLEXUS,
             pvalue.cod_cbrGO, pvalue.lib_mrqMERCEDES, pvalue.gammeMOY_INFER, pvalue.cod_cbrFE)
length(pvalues)
names(pvalues) <- colnames(opt.data2[, !(colnames(opt.data2) == "var_co2")])
variables.classees <- sort(pvalues)
print(variables.classees) # on obtient le classement suivant :

#Et pour Sn
Sn <- c(Sn.conso_exurb, Sn.conso_urb, Sn.typ_boite_nb_rappA_6, Sn.lib_mrqLEXUS,
        Sn.cod_cbrGO, Sn.lib_mrqMERCEDES, Sn.gammeMOY_INFER, Sn.cod_cbrFE)
length(Sn)
names(Sn) <- colnames(opt.data2[, !(colnames(opt.data2) == "var_co2")])
variables.classees2 <- rev(sort(Sn))
print(variables.classees2) # on obtient le classement suivant :



nrow(data_co2)
# Sans oublier la fréquence d'apparition de ces variables significatives dans notre base 
summary(data_co2$var_co2)
# faible   forte moyenne 
# 3613   32211   16538 
# 6.9 %  61.5 %   31.5 %

# conso_urb              9.395 moy
# conso_exurb            6.685 moy
# cod_cbrGO              94,13 %
# lib_mrqMERCEDES        66,68 %
# cod_cbrFE              0.00382 %
# gammeMOY-INFER         62,56 %
# lib_mrqLEXUS           0.48 %
# typ_boite_nb_rappA 6   1.26%













#################################################################################
#Utilisation#
#################################################################################
coef_reg <-summary(modele.forward)$coefficients
#        (Intercept) conso_urb conso_exurb cod_cbrGO lib_mrqMERCEDES cod_cbrFE `gammeMOY-INFER` lib_mrqLEXUS `typ_boite_nb_rappA 6`
# forte    -2861.1348  134.0892   209.01258 331.76214       21.134494 -175.0205       11.2490080   -40.200460              -4.625905
# moyenne   -324.3317   20.0975    33.55833  35.92446        2.682887 -216.3935       -0.1897904     7.889877               3.122098

V_conso_urb = 11.2
V_conso_exurb = 6.8
V_cod_cbrGO = 0   #Attention si 1 GO alors 0 FE
V_lib_mrqLEXUS = 0   #Attention si 1 alors aux autres !
V_lib_mrqMERCEDES = 0
V_cod_cbrFE =  0
V_gammeMOY_INFER = 0
V_typ_boite_nb_rappA_6 = 0

res_classif_par_reg_logi <- function(V_conso_urb, V_conso_exurb, V_cod_cbrGO, 
                                     V_lib_mrqMERCEDES, V_cod_cbrFE, V_gammeMOY_INFER, 
                                     V_lib_mrqLEXUS, V_typ_boite_nb_rappA_6){
  
  forte = coef_reg[[1]]+coef_reg[[3]]*V_conso_urb + coef_reg[[5]]*V_conso_exurb 
  + coef_reg[[7]]*V_cod_cbrGO + coef_reg[[9]]*V_lib_mrqMERCEDES + coef_reg[[11]]*V_cod_cbrFE 
  + coef_reg[[13]]*V_gammeMOY_INFER + coef_reg[[15]]*V_lib_mrqLEXUS + coef_reg[[17]]*V_typ_boite_nb_rappA_6
  
  moyenne = coef_reg[[2]] + coef_reg[[4]]*V_conso_urb + coef_reg[[6]]*V_conso_exurb 
  + coef_reg[[8]]*V_cod_cbrGO + coef_reg[[10]]*V_lib_mrqMERCEDES + coef_reg[[12]]*V_cod_cbrFE 
  + coef_reg[[14]]*V_gammeMOY_INFER + coef_reg[[16]]*V_lib_mrqLEXUS + coef_reg[[18]]*V_typ_boite_nb_rappA_6
  
  #Pollution = Faible / Moyenne / Forte
  
  Pfaible = 1/(1+exp(forte)+ exp(moyenne))            #proba Y=faible conditionnellement au var explicaive
  Pforte = exp(forte)/(1+exp(forte)+ exp(moyenne))    #proba Y=forte conditionnellement au var explicaive
  Pmoy = exp(moyenne)/(1+exp(forte)+ exp(moyenne))    #proba Y=forte conditionnellement au var explicaive
  
  #prendre la proba max
  maxi = max(Pfaible, Pforte, Pmoy, na.rm=TRUE)
  
  
  # Proba = c(Pfaible, Pforte, Pmoy)
  if (Pfaible == maxi) {
    Proba = "faible"
  } else if (Pforte == maxi) {
    Proba = "forte"
  } else {
    Proba = "moyenne"
  }
  
  return(Proba)
}  #nous donne le statut de pollution du new vehicule


print(res_classif_par_reg_logi(V_conso_urb, V_conso_exurb, V_cod_cbrGO, 
                               V_lib_mrqLEXUS, V_lib_mrqMERCEDES, V_cod_cbrFE, 
                               V_gammeMOY_INFER, V_typ_boite_nb_rappA_6)) 







#____________________________________________________________________________________

# plot(modele.forward, dat)


# barplot(sort(variables.classees2, decreasing = TRUE))
# barplot(sort(var.imp, decreasing = TRUE)[1:10], names.arg=rownames(var.imp)[ord][1:10], cex.names=0.6)


#_________________________________________________________________________________
# modele.forward


View(base_Test)
str(base_Test)

base_Test %>% 
  mutate(vehi = 1:nrow(base_Test)) %>% 
  group_by(vehi) %>% 
  mutate(predi = res_classif_par_reg_logi(conso_urb[1], conso_exurb[1], cod_cbrGO[1], lib_mrqMERCEDES[1], 
                                          cod_cbrFE[1], `gammeMOY-INFER`[1], lib_mrqLEXUS[1], `typ_boite_nb_rappA 6`[1])) %>% 
  dplyr::select(var_co2, predi) -> base_pred


# describe(base_Test)#define vectors of actual values and predicted values
actual <- factor(base_Test$var_co2)
pred <- factor(base_pred$predi)


library(caret)
#F score
table(actual, pred)
confusionMatrix(pred, actual, mode = "everything", positive="1") 












