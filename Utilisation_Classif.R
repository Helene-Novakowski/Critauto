
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
# View(CO2)
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





data_co2 %>% 
  dplyr::select(lib_mrqCADILLAC, lib_mrqLEXUS, lib_mrqMERCEDES, lib_mrqNISSAN, cod_cbrFE, cod_cbrGO, puiss_admin_98,
                `typ_boite_nb_rappA 6`, `typ_boite_nb_rappA 9`, `typ_boite_nb_rappV 0`, conso_urb, conso_exurb,
                CarrosserieBREAK, `CarrosserieTS TERRAINS/CHEMINS`, gammeLUXE, `gammeMOY-INFER`, gammeSUPERIEURE, hc_nox, var_co2) -> co2.data2














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






library(caTools)#séparer la vase en 1 dataset training et 1 test
#on sépare la base en training 75 % et test 25 %
split = sample.split(opt.data2$var_co2, SplitRatio = 0.75)
base_Train = subset(opt.data2, split == TRUE)
base_Test = subset(opt.data2, split == FALSE)


#la fonction multinom() calcule les estimateurs des paramètres,
#du model de RegLogMultinomial, par maximum de vraisemblance;
#maxit : est le nombre maximal d'itérations pour le calcul des estimateurs MV
modele.complet <- multinom(formula = var_co2 ~ ., data = base_Train, model = TRUE, maxit = 1000)
print(modele.complet)
print(summary(modele.complet))
attributes(modele.complet)
modele.complet$edf #donne le nombre de paramètres du modèle de RegLogMultinomial
formula(modele.complet$model)

# var_co2 ~ `typ_boite_nb_rappA 5` + `typ_boite_nb_rappA 6` + conso_exurb + 
#   CarrosserieCOMBISPACE + CarrosserieMINIBUS + `CarrosserieTS TERRAINS/CHEMINS` + 
#   gammeLUXE

modele.trivial <- multinom(formula = var_co2 ~ 1, data = base_Train, model = TRUE, maxit = 3000)
print(modele.trivial)
print(summary(modele.trivial))
modele.trivial$edf
attributes(modele.trivial)
formula(modele.trivial$model)

# var_co2 ~ 1



####Modèle
n <- nrow(base_Train) #nombre d'observation
#la méthode forward selection
modele.forward <- step(object = modele.trivial,
                       scope = list(lower = var_co2 ~ 1, upper = formula(modele.complet$model)),
                       direction = "forward", k = log(n))
modele.forward
formula(modele.forward$model) 



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












