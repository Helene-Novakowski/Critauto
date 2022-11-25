
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








###Echantillonnage sur les moyennes et fortes#### Pour homogénéisé la variable cible pollution dans notre base

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
view(Co2data) 

#On créé une base de données pour créer notre modèle puis une autre pour classifier
#On créé une base de données pour créer notre modèle puis une autre pour classifier
Co2data1 <- Co2data[sample(nrow(Co2data)),]
set.seed(5678)
perm <- sample(10839,8000)
base_Train <- Co2data1[perm,] #échantillon d'apprentissage
base_Test <- Co2data1[-perm,] #échantillon de validation


unique(base_Test$var_co2)




#Le modèle exception à 6 variables

base_Train %>%
  dplyr::select(var_co2, conso_exurb, conso_urb, cod_cbrGO, cod_cbrFE, puiss_admin_98, CarrosserieTS.TERRAINS.CHEMINS) -> base_Train
View(base_Train)
ncol(base_Train)
# opt.data2 <- data.frame(co2.data2)


#### Test de validité du modèle global : H_0 : w_2 = w_3 = (0,0,0) ####
modele <-  multinom(formula = var_co2 ~  ., data = base_Train, maxit = 3000)  #déviance du modèle global
modele.reduit <- multinom(formula = var_co2 ~ 1, data = base_Train, maxit = 3000)  #déviance du modèle réduit

Sn <- modele.reduit$deviance-modele$deviance #la statistique du rapport de vraisemblance
print(Sn) #51287.8
d <- modele$edf - modele.reduit$edf  #donne le n ddl de la loi du chi2 asymptotique de la stat Sn
#nb de paramère du modèle, 3 var +1 + 2 modalité donc 8 -2=6
#d différence des 2 dimensions (=nb de paramètre)

pvalue <- pchisq(q = Sn, df = d, lower.tail = F)
print(pvalue) #on obtient 0, le modèle optimal est très significatif




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






colnames(opt.data2)





#################################################################################
#Utilisation#
#################################################################################
coef_reg <-summary(modele.forward)$coefficients
coef_reg
#         (Intercept) conso_exurb conso_urb cod_cbrGO cod_cbrFE puiss_admin_98 CarrosserieTS.TERRAINS.CHEMINS
# forte    -1727.0156   140.07263  77.53300 195.33016 -135.1363     -0.8859035                      -5.877645
# moyenne   -291.5461    32.55356  16.15634  31.60347 -140.0694      0.2805151                      -4.422694

V_conso_urb = 11.2
V_conso_exurb = 8
V_cod_cbrGO = 1   #Attention si 1 GO alors 0 FE
V_cod_cbrFE = 0    #Attention si 1 FE alors 0 GO
V_puiss_admin_98 = 8.6
V_CarrosserieTS.TERRAINS.CHEMINS = 1

res_classif_par_reg_logi <- function(V_conso_urb, V_conso_exurb, V_cod_cbrGO, V_cod_cbrFE,
                                     V_puiss_admin_98, V_CarrosserieTS.TERRAINS.CHEMINS){
  
  forte = coef_reg[[1]]+coef_reg[[3]]*V_conso_exurb + coef_reg[[5]]* V_conso_urb + coef_reg[[7]]*V_cod_cbrGO 
  + coef_reg[[9]]*V_cod_cbrFE + coef_reg[[11]]*V_puiss_admin_98 + coef_reg[13]*V_CarrosserieTS.TERRAINS.CHEMINS
  
  moyenne = coef_reg[[2]] + coef_reg[[4]]*V_conso_exurb + coef_reg[[6]]*V_conso_urb + coef_reg[[8]]*V_cod_cbrGO 
  + coef_reg[[10]]*V_cod_cbrFE + coef_reg[[12]]*V_puiss_admin_98 + coef_reg[14]*V_CarrosserieTS.TERRAINS.CHEMINS
  
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


print(res_classif_par_reg_logi(V_conso_urb, V_conso_exurb, V_cod_cbrGO, V_cod_cbrFE,
                               V_puiss_admin_98, V_CarrosserieTS.TERRAINS.CHEMINS))







#____________________________________________________________________________________

# plot(modele.forward, dat)


# barplot(sort(variables.classees2, decreasing = TRUE))
# barplot(sort(var.imp, decreasing = TRUE)[1:10], names.arg=rownames(var.imp)[ord][1:10], cex.names=0.6)


#_________________________________________________________________________________
# modele.forward


View(base_Test)
str(base_Test)
colnames(base_Test)

base_Test %>% 
  dplyr::select(conso_urb, conso_exurb, cod_cbrGO, cod_cbrFE,
         puiss_admin_98, CarrosserieTS.TERRAINS.CHEMINS, var_co2) %>% 
  mutate(vehi = 1:nrow(base_Test)) %>% 
  group_by(vehi) %>% 
  mutate(predi = res_classif_par_reg_logi(conso_urb[1], conso_exurb[1], cod_cbrGO[1], cod_cbrFE[1], puiss_admin_98[1], V_CarrosserieTS.TERRAINS.CHEMINS[1])) %>% 
  dplyr::select(var_co2, predi) -> base_pred



# describe(base_Test)#define vectors of actual values and predicted values
actual <- factor(base_pred$var_co2)
pred <- factor(base_pred$predi)


library(caret)
#F score
table(actual, pred)
confusionMatrix(pred, actual, mode = "everything", positive="1") 












