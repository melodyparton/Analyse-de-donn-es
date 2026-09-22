jus_orange <- read.csv2(file = "orange_sansaccent.csv",header = T, sep = ';', dec = '.',row.names = 1)
summary(jus_orange)
cor(jus_orange[, 2:8])#ne sélectionne que les colonnes 2 à 8
# pour les corrélations, si |cor| proche de 1 alors il y a corrélation (positive ou négative) sinon si c'est proche de 0, pas de corrélation

#pour avoir les valeurs propres
library(FactoMineR)
res_pca <- PCA(jus_orange, 
               quanti.sup = 8:15,   # Glucose à Vitamine C
               quali.sup = 16:17,   # Conditionnement et Origine
               graph = FALSE)
res_pca$eig #pour le choix du nombre d'axes
#on retient les valeurs propres >1 --> nombre d'axes, ici 2
res_pca$var$coord[, 1:2] #on cherche les coordonnées des variables dans le plan d'intérêt constitué des axes 1 et 2
res_pca$var$cor[, 1:2] #on cherche les corrélations des variables dans le plan d'intérêt constitué des axes 1 et 2
res_pca$var$cos2[, 1:2] #on calcule les cosinus carrés des variables dans le plan d'intérêt constitué des axes 1 et 2
res_pca$var$contrib[, 1:2]#on calcule les contributions des variables dans le plan d'intérêt constitué des axes 1 et 2

#on fait pareil mais pour les individus au lieu des variables 
res_pca$ind$coord[, 1:2] #on cherche les coordonnées des individus dans le plan d'intérêt constitué des axes 1 et 2
res_pca$ind$cor[, 1:2] #on cherche les corrélations des individus dans le plan d'intérêt constitué des axes 1 et 2
res_pca$ind$cos2[, 1:2] #on calcule les cosinus carrés des individus dans le plan d'intérêt constitué des axes 1 et 2
res_pca$ind$contrib[, 1:2]#on calcule les contributions des individus dans le plan d'intérêt constitué des axes 1 et 2
plot(res_pca, choix = "ind", axes = c(1, 2))

plot(res.pca, choix = "var", axes = c(1, 2), 
     title = "Cercle des corrélations - Plan 1-2") #cercle des corrélations (variables)
plot(res.pca, choix = "ind", axes = c(1, 2), 
     title = "Représentation des individus - Plan 1-2") #nuage des individus

# Comment interpréter ces graphiques :
#   
#   Cercle des corrélations (variables) :
#   
#   La position de chaque flèche indique sa corrélation avec les axes 1 et 2
# Deux variables proches l'une de l'autre (même direction) sont corrélées positivement
# Deux variables opposées (directions inverses) sont corrélées négativement
# Deux variables à angle droit (90°) sont indépendantes
# Une flèche longue (proche du bord du cercle) = variable bien représentée sur ce plan (cos² élevé)
# D'après ce qu'on a vu dans la matrice de corrélation, tu devrais voir sucre/typicité d'odeur d'un côté, et acide/amer à l'opposé sur l'axe 1
# 
# Nuage des individus :
#   
#   Deux individus proches ont un profil sensoriel similaire
# Un individu situé du même côté qu'une variable (dans le cercle des corrélations) a une valeur élevée pour cette variable
# Les individus loin du centre et bien représentés (cos² fort) sont ceux qui caractérisent le mieux la structure du plan

#Pour répondre aux questions 5,6,7 avec les autres axes, il faut tester ceci : 
# Regarde si certaines variables ont un cos2 faible sur le plan 1-2 mais fort sur l'axe 3
res_pca$var$cos2[, 1:3]

# Idem pour les individus
res_pca$ind$cos2[, 1:3]
# Si tu repères des variables ou individus avec un cos² faible sur les axes 1-2 mais un cos² élevé sur l'axe 3, ça justifie d'étudier ce nouveau plan.
# 
# Si tu juges que c'est pertinent, reprends les questions 5, 6, 7 avec le plan (1,3) :


