jus_orange <- read.csv2(file = "orange_sansaccent.csv",header = T, sep = ';', dec = '.')
summary(jus_orange)
cor(jus_orange[, 2:8])#ne sélectionne que les colonnes 2 à 8
# pour les corrélations, si |cor| proche de 1 alors il y a corrélation (positive ou négative) sinon si c'est proche de 0, pas de corrélation

#pour avoir les valeurs propres
library(FactoMineR)
res_pca <- PCA(jus_orange[, 2:8], graph = F)
res_pca$eig #pour le choix du nombre d'axes
