install.packages("FactoMineR")
library(FactoMineR)



voitures <- read.csv("autos2005.csv", sep=";", header = T, dec = ",", row.names = 1, fileEncoding = "ISO-8859-1")
# Import du fichier CSV dans un data frame
# sep=";" : colonnes séparées par des points-virgules
# header=T : la 1re ligne contient les noms des variables
# dec="," : décimales écrites avec une virgule (sinon les nombres seraient lus comme du texte)
# row.names=1 : la 1re colonne (modèles de voitures) devient le nom des lignes,
#   ce qui étiquettera les individus sur les graphiques de l'ACP
# fileEncoding : fichier en Latin-1, pour bien lire les accents (cylindrée, réservoir...)
voitures <- read.csv("autos2005.csv", sep=";", header = T, dec = ",", row.names = 1, fileEncoding = "ISO-8859-1")
voitures

# Vérifie la structure : 40 individus, 12 variables, et surtout que tout est
# bien numérique (sinon la virgule décimale a mal été lue)
str(voitures)

# Statistiques de base (min, quartiles, moyenne, max) de chaque variable.
# Montre que les ordres de grandeur sont très différents d'une variable à l'autre
# (cylindrée en cm3, consommation en L/100km...) : ça justifie de standardiser plus bas
summary(voitures)


# Nuages de points de toutes les paires de variables.
# Des nuages très alignés = variables fortement corrélées (puissance, cylindrée,
# poids...). C'est la colinéarité évoquée dans l'énoncé, qui motive l'ACP
pairs(voitures)

# Garde les colonnes 1 à 11 (variables explicatives) et retire la 12e : le prix.
# L'énoncé demande l'ACP "sauf le prix" : le prix est la variable à expliquer,
# il ne doit pas influencer la construction des axes. On le comparera
# seulement après, aux groupes obtenus par classification
voitures1<- voitures[,1:11]
voitures1


# Centrage-réduction de chaque variable :
# center=T : on retranche la moyenne de chaque colonne
# scale=T : on divise par l'écart-type de chaque colonne
# Chaque variable a alors moyenne 0 et variance 1. Indispensable car l'ACP repose
# sur les variances : sans ça, une variable à grandes valeurs (cylindrée) dominerait
# les axes à cause de son unité seulement. On obtient une ACP normée
# (basée sur la matrice de corrélations), où toutes les variables pèsent pareil
voitures2<- scale(voitures1, center=T,scale = T)




# Calcule la matrice des distances entre tous les individus (les 40 voitures),
#le calcul des distances sert à faire la CHA
voitures.d <- dist(voitures2)
voitures.d




# Classification on the initial data

# CAH = Classification Ascendante Hiérarchique.
# Principe : au départ, chaque voiture forme sa propre classe (40 classes).
# À chaque étape, on fusionne les deux classes les plus proches, jusqu'à n'avoir
# plus qu'une seule classe contenant toutes les voitures.
# On part de la matrice de distances voitures.d calculée juste avant.
# method="ward.D2" : critère de Ward. On fusionne les deux classes dont la fusion
# fait le moins augmenter l'inertie intra-classe (la dispersion à l'intérieur des
# classes). Résultat : des classes compactes et homogènes, bien séparées entre elles.
# "ward.D2" est la version correcte de Ward quand on lui donne de vraies distances
# euclidiennes (et non des distances au carré)
cah.ward <- hclust(voitures.d, method = "ward.D2")

# Affichage de l'objet : rappelle la méthode utilisée, la mesure de distance
# et le nombre d'individus classés (40)
cah.ward

# Trace le dendrogramme (arbre de la classification).
# En bas, les 40 voitures ; plus on monte, plus les fusions se font entre des
# classes éloignées. La hauteur d'une fusion indique la perte d'inertie intra-classe
# qu'elle provoque : un grand saut de hauteur signale qu'on fusionne des groupes
# très différents, donc qu'il vaut mieux couper l'arbre juste avant.
# C'est ce qui sert à choisir le nombre de classes
plot(cah.ward)

# Dessine des rectangles sur le dendrogramme pour visualiser la coupure en k=3 classes.
# Ce choix de 3 se justifie par les grands sauts de hauteur en haut de l'arbre
# (à vérifier visuellement sur le dendrogramme)
rect.hclust(cah.ward, k=3)

# Coupe l'arbre en 3 classes et stocke le résultat dans groupes.cah :
# un vecteur de 40 valeurs (1, 2 ou 3) donnant la classe de chaque voiture,
# dans l'ordre des lignes de voitures. Ce vecteur est essentiel pour la suite :
# - comparer le prix moyen de chaque classe (ex. avec tapply ou aggregate)
# - colorer les points selon leur classe sur le plan factoriel 1-2 de l'ACP
groupes.cah <- cutree(cah.ward, k=3)

# Affichage du vecteur des classes : pour chaque voiture, son numéro de classe
groupes.cah

# Même chose, mais triée par numéro de classe : on voit directement
# quelles voitures sont regroupées ensemble dans la classe 1, puis 2, puis 3.
# print() force l'affichage ; sort() ordonne les valeurs de la plus petite à la plus grande
print(sort(groupes.cah))




# Classification based on the components of the PCA



# Cette fois, on classe les voitures non plus sur les données brutes standardisées,
# mais sur leurs coordonnées dans l'ACP (les composantes principales).
# Avantage : les dernières composantes, qui contiennent surtout du bruit, peuvent
# être écartées, et la classification porte sur l'information structurée.

# Réalise l'ACP avec le package FactoMineR (library(FactoMineR) doit avoir été chargé).
# voitures1 : les 11 variables explicatives, sans le prix (donc le prix ne construit pas les axes)
# scale.unit=T : centre et réduit les variables (ACP normée, sur la matrice de corrélations),
#   ce qui rend inutile le scale() fait plus haut : PCA le refait en interne
# ncp=Inf : conserve toutes les composantes (ici 11) dans le résultat, au lieu des 5 par défaut.
#   Ça permet à HCPC de décider ensuite lui-même combien de composantes utiliser.
# Cette ligne produit aussi les graphiques de base : le plan des individus (les voitures)
# et le cercle des corrélations (les variables) sur le plan factoriel 1-2.
# C'est le cercle qui montre les groupes de variables corrélées entre elles
# et donc la colinéarité dont parle l'énoncé
acpvoitures <- PCA(voitures1, scale.unit=T, ncp=Inf)

# Résumé de l'ACP : valeurs propres (la variance portée par chaque axe),
# pourcentage de variance expliquée et cumulée, puis coordonnées, contributions
# et cos² des individus et des variables.
# On s'en sert pour choisir le nombre d'axes à retenir : si le 1er axe porte par exemple
# 70 % de l'inertie, il résume à lui seul "la taille/puissance" des voitures
summary(acpvoitures)

# HCPC = Hierarchical Clustering on Principal Components (FactoMineR).
# Elle enchaîne automatiquement : calcul des distances sur les coordonnées de l'ACP,
# CAH avec le critère de Ward, dendrogramme, puis choix du nombre de classes.
# nb.clust=-1 : le nombre de classes est choisi automatiquement, en coupant l'arbre
#   à l'endroit où le gain d'inertie est maximal (le plus grand saut de hauteur).
#   Avec nb.clust=0, on cliquerait soi-même sur le dendrogramme pour choisir.
# Le résultat (cah.ward, qui écrase l'objet précédent) contient notamment :
# - data.clust : les données avec une colonne supplémentaire "clust" (la classe de chaque voiture)
# - desc.var : les variables qui caractérisent chaque classe
# - desc.ind : les individus les plus typiques de chaque classe (les "parangons")
# On y trouvera les classes à croiser avec le prix.
# Par défaut, HCPC affiche aussi le dendrogramme, le plan factoriel 1-2 avec les classes
# colorées et une vue en 3D (arbre + nuage de points)
cah.ward <- HCPC(acpvoitures, nb.clust=-1)

# clicking on the graph allows the groups to appear in the PCA projection plane
# (commentaire déjà présent dans ton code, que je traduis : avec nb.clust=0, on clique sur le
# dendrogramme pour fixer le nombre de classes, puis les groupes apparaissent colorés
# dans le plan de projection de l'ACP)

plot(cah.ward,choice="map")
plot(cah.ward,choice="3D.map")
plot(cah.ward,choice="tree")
plot(cah.ward,choice="bar")
cah.ward$desc.var


#with the missing data
#importing data and descriptive statistics
cars <- read.csv("autos2005_2.csv",sep=";",header = T,dec = ",", row.names = 1,fileEncoding = "ISO-8859-1")
cars
str(cars)
summary(cars)
pairs(cars)
cars1<-cars[,2:11] #we do not take power and price variables for the study
cars1

cars2<-scale(cars1,center = T, scale = T)
cars.d<- dist(cars2)
cars.d
cah.ward<-hclust(cars.d, method = "ward.D2")
cah.ward
plot(cah.ward)
rect.hclust(cah.ward,k=3)
groupes.cah
print(sort(groupes.cah))











