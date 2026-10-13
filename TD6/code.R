############################################################
# Exercice 1 : Analyse discriminante - données Poissons
# (même approche que le corrigé : indexation par colonnes,
#  liste poissons1 obtenue avec split)
############################################################

library(MASS)   # lda()
library(klaR)   # greedy.wilks() : sélection de variables

############################################################
# 1. DONNÉES ET STATISTIQUES DESCRIPTIVES
############################################################

load("poissons.rda")   # charge directement l'objet "poissons" (pas besoin de "poissons = ")
poissons
summary(poissons)      # résumé global : colonne 1 = site (Y), colonnes 2 à 15 = 14 variables (X)

# Un data.frame par site (liste de 3 éléments)
poissons1 <- split(poissons[,2:15], poissons[,1])
poissons1

summary(poissons1$site1)
summary(poissons1$site2)
summary(poissons1$site3)

boxplot(poissons[,2]~ poissons[,1])   


par(mfrow = c(3,5))

for (j in 2:15) {
  boxplot(poissons[,j] ~ poissons[,1],
          main = names(poissons)[j],
          xlab = "site",
          ylab =  names(poissons)[j])
}
par(mfrow = c(1,1))

model <- aov(BR~site,data=poissons)
summary(model)

model <- aov(TUB~site,data=poissons)
summary(model)

summary(manova(as.matrix(poissons[,2:15])~poissons[,1]),test="wilks")
library(MASS)
modell<-lda(site~.,data=poissons)
modell


model2<-lda(site~YEU+BR+OP+NAG+FOI+MUS+POI+LON+LAR+LART+LARM+DYEU,data=poissons)
model2


prev <-lda(site~.,data=poissons,CV=TRUE)$class
prev
table(prev, poissons$site)

sum(prev!=poissons$site)/nrow(poissons)
don1<-data.frame(12,122,78,123,10,310,620,3,72,172,36,45,11,89)





# Corrélations entre variables explicatives (colinéarité possible entre
# les mesures de taille : POI, LON, LART, LAR)
round(cor(poissons[,2:15]), 2)


############################################################
# 2. TESTS : LES SITES SE DIFFÉRENCIENT-ILS ?
############################################################

# ANOVA variable par variable : p-value du test d'égalité des moyennes
# entre les 3 sites (H0 : pas de différence). On trie de la plus petite à la plus grande.
pval <- apply(poissons[,2:15], 2, function(x) summary(aov(x ~ poissons[,1]))[[1]][1,5])
round(sort(pval), 4)

# Test global sur les 14 variables à la fois (MANOVA, lambda de Wilks)
# H0 : les 3 vecteurs de moyennes sont égaux
summary(manova(as.matrix(poissons[,2:15]) ~ poissons[,1]), test = "Wilks")


############################################################
# 3. ANALYSE DISCRIMINANTE LINÉAIRE
############################################################

# Chaque groupe n'a que 7 ou 8 poissons pour 14 variables : les covariances
# de groupe sont singulières, donc on utilise l'analyse discriminante LINÉAIRE
# (matrice de covariance intra commune aux 3 groupes)
res.lda <- lda(poissons[,2:15], poissons[,1])
res.lda

res.lda$prior     # probabilités a priori (proportions des groupes)
res.lda$means     # moyennes des variables dans chaque groupe
res.lda$scaling   # coefficients des axes discriminants LD1 et LD2
# (nombre d'axes = min(14, 3 - 1) = 2)

# Part de la capacité discriminante portée par chaque axe
prop <- res.lda$svd^2 / sum(res.lda$svd^2)
round(prop, 3)


############################################################
# 4. REPRÉSENTATION DES INDIVIDUS SUR LE PLAN DISCRIMINANT
############################################################

pred <- predict(res.lda)
proj <- pred$x                    # coordonnées des poissons sur LD1 et LD2

plot(proj, col = as.numeric(poissons[,1]), pch = 19,
     xlab = "LD1", ylab = "LD2", main = "Poissons sur le plan discriminant")
text(proj, labels = rownames(poissons), pos = 3, cex = 0.7)
legend("topright", legend = levels(poissons[,1]), col = 1:3, pch = 19)

# Corrélations variables / axes discriminants : sert à interpréter LD1 et LD2
round(cor(poissons[,2:15], proj), 2)


############################################################
# 5. QUALITÉ DE LA CLASSIFICATION
############################################################

# Matrice de confusion sur l'échantillon d'apprentissage
# (lignes = site observé, colonnes = site prédit)
table(poissons[,1], pred$class)

# Taux d'erreur apparent
mean(pred$class != poissons[,1])

# Probabilités a posteriori d'appartenance à chaque site
round(pred$posterior, 3)

# Validation croisée leave-one-out : chaque poisson est classé par un modèle
# estimé sans lui -> estimation plus réaliste de l'erreur
res.cv <- lda(poissons[,2:15], poissons[,1], CV = TRUE)
table(poissons[,1], res.cv$class)
mean(res.cv$class != poissons[,1])


############################################################
# 6. SÉLECTION DE VARIABLES
############################################################

# Sélection pas à pas selon le lambda de Wilks (on garde une variable si p < niveau)
sel <- greedy.wilks(site ~ ., data = poissons, niveau = 0.1)
sel

# AD avec les variables retenues, puis même évaluation que ci-dessus
res.lda.sel <- lda(sel$formula, data = poissons)
res.lda.sel

pred.sel <- predict(res.lda.sel)
table(poissons[,1], pred.sel$class)
mean(pred.sel$class != poissons[,1])

res.cv.sel <- lda(sel$formula, data = poissons, CV = TRUE)
mean(res.cv.sel$class != poissons[,1])

