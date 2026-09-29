# ============================================================
# Exercice 1 : AFC sur les données Universités (2007-2008)
# ============================================================

# install.packages(c("FactoMineR", "factoextra"))  # à faire une seule fois
library(FactoMineR)
library(factoextra)

# ---- 1. Import des données --------------------------------
# Adapter le chemin (ou utiliser Session > Set Working Directory)
univ <- read.csv2("universite.csv", fileEncoding = "latin1", row.names = 1)
str(univ)
head(univ)

# Noms plus courts pour les disciplines (lisibilité des graphiques)
rownames(univ) <- c("Droit-SP", "Eco-Gestion", "AES", "Lettres-Arts",
                    "Langues", "SHS", "Pluri-lettres", "Sciences-fond",
                    "Sciences-vie", "STAPS")

# ---- 2. Tableau de contingence actif ----------------------
# Colonnes 1 à 6  : Licence/Master/Doctorat x F/H  (actives)
# Colonnes 7 à 12 : totaux (supplémentaires, non utilisées pour les axes)
names(univ)

# ---- 3. Test du chi2 d'indépendance -----------------------
tab <- as.matrix(univ[, 1:6])
chi <- chisq.test(tab)
chi                      # X-squared très grand, p-value < 2.2e-16
sum(tab)                 # effectif total n

# ---- 4. AFC -----------------------------------------------
res <- CA(univ, col.sup = 7:12, graph = FALSE)

# ---- 5. Valeurs propres / choix du nombre d'axes ----------
res$eig
fviz_screeplot(res, addlabels = TRUE, ylim = c(0, 80))
# Inertie totale = chi2 / n
chi$statistic / sum(tab)

# ---- 6. Plan factoriel 1-2 : lignes et colonnes -----------
fviz_ca_biplot(res, axes = c(1, 2), repel = TRUE)
fviz_ca_row(res, axes = c(1, 2), repel = TRUE)
fviz_ca_col(res, axes = c(1, 2), repel = TRUE)

# Plan 1-3 (pour les disciplines mal représentées dans le plan 1-2)
fviz_ca_biplot(res, axes = c(1, 3), repel = TRUE)

# ---- 7. Aides à l'interprétation --------------------------
# Coordonnées
round(res$row$coord[, 1:3], 3)
round(res$col$coord[, 1:3], 3)

# Contributions (en %)
round(res$row$contrib[, 1:3], 1)
round(res$col$contrib[, 1:3], 1)

# Qualité de représentation (cos2)
round(res$row$cos2[, 1:3], 2)
round(res$col$cos2[, 1:3], 2)

# Graphiques de contributions / cos2
fviz_contrib(res, choice = "row", axes = 1)
fviz_contrib(res, choice = "row", axes = 2)
fviz_contrib(res, choice = "col", axes = 1)
fviz_contrib(res, choice = "col", axes = 2)
fviz_ca_row(res, col.row = "cos2", repel = TRUE,
            gradient.cols = c("grey70", "orange", "red"))

# Éléments supplémentaires (Total-F, Total-H, Total-Licence, ...)
round(res$col.sup$coord[, 1:3], 3)
fviz_ca_biplot(res, repel = TRUE)   # les supplémentaires apparaissent en pointillés

# ---- 8. Profils lignes / colonnes -------------------------
# Profils-lignes : répartition (%) de chaque discipline sur les 6 modalités
round(100 * prop.table(tab, 1), 1)

# Profil moyen (ligne "marge")
round(100 * colSums(tab) / sum(tab), 1)

# ---- 9. Réponses aux questions ----------------------------
# Q1 : disciplines au profil proche -> points proches sur le plan factoriel
# Q2 : disciplines féminines / masculines
part_F <- round(100 * univ$Total.F / univ$Total, 1)
names(part_F) <- rownames(univ)
sort(part_F, decreasing = TRUE)

# Q3 : longueur des études
part_MD  <- round(100 * (univ$Total.Master + univ$Total.Doctorat) / univ$Total, 1)
part_Doc <- round(100 * univ$Total.Doctorat / univ$Total, 1)
names(part_MD) <- names(part_Doc) <- rownames(univ)
data.frame(Master_Doctorat = part_MD, Doctorat = part_Doc)[order(-part_MD), ]

# Représentation graphique du niveau d'études par discipline
barplot(t(100 * prop.table(as.matrix(univ[, c("Total.Licence", "Total.Master",
                                              "Total.Doctorat")]), 1)),
        legend.text = c("Licence", "Master", "Doctorat"),
        las = 2, cex.names = 0.7, ylab = "% des étudiants",
        args.legend = list(x = "topright", cex = 0.7))

