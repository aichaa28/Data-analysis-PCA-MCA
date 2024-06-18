library(FactoMineR)
library(factoextra)
library(readxl)
library(Factoshiny)
library(corrplot)


Formulaire_Musculation_réponses_ <- read_excel("Formulaire Musculation (réponses).xlsx")
variables_quantitatives1 <-Formulaire_Musculation_réponses_[, (11:20)]


# Nouveaux noms de colonnes
nouveau_noms_colonnes <- c("regime adapte", "equilibre nutri", "compréhension des besoins nutri", "impact mentale", "reduction stress", "effet positif sur mental", "amelioration qualite de vie physique", "progres physique", "amelioration de la sante physique", "impact positif sur autre aspects")

# Affecter les nouveaux noms de colonnes
colnames(variables_quantitatives1) <- nouveau_noms_colonnes


matrice_cor <- cor(variables_quantitatives1)


corrplot(matrice_cor,method="circle",type = "upper",tl.col = "black",tl.srt = 45)

# Réalisation de l'ACP
pca1 <- PCA(variables_quantitatives1, graph = FALSE)

# Affichage des résultats de l'ACP
summary(pca1)

# Carte des individus
plot(pca1, choix = "ind")

# Matrice de corrélation
matrice_cor <- cor(variables_quantitatives1)
corrplot(matrice_cor, method = "circle", type = "upper", tl.col = "black", tl.srt = 0, tl.cex = 0.7)
# Calcul de la PCA
pca_result <- prcomp(matrice_cor, scale. = TRUE, center = TRUE)
# Réduction à 3 dimensions
pca_reduc <- pca_result$x[, 1:3]
# Affichage des trois premières dimensions
print(pca_reduc)
# Création de nouvelles variables par thème
variables_quantitatives1_Nutrition <- rowMeans(variables_quantitatives1[, c(
  "regime adapte", "equilibre nutri" , "compréhension des besoins nutri" )])

variables_quantitatives1_BienEtreMental <- rowMeans(variables_quantitatives1[, c(
  "impact mentale", "reduction stress" , "effet positif sur mental")])

variables_quantitatives1_EffetsPhysiques <- rowMeans(variables_quantitatives1[, c(
  "amelioration qualite de vie physique", "progres physique", "impact positif sur autre aspects" , "amelioration de la sante physique")])
res_pca_theme <- PCA(data.frame(
  Nutrition = variables_quantitatives1_Nutrition,BienEtreMental = variables_quantitatives1_BienEtreMental,EffetsPhysiques = variables_quantitatives1_EffetsPhysiques), graph = FALSE)
#graph;
# Définir des couleurs pour les variables
colors <- c("blue","purple","pink")  # Ajoutez autant de couleurs que de variables/thèmes
plot(res_pca_theme, choix = "var",col.var = colors)  
summary(res_pca_theme)
plot(res_pca_theme, choix = "ind",title = "Projection des individus par theme")


#ACP2: 

variables_quantitatives2 <-Formulaire_Musculation_réponses_[, (27:37)]
# Nouveaux noms de colonnes
nouveau_noms_colonnes2 <- c("amelioration de la concentration", "concentration sur mouvements ", "role essentiel pour la concentration", "influence positive sur performances  ", "eviter les interactions sociales", "amelioration de la realisation des exos", "atteinte de meilleurs resultats","perseverance", "motivation", "volonte", "utilisation de playlist personalise")

# Affecter les nouveaux noms de colonnes
colnames(variables_quantitatives2) <- nouveau_noms_colonnes2
matrice_cor2 <- cor(variables_quantitatives2)
corrplot(matrice_cor2, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.6)

# Réalisation de l'ACP
pca2 <- PCA(variables_quantitatives2, graph = FALSE)
# Affichage des résultats de l'ACP
summary(pca2)
# Carte des individus et cercle 
plot(pca2, choix = "ind")
plot(pca2,choix="var",cex = 0.7)
# Création de nouvelles variables par thème
variables_quantitatives2_ConcentrationEtMental <- rowMeans(variables_quantitatives2[, c(
  "amelioration de la concentration","concentration sur mouvements ","role essentiel pour la concentration","eviter les interactions sociales")])

variables_quantitatives2_ReussiteEtRealisation <- rowMeans(variables_quantitatives2[, c(
  "amelioration de la realisation des exos",  "atteinte de meilleurs resultats",  "influence positive sur performances  " )])
variables_quantitatives2_Determination <- rowMeans(variables_quantitatives2[, c(
  "perseverance", "motivation", "volonte" )])

res_pca_theme2 <- PCA(data.frame(
  ConcentrationEtMental = variables_quantitatives2_ConcentrationEtMental,
  ReussiteEtRealisation = variables_quantitatives2_ReussiteEtRealisation,
  Determination = variables_quantitatives2_Determination
), graph = FALSE)

plot(res_pca_theme2, choix = "var",col.var = colors)  # Affiche le cercle des variables
summary(res_pca_theme2)
plot(res_pca_theme2, choix = "ind",title = "Projection des individus par theme")


