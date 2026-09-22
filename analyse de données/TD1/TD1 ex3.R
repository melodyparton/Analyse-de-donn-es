suite2 <- seq(1, 12, 2)
is.vector(suite2)


vecteur1 <- c(5 ,6, 1, 4 ,5)
print(vecteur1)
mode(vecteur1) #donne le type d'un vecteur (numerable, etc)
length(vecteur1)
is.vector(vecteur1)
print(vecteur1[2])
print(vecteur1[2:4])


vecteur2 <- c("bleu","jaune","vert")
print(vecteur2)
mode(vecteur2)
length(vecteur2)

vecteur3 <- c(T, T, F, T, F, F)
print(vecteur3)
mode(vecteur3)
length(vecteur3)

x <- c(2, 3, 6, 10, 12)
y <- c(1, 6, 7, 2, 1)
z <- x + y

2 * x + 5
(x + y)/2
a = c(x[2],x[4])


indices_rm_x = c(2,3) #on crée un vecteur avec tous les indices qu'on veut enlever
x <- x[-indices_rm_x] #on enlève les éléments des indices dans le vecteur indices_rm_x
indices_sup_a_4 = y > 5 #tous les élements de y qui sont > 4
sup_a_4 <- y[indices_sup_a_4] #on les extrait de y
print("x sans modif : ")
print(x)
x[x >= 5] <- 20 #tous les éléments de x supérieurs à 5 sont remplacés par 20
print("x en modifiant tous ceux supérieursà 5 : ")
print(x)

donnees <- 1
vecteur_de_50_1 <- rep(x = donnees, times = 50) #on crée un vecteur qui contient 50 fois un 1
print(vecteur_de_50_1)

donnees <- "MINES"
vecteur_de_mines <- rep(x = donnees, times = 5) 
print(vecteur_de_mines)

print(sup_a_4)
print(x)
print(a)
