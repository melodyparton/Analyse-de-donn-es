age <- c(17, 28, 64, 8, 25, 36)
print(age)
sexe <- c("H","F","F","H","H","F") 
print(sexe)
donnees <- data.frame(age, sexe)
print(donnees)
x <- donnees[3,1]
print(x)
x <- donnees[4,]
print(x)
x <- donnees[ ,2]
print(x)

