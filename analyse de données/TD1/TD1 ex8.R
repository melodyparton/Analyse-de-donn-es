tab1 <- read.table("table1.txt")
print(tab1)
tab2 <- read.table("table2.txt",header = T) #le nom des colonnes est inscrit dans le fichier
print(tab2)
tab3 <- read.table("table3.txt",dec= ",") #on remplace les , par des .
print(tab3)
tab4 <- read.csv("table4.csv")
print(tab4)
