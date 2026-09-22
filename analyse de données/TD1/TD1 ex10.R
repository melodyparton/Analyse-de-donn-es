poids <- chickwts$weight
print(poids)
print(mean(poids))#moyenne
print(var(poids))#variance
print(sd(poids))#écart-type 
print(hist(poids))#histogramme
print(boxplot(poids)) #boxplot
print(median(poids))#mediane
print(quantile(poids,0.5))#mediane mais avec les quantiles
print(quantile(poids,0.25))#premier quartile
print(quantile(poids,0.75))#troisième quartile


nourriture <- chickwts$feed
print(mode(nourriture))

