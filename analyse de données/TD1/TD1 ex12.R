parthenaise <- c( 53, 49, 40 ,48, 43, 42, 43 ,46, 42, 43,38, 40, 50, 44)
charolaise <- c( 46, 46, 48, 38 ,42, 42, 40 ,53, 55, 41 ,47, 30)
print(mean(parthenaise))
print(mean(charolaise))
print(sd(parthenaise))
print(sd(charolaise))
var.test(parthenaise,charolaise)#on fait le test p-value et tout pour savoir si les variances sont égales (H0)
#et enft pvalue = 0.1239 > 0.05 -> on ne peut pas rejeter l'hypothèse nulle
#de plus , confidence interval for the ratio contains 1 -> the variances are not significantly different (ratio 1 -> equal variances)
