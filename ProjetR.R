data1=read.table("autos2.csv", dec=",",sep= ";",header=TRUE)
attach(data1)
#imposition de la structure des variables qualitative 
data1$FINITION=as.ordered(FINITION)
data1$Modele=as.factor(Modele)
#imposition de la structure  des variables quantitative continue
data1$CYL=as.double(CYL)
data1$PUISS=as.double(PUISS)
data1$LONG=as.double(LONG)
data1$LARG=as.double(LARG)
data1$POIDS=as.double(POIDS)
data1$V.MAX=as.double(V.MAX)
data1$PRIX=as.double(PRIX)
data1$R.POID.PUIS=as.double(R.POID.PUIS)
#Tableaux des frequence et effectis pour les variables qualitatives(Modele et FINITION)
TabEffec_Modele=table(Modele)
TabFreq_Modele=table(Modele)/length(Modele)
TabEffec_FINITION=table(FINITION)
TabFreq_FINITION=table(FINITION)/length(FINITION)

#tableaux des frequences et effectis pour les variables quantitatives
TabEffec_CYL=table(CYL)
TabFreq_CYL=table(CYL)/length(CYL)
TabEffec_PUISS=table(PUISS)
TabFreq_PUISS=table(PUISS)/length(PUISS)
TabEffec_LONG=table(LONG)
TabFreq_LONG=table(LONG)/length(LONG)
TabEffec_LARG=table(LARG)
TabFreq_LARG=table(LARG)/length(LARG)
TabEffec_POIDS =table(POIDS)
TabFreq_POIDS =table(POIDS)/length(POIDS)
TabEffec_V.MAX=table(V.MAX)
TabFreq_V.MAX=table(V.MAX)/length(V.MAX)
TabEffec_PRIX=table(PRIX)
TabFreq_PRIX=table(PRIX)/length(PRIX)
TabEffec_R.POID.PUIS=table(R.POID.PUIS)
TabFreq_R.POID.PUIS=table(R.POID.PUIS)/length(R.POID.PUIS)

# Representation graphique des variables qualitative
col=c("red", "yellow","green")
barplot(table(FINITION), col=col)
 


#representation graphique des variables quantitative 
#nous pouvons faire comme representations les histogrammes et des boxplot nous allons utiliser les boxplot ici 

boxplot(CYL,main="Histogramme de la variable CYL",col="blue")
boxplot(PUISS,main=" Boîte à moustaches  de la variable PUISS",col="pink")
boxplot(LONG,main="Boîte à moustaches de la variable LONG",col="yellow")
boxplot(LARG,main="Boîte à moustaches de la variable LARG",col="green")
boxplot(POIDS,main="Boîte à moustaches de la variable POIDS",col="yellow")
boxplot(V.MAX,main="Boîte à moustaches",col="blue")
boxplot(PRIX,main="Boite a moustache de la variable PRIX",col="yellow")
boxplot(R.POID.PUIS,main="Boite a moustache de la variable R.POID.PUIS",col="green")
print("Les variables CYL ET POIDS presente uniquement des valeurs extremes d'apres le boxplot")
#les indicateurs de position uniquement pour les variables quantitatives: Mode, Moyenne,médiane, quartiles

#****CYL*****
summary(CYL)
Mode_CYL=names(which.max(table(CYL)))
cat("le cylindre moyens pour une voiture est de 1632.\n on constate que au moins  25% des voitures ont un cylindre inferieur ou egale  a 1310 , au moins 50% ont un cylindre inferieur ou egales a 1578 et  75% ont un cylindre inferieur a 1798  ")
cat("le mode de CYL est egale a 1294 donc on peut dire que la  majorite des voitures utilise le cylindre 1294")

#Centile d’ordre t%, t compris entre 1 et 100 > quantile(age,t/100
centile_CYL=quantile(CYL,1/100)
#Décile d’ordre t%, t compris entre 1 et 10 > quantile(age,t/10)
decile_CYL=quantile(CYL,1/10)
#****PUISS
summary(PUISS)
Mode_PUISS=names(which.max(table(PUISS)))

#Centile d’ordre t%, t compris entre 1 et 100 > quantile(age,t/100
centile_PUISS=quantile(PUISS,1/100)
#Décile d’ordre t%, t compris entre 1 et 10 > quantile(age,t/10)
decile_PUISS=quantile(PUISS,1/10)

#***LONG
summary(LONG)
Mode_LONG=names(which.max(table(LONG)))
#Centile d’ordre t%, t compris entre 1 et 100 > quantile(age,t/100
centile_LONG=quantile(LONG,1/100)
#Décile d’ordre t%, t compris entre 1 et 10 > quantile(age,t/10)
decile_LONG=quantile(LONG,1/10)
#****LARG
Moy_LARG=mean(LARG)
Mode_LARG=names(which.max(table(LARG)))
median_LARG=median(LARG)
quart_LARG = quantile(LARG,probs=c(0.25, 0.5, 0.75))
#Centile d’ordre t%, t compris entre 1 et 100 > quantile(age,t/100
centile_LARG=quantile(LARG,1/100)
#Décile d’ordre t%, t compris entre 1 et 10 > quantile(age,t/10)
decile_LARG=quantile(LARG,1/10)
#***POIDS
Moy_POIDS=mean(POIDS)
Mode_POIDS=names(which.max(table(POIDS)))
median_POIDS=median(POIDS)
quart_POIDS = quantile(POIDS,probs=c(0.25, 0.5, 0.75))
#Centile d’ordre t%, t compris entre 1 et 100 > quantile(age,t/100
centile_POIDS=quantile(POIDS,1/100)
#Décile d’ordre t%, t compris entre 1 et 10 > quantile(age,t/10)
decile_POIDS=quantile(POIDS,1/10)

#****V.MAX
Moy_V.MAX=mean(V.MAX)
Mode_V.MAX=names(which.max(table(V.MAX)))
median_V.MAX=median(V.MAX)
quart_V.MAX = quantile(V.MAX,probs=c(0.25, 0.5, 0.75))
#Centile d’ordre t%, t compris entre 1 et 100 > quantile(age,t/100
centile_V.MAX=quantile(V.MAX,1/100)
#Décile d’ordre t%, t compris entre 1 et 10 > quantile(age,t/10)
decile_V.MAX=quantile(V.MAX,1/10)
#**R.POID.PUISS
Moy_R.POID.PUIS=mean(R.POID.PUIS)
Mode_R.POID.PUIS=names(which.max(table(R.POID.PUIS)))
median_R.POID.PUIS=median(R.POID.PUIS)
quart_R.POID.PUIS = quantile(R.POID.PUIS,probs=c(0.25, 0.5, 0.75))
#Centile d’ordre t%, t compris entre 1 et 100 > quantile(age,t/100
centile_R.POID.PUIS=quantile(R.POID.PUIS,1/100)
#Décile d’ordre t%, t compris entre 1 et 10 > quantile(age,t/10)
decile_R.POID.PUIS=quantile(R.POID.PUIS,1/10)
# Indicateurs de dispersion : variance, ecart-type et coefficients de variation
#**CYL 
variance_CYL=var(CYL)
sd_CYL = sqrt(variance_CYL)
covariation_CYL= sd_CYL/mean(variance_CYL)
#***PUISS
variance_PUISS=var(PUISS)
sd_PUISS = sqrt(variance_PUISS)
covariation_PUISS= sd_PUISS/mean(variance_PUISS)
#***LONG
variance_LONG=var(LONG)
sd_LONG = sqrt(variance_LONG)
covariation_LONG= sd_LONG/mean(variance_LONG)
#***LARG
variance_LARG=var(LARG)
sd_LARG = sqrt(variance_LARG)
covariation_LARG= sd_LARG/mean(variance_LARG)
#**POIDS
variance_POIDS=var(POIDS)
sd_POIDS = sqrt(variance_POIDS)
covariation_POIDS= sd_POIDS/mean(variance_POIDS)
#****V.MAX
variance_V.MAX=var(V.MAX)
sd_V.MAX = sqrt(variance_V.MAX)
covariation_V.MAX= sd_V.MAX/mean(variance_V.MAX)
#**PRIX
variance_PRIX=var(PRIX)
sd_PRIX = sqrt(variance_PRIX)
covariation_PRIX= sd_PRIX/mean(variance_PRIX)
#**R.POID.PUIS
variance_POIDS=var(POIDS)
sd_POIDS = sqrt(variance_POIDS)
covariation_POIDS= sd_POIDS/mean(variance_POIDS)

#2)Analyse Bivarier ***ssss
#***Prix_VS_Modele
boxplot(PRIX ~ Modele,main="Boîte à moustaches entre de la relation Modele et Prix  ",col="green")
print("d'apres le boxplot il existe une relation entre la variable prix et Modele")
#d'apres la boite a moustache il existe une relation entre le prix et le Modele de voitrue(on constate que les voitures Renault cout beaucoup plus chere que les autres modeles)
#*****Prix vs Finition
boxplot(PRIX ~ FINITION,main="Relation entre la Finition  et le prix ",col="green")
print("d'apres le boxplot il exise egalement une relation entre le prix et la Finition")

#***PRIXVSCYL
plot(PRIX ~ CYL,main="Nuages de points entre Prix et CYL",col="blue")
abline(lm(PRIX ~ CYL),col='red')
cord=round(cor(CYL,PRIX),2)
print(paste("le coefficient de correlation entre le CYL et Prix est  :", cord))
print("d'apres le nuage de point il existe une relation entre le prix et CYL .d'apres le coefficient de correlation cette relation est forte")
#***PRIX VS PUISS
plot(PRIX~PUISS,main="Nuages de points entre Prix et CYL",col="blue")
abline(lm(PRIX~PUISS),col='red')
cord=round(cor(PRIX,PUISS),2)
print(paste("le coefficient de correlation entre le PUISS et Prix est  :", cord))
print("d'apres le nuage de point il existe une relation entre le prix et PUISS .d'apres le coefficient de correlation cette relation est tres forte")

#******PRIX vs LONG
plot(PRIX ~ LONG,main="Nuages de points entre Prix et LONG",col="blue")
abline(lm(PRIX ~ LONG),col='red')
cord=round(cor(PRIX,LONG),2)
print(paste("le coefficient de correlation entre le LONG et Prix est  :", cord))
print("d'apres le nuage de point il existe une relation entre le PRIX et LONG .d'apres le coefficient de correlation cette relation est forte")

#***PRIX vs LARG
plot(PRIX ~ LARG,main="Nuages de points entre Prix et LARG",col="blue")
abline(lm(PRIX ~ LARG),col='red')
cord=round(cor(PRIX,LARG),2)
print(paste("le coefficient de correlation entre le LARG et Prix est  :", cord))
print("d'apres le nuage de point il existe une relation entre le PRIX et LARG .d'apres le coefficient de correlation cette relation est faible")

#***PRIX VS POIDS
plot(PRIX ~ POIDS,main="Nuages de points entre Prix et POIDS",col="blue")
abline(lm(PRIX ~ POIDS),col='red')
cord=round(cor(PRIX,POIDS),2)
print(paste("le coefficient de correlation entre le LARG et Prix est  :", cord))
print("d'apres le nuage de point il existe une relation entre le PRIX et LARG .d'apres le coefficient de correlation cette relation est forte")

#***PRIX VS V.MAX
plot(PRIX ~ V.MAX,main="Nuages de points entre Prix et V.MAX",col="blue")
abline(lm(PRIX ~ V.MAX),col='red')
cord=round(cor(PRIX,V.MAX),2)
print(paste("le coefficient de correlation entre le LARG et Prix est  :", cord))
print("d'apres le nuage de point il existe une relation entre le PRIX et LARG .d'apres le coefficient de correlation cette relation est faible")


#***PRIX VS R.POID.PUIS
plot(PRIX ~ R.POID.PUIS,main="Nuages de points entre Prix et R.POID.PUIS",col="blue")
abline(lm(PRIX ~ R.POID.PUIS),col='red')
cord=round(cor(PRIX,R.POID.PUIS),2)
