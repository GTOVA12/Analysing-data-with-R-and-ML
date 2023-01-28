#Etape1 : Définition de la problématique et des données :

#Problématique : Étude statistique de la perception des élèves ingénieurs sur la relation entre la double diplomation et le marché de travail. 
#Population : étudiants ENSAKistes de la première et deuxième année de la filière mécatronique d’automobile.
#Question de recherche : Que pensent les étudiants par rapport à l'influence de la double diplomation sur le marché du travail?
#Hypothèse de recherche : Les élèves pensent que la double diplomation affecte positivement le marché du travail.

#Etape2 :Importation des données

library(readxl)
M <- read_excel("C:/Users/farou/Pictures/Screenshots/STAT/M.xlsx")
View(M)
#Etap3 : Pré-traitement
#3.1 : conversion

M$age[3]=25
M$age[11]=20
M$age[29]=21
if(is.character(M$age))
  M$age=as.integer(M$age)

if(is.character(M$Genre))
  M$Genre=factor(M$Genre)

if(is.character(M$A_E))
  M$A_E=factor(M$A_E)

if(is.character(M$MT_item1))
  M$MT_item1=factor(M$MT_item1 )



if(is.character(M$MT_item2))
  M$MT_item2=factor(M$MT_item2)


if(is.character(M$MT_item3))
  M$MT_item3=factor(M$MT_item3)

if(is.character(M$For_item1))
  M$For_item1=factor(M$For_item1)

if(is.character(M$ITEM1))
  M$ITEM1=factor(M$ITEM1)

if(is.character(M$ITEM2))
  M$ITEM2=factor(M$ITEM2)

if(is.character(M$ITEM3))
  M$ITEM3=factor(M$ITEM3)

if(is.character(M$ITEM4))
  M$ITEM4=factor(M$ITEM4)







#3.2 nettoyage
#3.2.1 Valeur abberantes

boxplot(M$age)
#on voit dans ce dessin déja que y'as des valeurs qui sont appartiennent aux valeurs normaux
C=boxplot.stats(M$age)$out
C
#cette algorithme ci-dessous détecte les VA et les convertis en NA

for(i in 1:length(M$age))
{
  for(j in 1:length(C))
  {
    if(!is.na(M$age[i]))
    {
      if(M$age[i]==C[j])
      {
        M$age[i]=NA
      }
    }
  }
} 
#3.2.2 traitement des valeurs manquantes : on est dans le cas ou on peut pas remplacer

L=sum(is.na(M$age))
P=L/length(M$age)
P
if(P<0.05)
{
  print('On va supprimer les samples qui ont NA mais pas que la cellules qui contient cette valeur mais la ligne toute entière')
  library(dplyr)
  D_sans_NA = D %>%
    dplyr::na.omit()
  View(D_sans_NA)
}
if(P>0.05)
{
  print('on doit estimer')
  for(i in 1:length(M$age))
  {
    if(is.na(M$age[i]))
    {
      M$age[i]=mean(M$age,na.rm = TRUE)
    }
  }
}

# si on regarde les résultats on verra que l'age est un double alors qu'il est censé etre integer donc on va faire la conversion
#de nouveau
if(is.numeric(M$age))
  M$age=as.integer(M$age)


#3.3 normalités
options(scipen=999)
shapiro.test(M$age)
hist(M$age)

#on a trouvé P-value treéééééés inférieur a 5% donc on peut dire sans hésiter que H1 est majoritaire =>donc notre distribution 
#ne suit pas la distribution théorique qui elle suit effectivement la loi normale => d'ou la nécessité de savoir le pourquoi
#je pense parce que notre population est trés centré sur un age donc c'est comme si on a une distribution de dirac

#on va voir si on a la quasi normalités avec le test de l'inclinaison et l'appaltissement
library(moments)
skewness(M$age)
kurtosis(M$age)
#donc on a une distribution qui est quasi-normale qui est incliner vers la gauche(skeweness>0) et que kurtosis est proche de 3 
-------------------------------------------------------------------------------------------------
  

#########################################Etape4:Analyse de données#####################################################



summary(M$Genre)

# le test d'hypoyhéses de KHI-2 pour savoir si notre échantillon est équilibré au niveau des variables qualitatifs

#on a essentiellement deux Variables qui coupent notre population a savoir Année d'études(CI1,CI2) et le genre 

chisq.test(table(M$Genre))

#on a un P-value supérieur largementttt à 5% autrement dit il n yas pas de différence significative entre la distribution théorique ou
#on a un équilibre pure et notre distribution=> le nombre des hommes est un peu prés égale a celui des femmes

table(M$A_E)
chisq.test(table(M$A_E))

#on sort avec la meme conclusion que précedemment a savoir qu'on accepte l'hypothèse nulle => il ny as pas de différence significatie entre la distribution
#théorique et celle observé=>on a un peu prét le meme nombre d'invidus de chaque cotés "CI1,CI2"

#on va aller plus loin cad on va voir si le nombre des hommes et des femmes présent dans CI1 est le meme que celui présent dans CI2

#meme si on est pas dans la partie bivariés mais faut qu'on tranche par rapport a la nature de notre échantillonage 

X=table(M$Genre,M$A_E) 
X
chisq.test(X)

library(questionr)
cramer.v(X)

#ca nous prouve de plus que y 'as de relation vu que le v de cramer est trés petit
#d'aprés ce test on peut dire qu'on a H0 qui est accepté donc on a vraiment un équilibre parfait dans notre distribution 
-------------------------------------------------------------------------------------------------------------------
#ON PEUT DIRE clairement que on a un échantillon exploratoire
  
  
summary(M$age)

#l'age minimum est 19 le max c'est 22,le premier quartile qui  représente 25% de l'échantillon est 20 asn le deuxieme quartile est
#la médiane qui sépare la distribution en 2parties égales (il est proches de la moyenne) et le troisième quartile a qui lui représente 75%


table(M$MT_item1)#y as 28% des gens qui pensent que y'as pei d'offres

table(M$MT_item2)#l'écrasante majorité pensent que le covid a laissé des traces négatifs sur le marché du travail

table(M$MT_item3)#les étudiants penchent plus à l'idée qui dit que y'as peu de demandes

table(M$For_item1)#on voit une répartition presque équitable sur les deux réponses a savoir (une option qu'il faut bien réfléchir a ,une bonne option)

table(M$ITEM1)#y as un grand nombre qui pense que la DD facilite l'intégration au marché du travail

table(M$ITEM2)#pareil les gens ont tendance a pensé que la DD permet d'accéder a un marché plus large

table(M$ITEM3)#les étudiantes du CI1 et CI2 pensent que la DD permet d'avoir une experience professionel enrichissante 

table(M$ITEM4)#y'as peu de gens qui pensent que l'ffet de la DD sur l'étudiant est faible


#4.2 Statistique bi-variées

#je vais voir la relation entre A_E et  for_item1

####################Relation de A_E############################"
table(M$A_E,M$For_item1)

#chisq.test(table(M$A_E,M$For_item1))
fisher.test(table(M$A_E,M$For_item1))

xx=table(M$A_E,M$For_item1)
library(rcompanion)
freemanTheta(xx,group="row")#c'est utilisée for the significance of association between nominal-ordinal variables
#on peut trouvé aussi epsilon-squared
epsilonSquared(xx,group="row")
#on a trouvé 0.134 dans le deuxiéme test donc on peut dire que on a une relation moderer c'est un 
#test régide vu que a partir de 0.36 on peut dire que y as une relation forte


require(ggplot2)
require(ggthemes)

ggplot(data=M, aes(x=M$A_E,fill = M$For_item1))+
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(M$For_item1)

#on voit clairement dans le graphe que les CI1 sont plus nombreux a penser que
#c'est une bonne option ,alors que les CI2 pense plutot qu'il faut se poser 
#et bien réfléchir a cet option avant de prendre n'importe quel décision ils sont dans le mode réflexion



#chisq.test(table(M$A_E,M$MT_item1))
fisher.test(table(M$A_E,M$MT_item1))
#H0 est accepté=>IL n y as pas d'association=> on peut pas dire que les CI1 ont une pensée différente que les CI2 par rapport au faite que le marché de travail a connu une regression

#on fait le meme style d'interprétation

#chisq.test(table(M$A_E,M$MT_item2))
fisher.test(table(M$A_E,M$MT_item2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$A_E,M$MT_item3))
fisher.test(table(M$A_E,M$MT_item3))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$A_E,M$ITEM1))
fisher.test(table(M$A_E,M$ITEM1))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$A_E,M$ITEM2))
fisher.test(table(M$A_E,M$ITEM2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$A_E,M$ITEM3))
fisher.test(table(M$A_E,M$ITEM3))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$A_E,M$ITEM4))
fisher.test(table(M$A_E,M$ITEM4))

#H1 est accepté=>IL y as une asociation=> qui dit ca dit que les etudiants en CI1 pensent différement que les CI2 par rapport au faite que
#la double diplomation a un effet faible sur l'étudiant en sois

#voyons d'abord la force de ce lien
yy=table(M$A_E,M$ITEM4)
library(rcompanion)
freemanTheta(yy,group="row")
epsilonSquared(yy,group="row")#c'est un lien pas faible mais pas fort aussi ,médiocre

#voyons ce que ca va donner en détail

ggplot(data=M, aes(x=M$A_E,fill = M$ITEM4))+
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(M$ITEM4)

#on voit déja que la majorité qui ont pas de penser par rapport a l'effet de la double diplomation sur les étudiants sont les CI1
#en plus de ca l'effectif qui reste du CI1 sont pas d'accord ou voir meme pas du tout d'accord ce qui confirme clairement 
#la conclusion qu'on a sortie avec comme quoi les CI1 tend plus a penser que la DD est une bonne option
#faut aussi voir que ceux qui pensent que l'effet est faible sont faible en termes d'effectifs et sont que des CI2

####################Relation de Genre############################"


#chisq.test(table(M$Genre,M$For_item1))
fisher.test(table(M$Genre,M$For_item1))
#H0 est accepté => il n y as pas d'association => donc on peut pas dire que les femmes ont ont une vision plus positif que les hommes sur la double diplomation et vice versa

#chisq.test(table(M$Genre,M$MT_item1))
fisher.test(table(M$Genre,M$MT_item1))
#H0 est accepté=>IL n y as pas d'aasociation

#chisq.test(table(M$Genre,M$MT_item2))
fisher.test(table(M$Genre,M$MT_item2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$Genre,M$MT_item3))
fisher.test(table(M$Genre,M$MT_item3))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$Genre,M$ITEM1))
fisher.test(table(M$Genre,M$ITEM1))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$Genre,M$ITEM2))
fisher.test(table(M$Genre,M$ITEM2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$Genre,M$ITEM3))
fisher.test(table(M$Genre,M$ITEM3))
#H0 est accepté=>IL n y as pas d'asociation

chisq.test(table(M$Genre,M$ITEM4))
fisher.test(table(M$Genre,M$ITEM4))
#H0 est accepté=>IL n y as pas d'asociation entre les 2 variables












####################Relation de MT_item1###############################






#chisq.test(table(M$MT_item1,M$MT_item2))
fisher.test(table(M$MT_item1,M$MT_item2))
#H1 est accepté => il y as une association entre ces deux variables => donc 

library(questionr)
cramer.v(table(M$MT_item1,M$MT_item2))# ca me donne une erreur c'est du au faite qu'on a pas atteint un effectif>5 sur toute les collonnes

library(DescTools)
#ce sont des coefficients qui étudie la nature de la relation entre deux variables qualitatifs ordinales
LL=table(M$MT_item1,M$MT_item2)
SomersDelta(LL,direction ="column",conf.level=0.95)
KendallTauB(LL,conf.level = 0.95)#donc le tau_b est >0.10 donc la relation est faible mais pas trés faible vu que tau_b n'est pas inférieur a 0.10
GoodmanKruskalGamma(LL,conf.level = 0.95)
#ce sont tous des tests qui nous permettent de voir la force de la liaison entre les 2 variables ordinalesssssss



#chisq.test(table(M$MT_item1,M$MT_item3))
fisher.test(table(M$MT_item1,M$MT_item3))
#H0 est accepté pour les 2 tests=>IL n y as pas d'asociation

#chisq.test(table(M$MT_item1,M$ITEM1))#ca me donne H1 accepté:erreur vient du faite que l'effectif marginal du tableau de contingence est<5
fisher.test(table(M$MT_item1,M$ITEM1))#ca  me donne H1 refusé 
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$MT_item1,M$ITEM2))
fisher.test(table(M$MT_item1,M$ITEM2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$MT_item1,M$ITEM3))
fisher.test(table(M$MT_item1,M$ITEM3))
#H0 est accepté=>IL n y as pas d'asociation


#chisq.test(table(M$MT_item1,M$ITEM4))
fisher.test(table(M$MT_item1,M$ITEM4))
#H0 est accepté=>IL n y as pas d'asociation



require(ggplot2)
require(ggthemes)

ggplot(data=M, aes(x=M$MT_item1,fill = M$MT_item2))+
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(M$MT_item2)


#c'est un graphe qui explique la relation qui lie les 2 



####################Relation de MT_item2###############################


fisher.test(table(M$MT_item2,M$For_item1))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$MT_item2,M$MT_item3))
fisher.test(table(M$MT_item2,M$MT_item3))
#H0 est accepté=>IL n y as pas d'asociation

chisq.test(table(M$MT_item2,M$ITEM1))#ca me donne H1 accepté:erreur vient du faite que l'effectif marginal du tableau de contingence est<5
fisher.test(table(M$MT_item2,M$ITEM1))#ca  me donne H1 refusé mais il est trés proche de 5%:=> donc je vais voir la force de liaison


LL2=table(M$MT_item2,M$ITEM1)
library(DescTools)
#ce sont des coefficients qui étudie la nature de la relation entre deux variables qualitatifs ordinales

KendallTauB(LL2,conf.level = 0.95)#donc le tau_b est égale a 0.17 donc la relation est médiocre ,ce test est stricte.

ggplot(data=M, aes(x=M$MT_item2,fill = M$ITEM1))+
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(M$ITEM1)



#pour rappel MT_item2:>le marché de travail a connu une baisse drastique

#ITEM1:la DD facilite l'intégration au marché du travail

#on remarque que la majorité ceux qu sont d'accord que le marché du travail a connu une baisse drastique c'est ceux qui pensent
#que la DD facilite l'intégration


#chisq.test(table(M$MT_item2,M$ITEM2))
fisher.test(table(M$MT_item2,M$ITEM2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$MT_item2,M$ITEM3))
fisher.test(table(M$MT_item2,M$ITEM3))
#H0 est accepté=>IL n y as pas d'asociation:on a favoriser le test de fisher


#chisq.test(table(M$MT_item2,M$ITEM4))
fisher.test(table(M$MT_item2,M$ITEM4))

#H0 est accepté=>IL n y as pas d'asociation







###################Relation de MT_item3###############################


fisher.test(table(M$MT_item3,M$For_item1))
#H0 est accepté=>IL n y as pas d'asociation



#chisq.test(table(M$MT_item3,M$ITEM1))
fisher.test(table(M$MT_item3,M$ITEM1))
#H0 est accepté=>IL n y as pas d'asociation


#chisq.test(table(M$MT_item2,M$ITEM2))
fisher.test(table(M$MT_item3,M$ITEM2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$MT_item2,M$ITEM3))
fisher.test(table(M$MT_item3,M$ITEM3))
#H0 est accepté=>IL n y as pas d'asociation:on a favoriser le test de fisher


#chisq.test(table(M$MT_item2,M$ITEM4))
fisher.test(table(M$MT_item3,M$ITEM4))

#H0 est accepté=>IL n y as pas d'asociation



 

###################Relation de For_item1###############################


#chisq.test(table(M$For_item1,M$ITEM1))
fisher.test(table(M$For_item1,M$ITEM1))
#H0 est accepté=>IL n y as pas d'asociation


#chisq.test(table(M$For_item1,M$ITEM2))
fisher.test(table(M$For_item1,M$ITEM2))
#H0 est accepté=>IL n y as pas d'asociation

#chisq.test(table(M$For_item1,M$ITEM3))
fisher.test(table(M$For_item1,M$ITEM3))
#H0 est accepté=>IL n y as pas d'asociation:on a favoriser le test de fisher


#chisq.test(table(M$For_item1,M$ITEM4))
fisher.test(table(M$For_item1,M$ITEM4))
#H0 est accepté=>IL n y as pas d'asociation



###################Relation de ITEM1###############################

# pour rappel
#ITEM1:La DD facilite l'intégration au marché du travail dans le secteur mécatronique.
#ITEM2:La DD permet d'accéder à un marché plus large.
#ITEM3:La DD donne une expérience internationale enrichissante.
#ITEM4:La DD a un effet faible sur l'image de l'ingenieur
  
#chisq.test(table(M$ITEM1,M$ITEM2))
fisher.test(table(M$ITEM1,M$ITEM2))
#H1 est accepté=>IL y as UNE d'asociation entre ces 2 ITEMS

#voyant la force de ce lien :> on a deux variables quali qui sont ordinales
LL3=table(M$ITEM1,M$ITEM2)
library(DescTools)

KendallTauB(LL2,conf.level = 0.95)
#c'est un lien faible entre les 2 qu'on peut voir avec ce graphe ci dessous



ggplot(data=M, aes(x=M$ITEM1,fill = M$ITEM2))+
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(M$ITEM2)
#on voit que ceux qui était tout a fait d'accord ou bien d'accord sont essentiellemnt
#ceux qui était d'accord et tout a fait d'accord sur le faite que la DD nous fait intégrer un marché plus large




#chisq.test(table(M$ITEM1,M$ITEM3))
fisher.test(table(M$ITEM1,M$ITEM3))
#H0 est accepté=>IL n y as pas d'asociation:on a pris la décision a la base du test de khi-2


#chisq.test(table(M$ITEM1,M$ITEM4))
fisher.test(table(M$ITEM1,M$ITEM4))
#H1 est accepté=>IL y as une  asociation qui lie les deux variables

#on teste ici la force du lien existant
LL3=table(M$ITEM1,M$ITEM4)
KendallTauB(LL3,conf.level = 0.95)
#ici on peut dire qu'il existe un lien fort strong entre ces deux Variables 

ggplot(data=M, aes(x=M$ITEM1,fill = M$ITEM4))+
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(M$ITEM4)

#déja la première remarque que j'ai fait c'est que y'as clairement 3 distribution normales ce qui est trés bien ca reflete la réalité
#ca montre que notre étude est faite de la bonne facon,c'est réjouissant comme résultats

#deuxiement je remarque que ceux qui sont d'accord sur  le faite que la DD permet l'intégration a un maché plus large 
#sont ceux qui admette pas et refuse l'idée que la DD a un effet faible sur l'étudiant



###################Relation de ITEM2###############################





#chisq.test(table(M$ITEM2,M$ITEM3))
fisher.test(table(M$ITEM2,M$ITEM3))
#H1 est accepté=>IL y as une d'asociation:on a favoriser le test de fisher

LL4=table(M$ITEM2,M$ITEM3)
library(DescTools)

KendallTauB(LL4,conf.level = 0.95)
#c'est un lien forttttttt entre les 2 qu'on peut voir avec ce graphe ci dessous



ggplot(data=M, aes(x=M$ITEM2,fill = M$ITEM3))+
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(M$ITEM3)

#on voit que ceux qui pensent que la DD permet d'y accéder a un marché plus large c'est aussi ceux qui affirme
#que la DD nous fait enrichir niveau sociale par une expérience internationnale
#En plus la majorité des réponses étaient de cette catégorie

#chisq.test(table(M$ITEM2,M$ITEM4))
fisher.test(table(M$ITEM2,M$ITEM4))
#H0 est accepté=>IL n y as pas d'asociation



###################Relation de ITEM3###############################

#chisq.test(table(M$ITEM3,M$ITEM4))
fisher.test(table(M$ITEM3,M$ITEM4))
#H0 est accepté=>IL n y as pas d'asociation








####################Relation d'age############################"


#vu qu'on a l'age qui est quasi-normale et pas totalement normale alors je vais appliquer les 2 tests


t.test(M$age~M$A_E)
wilcox.test(M$age~M$A_E,conf.int=T,exact=F)

#le résultat est trés inférieur a 5% qui est notre seuil avec lequel on accepte ou 
#on refuse nos hypothèses 
#les deux tests nous ont donné le meme résultats si c'était pas le cas on aura prit celui de Mann-whitney

#donc H1 est accepté=>il y as une différence significative entre l'age de ceux qui sont en 1ére année cycle d'ingénieur et ceux en deuxiéme année et 
#ce tets nous donnent aussi l'age moyen des CI1 et des CI2 qui est respectivement (20 ans ,21ans)
#ce qui match completement avec la réalité =>c'est un trés bon résultats



t.test(M$age~M$Genre)
wilcox.test(M$age~M$Genre,conf.int=T,exact=F)#les paramétres que j'ai ajouté 
#c'est pour que ca nous donne l'intervalle de confiance et pour exact=F si on enléve ca ,R va nous retourner 
#un warning comme quoi il peut pas calculer la valeur trés précise

#J'ai effectué les deux test juste voir si ils vont me donner le meme resultat
#ce qui est le cas vu que dans les 2 tests on a H0 est majoritaire
# et du coup on peut dire qu'il n y a pas de différence significative entre l'age des hommes et l'age des femmes avec une moyenne qui tournent autour de 20ans pour les deux genres

#pour le genre et l année d'étude on a déja fait ca pour prouver que l'échantillon est éxploratoire







#-------------------------------------------------------4.3:Analyse multivariés-----------------------------------------------#

#commencons d'abord par la conversion


if(is.factor(M$MT_item1))
  M$MT_item1=as.character(M$MT_item1)


for(i in 1:length(M$MT_item1))
{
  if(M$MT_item1[i]=="A tout à fait d'accord")
  {
    M$MT_item1[i]=1
  }
  if(M$MT_item1[i]=="d'accord")
  {
    M$MT_item1[i]=2
  }
  if(M$MT_item1[i]=="neutre")
  {
    M$MT_item1[i]=3
  }
  if(M$MT_item1[i]=="pas d'accord")
  {
    M$MT_item1[i]=4
  }
  if(M$MT_item1[i]=="pas du tout d'accord")
  {
    M$MT_item1[i]=5
  }
}
  
if(is.character(M$MT_item1))
  M$MT_item1=as.integer(M$MT_item1)






if(is.factor(M$MT_item2))
  M$MT_item2=as.character(M$MT_item2)


for(i in 1:length(M$MT_item2))
{
  if(M$MT_item2[i]=="A tout à fait d'accord")
  {
    M$MT_item2[i]=1
  }
  if(M$MT_item2[i]=="d'accord")
  {
    M$MT_item2[i]=2
  }
  if(M$MT_item2[i]=="neutre")
  {
    M$MT_item2[i]=3
  }
  if(M$MT_item2[i]=="pas d'accord")
  {
    M$MT_item2[i]=4
  }
  if(M$MT_item2[i]=="pas du tout d'accord")
  {
    M$MT_item2[i]=5
  }
}

if(is.character(M$MT_item2))
  M$MT_item2=as.integer(M$MT_item2)










if(is.factor(M$MT_item3))
  M$MT_item3=as.character(M$MT_item3)


for(i in 1:length(M$MT_item3))
{
  if(M$MT_item3[i]=="A tout à fait d'accord")
  {
    M$MT_item3[i]=1
  }
  if(M$MT_item3[i]=="d'accord")
  {
    M$MT_item3[i]=2
  }
  if(M$MT_item3[i]=="neutre")
  {
    M$MT_item3[i]=3
  }
  if(M$MT_item3[i]=="pas d'accord")
  {
    M$MT_item3[i]=4
  }
  if(M$MT_item3[i]=="pas du tout d'accord")
  {
    M$MT_item3[i]=5
  }
}



if(is.character(M$MT_item3))
  M$MT_item3=as.integer(M$MT_item3)





if(is.factor(M$ITEM1))
  M$ITEM1=as.character(M$ITEM1)


for(i in 1:length(M$ITEM1))
{
  if(M$ITEM1[i]=="A tout à fait d'accord")
  {
    M$ITEM1[i]=1
  }
  if(M$ITEM1[i]=="d'accord")
  {
    M$ITEM1[i]=2
  }
  if(M$ITEM1[i]=="Sans Opinion")
  {
    M$ITEM1[i]=3
  }
  if(M$ITEM1[i]=="pas d'accord")
  {
    M$ITEM1[i]=4
  }
  if(M$ITEM1[i]=="pas du tout d'accord")
  {
    M$ITEM1[i]=5
  }
}


if(is.character(M$ITEM1))
  M$ITEM1=as.integer(M$ITEM1)


if(is.factor(M$ITEM2))
  M$ITEM2=as.character(M$ITEM2)


for(i in 1:length(M$ITEM2))
{
  if(M$ITEM2[i]=="A tout à fait d'accord")
  {
    M$ITEM2[i]=1
  }
  if(M$ITEM2[i]=="d'accord")
  {
    M$ITEM2[i]=2
  }
  if(M$ITEM2[i]=="Sans Opinion")
  {
    M$ITEM2[i]=3
  }
  if(M$ITEM2[i]=="pas d'accord")
  {
    M$ITEM2[i]=4
  }
  if(M$ITEM2[i]=="pas du tout d'accord")
  {
    M$ITEM2[i]=5
  }
}


if(is.character(M$ITEM2))
  M$ITEM2=as.integer(M$ITEM2)












if(is.factor(M$ITEM3))
  M$ITEM3=as.character(M$ITEM3)


for(i in 1:length(M$ITEM3))
{
  if(M$ITEM3[i]=="A tout à fait d'accord")
  {
    M$ITEM3[i]=1
  }
  if(M$ITEM3[i]=="d'accord")
  {
    M$ITEM3[i]=2
  }
  if(M$ITEM3[i]=="Sans Opinion")
  {
    M$ITEM3[i]=3
  }
  if(M$ITEM3[i]=="pas d'accord")
  {
    M$ITEM3[i]=4
  }
  if(M$ITEM3[i]=="pas du tout d'accord")
  {
    M$ITEM3[i]=5
  }
}


if(is.character(M$ITEM3))
  M$ITEM3=as.integer(M$ITEM3)






if(is.factor(M$ITEM4))
  M$ITEM4=as.character(M$ITEM4)


for(i in 1:length(M$ITEM4))
{
  if(M$ITEM4[i]=="A tout à fait d'accord")
  {
    M$ITEM4[i]=1
  }
  if(M$ITEM4[i]=="d'accord")
  {
    M$ITEM4[i]=2
  }
  if(M$ITEM4[i]=="Sans Opinion")
  {
    M$ITEM4[i]=3
  }
  if(M$ITEM4[i]=="pas d'accord")
  {
    M$ITEM4[i]=4
  }
  if(M$ITEM4[i]=="pas du tout d'accord")
  {
    M$ITEM4[i]=5
  }
}


if(is.character(M$ITEM4))
  M$ITEM4=as.integer(M$ITEM4)

# write.table(M,"ABCE.csv",row.names = F,sep=";") 



library(ltm)
items=data.frame(M$MT_item1,M$MT_item2)
items2=data.frame(M$ITEM1,M$ITEM2,M$ITEM3,M$ITEM4)
cronbach.alpha(items)#ca nous donne 0.3
cronbach.alpha(items2)

#quand on supprime ITEM4 meme si sa qualité représentativite est >0.5 l'alpha grimpe
items3=data.frame(M$ITEM1,M$ITEM2,M$ITEM3)
cronbach.alpha(items3)#ca nous donne presque 0.7 ce qui est trés bien

library(psych)
KMO(items3)#>0.6


--------------------------------------------------------------------------------------------------------------
#ACP est trés utilie quand on a des corrélations fortes entre les variables
#utilisé "cest ce qu'on a montré dans la partie bi-variée" pourquoi?=>prcq qui dit corrélation forte
  #dit une redondance d'informations et donc les composants qu'on va introduire vont chercher les axes sur lequels on trouve la plus variances entre
  #des observations ce qui va nous permettre de réduire les dimensions
---------------------------------------------------------------------------------------------  
library("FactoMineR")
library("factoextra")


View(M)

M.active = M[,c(8,9,10)]

View(M.active)

PCA(M.active, scale.unit = TRUE, ncp = 5, graph = TRUE)


res.pca <- PCA(M.active, graph = FALSE)

res.pca

#Valeur propre mesure la quantité de variance expliqué par un axe principale
#ils sont grandes pour les premiers composants et petites pour les autres
eig.val <- get_eigenvalue(res.pca)
eig.val


#on a les deux premier composant expliquent plus de 84% des informations qu'on a 
#dans notre observation 


fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 80))#un graphe qui nous explique la répartition 
#des pourcentage par rapport au différent Dimensions



#######################Partie Variables################
var=get_pca_var(res.pca)
var
#quand on parle de contribution d'une variable d'origne on veut dire par ca 
#le pourcentage de contribution de la variable Item1 par exemple a la création
#à la première composante prcq si y as pas de variables d'origines on aura pas de composants qui vont les représenter
#N.B:en switchant de 3 a 2variables on pert l'information

# Cos2: qualité de répresentation
var$cos2
# Contributions aux composantes principales
var$contrib



#le cercle de corrélation représente les coordonnées de nos variables par leur corrélation
#avec les composants

fviz_pca_var(res.pca, col.var = "black")


#la qualité de représentation
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)


# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
#ca montre la représentativité des deux axes principales par rapport aux différent variables qu'on disposent

#faut savoir que plus la corrélation est forte (akm la fléche de la variable tend vers la circonference ) plus la qualité de  la 
#représentation est grande =>plus le cos2 est grand c'est ce qu'on voit sur ce graphe

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)



###############la contribution de la variable a la définition des composants######

#Plus la valeur de la contribution est importante, plus la variable contribue à la composante principale en question.





# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)#la contribution des 3 items aux 2 CP(composants principales)
#La ligne en pointillé rouge, sur le graphique ci-dessus, indique la contribution moyenne attendue


#les ITEM3 ET ITEM2 sont ceux qui ont contribué le plus au niveau des deux axes
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)



##----------------------------------Individus-----------------------------------------------##





ind <- get_pca_ind(res.pca)
ind

fviz_pca_ind (res.pca)

fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

#une différenete facon de orésenter la qualité de représentativité des samples

fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE # Évite le chevauchement de texte
)



#une autre qui combine les 2
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)


# Contribution totale sur PC1 et PC2
#comme pour les varibales les echantillons contribue aussi a la creation des CP
#on voit ci dessus comment
fviz_contrib(res.pca, choice = "ind", axes = 1:2)



M.active2 = M[,c(7,8,9,10)]
M.pca <- PCA(M.active2[, - 1], graph = FALSE)

fviz_pca_ind(M.pca,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = M$For_item1, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)

#c'est une classification par réponse a la question?que pensez vous de la DD?

#biplot
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individues
)




library(Rcmdr)
shapiro.test(D1$PC1)
#donc il y as une différence significative entre PC1 et la loi normale théorique 
library(moments)

kurtosis(D1$PC1)
skewness(D1$PC1)
#on peut trancher sur le faite que D1$PC1 ne suit pas la loi normale

wilcox.test(D1$PC1~D1$Genre)
#H0 est accepté il y'as donc pas de différence significative entre la valeur de PC1 pour les hommes et pour les femmes







#L'intervalle de confiance de l'étude est de 85% avec une marge d'erreur de 9%
#ca nous donne un échantillon de 45 individus ce qui est approxitivement le cas pour nous vu qu'on a 43individus
#le résultat est tiré du site https://www.surveymonkey.com/mp/sample-size-calculator/



#Conclusion
#D'aprés notre étude statistique on sort avec le résultat qui nous dit que si on se base sur la statistique univariés on voit que la majorité pensent que
#la relation qui lie la DD avec le marché de travail de facon général est positif
#mais si on se refère a la statistique bivariés on voit que les CI1 sont majoritairement ceux qui confirment amplement notre H.R

#par contre les CI2 y 'as une petite population et je souligne sur le mot petite 
#qui est contre notre hypothéses et qui pensent le contraire mais méme parmis eux on trouve une bonne proportion des gens qui confirment l H.R