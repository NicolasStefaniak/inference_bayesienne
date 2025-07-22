#########################################################
#########################################################
## script accompagnant la formation aux inférences     ##
## bayesiennes                                         ##
##  Auteur ; Nicolas Stefaniak                         ##
## date : 3 et 4 juillet 2025                          ##
#########################################################
#########################################################



# Installation des packages -----------------------------------------------

install.packages("BayesFactor")
install.packages("bayestestR")
install.packages("pwr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("devtools")
devtools::install_github("mrzdcmps/changeofevidence")
install.packages("MASS")
install.packages("tidyverse")

# Chargement des packages -------------------------------------------------

library(BayesFactor)
library(bayestestR)
library(pwr)
library(dplyr)
library(ggplot2)
library(plotly)
library(changeofevidence)
library(MASS)
library(tidyverse)


# Á propos de cet ouvrage -------------------------------------------------



# Introduction -------------------------------------------------


# Le théorème de Bayes ----------------------------------------------------


# Accumuler des informations ----------------------------------------------


# Les axiomes des probabilités  -------------------------------------------



# Quelques rappels sur l'approche de Neyman-Pearson ----------------------------------------------


# Que signifie la valeur p 

echantillon<-rnorm(20, 100, 15 )
echantillon
mean(echantillon)
sd(echantillon)

# on fait une comparaison de deux populations dont les moyennes sont strictement égales 
t.out<-t.test(rnorm(n = 20, # taille de l'échantillon 1
                    mean = 0, # moyenne du groupe 1
                    sd = 1), # écart-type de l'échantillon 1
              rnorm(n = 20, # taille de l'échantillon 2
                    mean = 0,# moyenne du groupe 2
                    sd = 1) ) # écart-type de l'échantillon 2

t.out

set.seed(1234) # le fait de mettre une graine assure une reproductibilité des résultats 


# on va créer une fonction qui compare deux moyennes qui sont strictement égales 
# lorsque la condition vaut 0 et
# dont les moyennes se différencient de 0,2 écart-type (d = 0.2) quand la condition 
# vaut 1
# La fonction renvoie la probabilité du test t 

F1<-function(cond = 0){ 
  if(cond==0){
  # on fait une comparaison de deux populations dont les moyennes sont strictement égales 
  t.out<-t.test(rnorm(n = 20, # taille de l'échantillon 1
                      mean = 0, # moyenne du groupe 1
                      sd = 1), # écart-type de l'échantillon 1
                rnorm(n = 20, # taille de l'échantillon 2
                      mean = 0,# moyenne du groupe 2
                      sd = 1) )# écart-type de l'échantillon 2
  }else{ 
                        t.out<-t.test(rnorm(n = 20, # taille de l'échantillon 1
                                            mean = 0, # moyenne du groupe 1
                                            sd = 1), # écart-type de l'échantillon 1
                                      rnorm(n = 20, # taille de l'échantillon 2
                                            mean = 0.2,# moyenne du groupe 2
                                            sd = 1) ) # écart-type de l'échantillon 2
                      }
  
  return(c(t.out$p.value))
  
}





df<-data.frame(condition = sample(0:1, 200000, replace=T ))

df$p.value<-apply(df, 1, F1)

# on crée une colonne qui indique si c'est significatif ou non
df$sign<-if_else(df$p.value<.05, "oui", "non") 

T1<-table(df$condition, df$sign) # on fait une table 
T1

round(prop.table(T1, 1),4)*100 # on obtient les proportions par ligne


pwr.t.test(n = 20,# la taille de l'échantillon dans chacun des groupes
           d =0.2, # la taille d'effet 
           type="two.sample") # le type de test de Student 


# changer les probabilité entre différence de 0.2 (1/3) et de 0 (2/3)
df<-data.frame(condition = 
                 sample(0:1, 200000, 
                        replace=T,
                        prob=c(2/3, 1/3) )) 

*# arrondir les probabilités
df$p.rond<-round(df$p.value, 4)
df$p.rond[15454]

dim(df[which(df$condition==1 & df$p.rond==.030),])

df[which(df$condition==1 & df$p.rond==.030),]
df[which(df$condition==0 & df$p.rond==.030),]


df2<-df %>%filter(p.rond == 0.0001)
df2


table(df2$condition)

df[15454,]

df3<- df[sample(nrow(df), 1000), ]
df3$condition<-as.factor(df3$condition)
df3$condition<-recode_factor(df3$condition, "0"="Pas de différence", "1"="Différence")
p<-ggplot(df3, aes(y =p.value, x =condition))+geom_point()+ geom_hline(yintercept=0.05, color ="red", size=1)

ggplotly(p)



#  Quand un effet négligeable est significatif 



F1<-function(n){
  t.out<-t.test(rnorm(n, 100,15), 
                rnorm(n, 100.1,15))
  return(t.out$p.value)
}

df<-data.frame(n = rep(2^(1:20), 1000))

df$p<-apply(df, 1, F1)

df$sig<-if_else(df$p<.005, 1,0)

df2<-df %>% group_by(n) %>% summarize(sig = sum(sig))

p<-ggplot(df2, aes(x = n, y=sig))+geom_line()+scale_x_continuous(trans='log2')+
  xlab("Taille de l'échantillon pour chaque groupe")+
  ylab("Nombre d'effets significatifs sur 1000")
p



# Quand la règle d'arrêt vient tout changer  



F1<-function(n){
  t.out<-t.test(rnorm(n, 100,15), 
                rnorm(n, 100,15))
  if(t.out$p.value>.05){
    t.out2<-t.test(rnorm(n+15, 100,15), 
                   rnorm(n+15, 100,15))
  }else{t.out2<-t.out}
  return(c(t.out$p.value, t.out2$p.value))
}

df<-data.frame(etude = rep(35,1000))

results<-apply(df,1, F1)
results<-t(results)
dimnames(results)[[2]]<-c("N35", "N50")

results<-as.data.frame(results)
results$sig1<-if_else(results$N50<.05,1,0)
table(results$sig1)



# Les probabilités : objectif ou subjectif ? ------------------------------




# Les probabilité a priori -------------------------------------


# Contruire une distribution a priori

# Vous pouvez modifier les paramètres du graphique pour l'ajuster à votre cas de figure 
p1 <- ggplot(data = data.frame(x = c(150, 550)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 325, sd = 50)) + ylab("") +
  scale_y_continuous(breaks = NULL)+ 
  geom_vline(xintercept = 400, color="red")+
  geom_vline(xintercept = 325, color="blue")+
  geom_vline(xintercept = 250, color="yellow")+
  geom_area(stat = "function", fun = dnorm, fill = "#00998a", 
            xlim = c(325-1.96*50, 325+1.96*50), 
            args = list(mean = 325, sd = 50), alpha = 0.25) 
p1


z<-(400-325)/50

pnorm(z, lower.tail=F)


z<-(250-325)/50

pnorm(z, lower.tail=T)

# Envisager les priors comme des tailles d'effet 





# Distribution Cauchy v. Distribution normale

ggplot(data = tibble(x = -10:10), aes(x)) +
  stat_function(fun = dcauchy, n = 1000, args = list(0, 1)) +
  annotate(geom="text", x=-4, y=0.3, label="Distribution Cauchy",
           color="black")+
  stat_function(fun = dnorm, n = 1000, args = list(0), color ="blue") +
  annotate(geom="text", x=4, y=0.3, label="Distribution normale",
           color="blue")



# Explication du prior dans BayesFactor

# Définir les valeurs de rscale
rscales <- c(0.2, 0.5, 0.707, 1, 1.41)

# Créer une grille de valeurs de delta (taille d'effet standardisée)
delta <- seq(-4, 4, length.out = 1000)

# Construire un data frame avec les densités Cauchy pour chaque rscale
prior_df <- do.call(rbind, lapply(rscales, function(r) {
  data.frame(
    delta = delta,
    density = dcauchy(delta, location = 0, scale = r),
    rscale = paste0("r = ", r)
  )
}))

ggplot(prior_df, aes(x = delta, y = density, color = rscale)) +
  geom_line(size = 1) +
  labs(
    title = "Priors Cauchy sur la taille d'effet standardisée (δ)",
    x = "Taille d'effet standardisée (δ)",
    y = "Densité du prior",
    color = "Échelle du prior"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")



# La vraisemblance  -------------------------------------------------------

binom.test(x = 10, # nombre de succès, càd. avoir 6
           n = 20, # nombre d'essais 
           p = 1/6) # la probabilité d'avoir 6 si le dé n'est pas pipé


# la probabilité d'avoir 6 si la probabibité d'avoir 6 et deux fois plus élevée que les autres
binom.test(x = 10, # nombre de succès, càd. avoir 6
           n = 20, # nombre d'essais 
           p = 2/7) 






# Estimation du maximum de vraisemblance 
MLE.data<-data.frame( )
for(i in 1:20){ # boucle de 20 itérations 
  # estimation de la probabilité d'avoir 6 avec un incrément de 0.05 de 
  # la probabilité du succès à chacune des itérations 
  p <- 0.05 * i 
  
  # estimation de la binomiale 
  bi<-binom.test(x = 10, 
                 n = 20,  
                 p = p) 
  # on collecte les données pour chacune des hypothèses qu'on a testée
  MLE.data<-rbind(MLE.data, c(p, bi$p.value))
  
}
# on donne des noms transparent au jeu de données 
names(MLE.data)<-c("p.succès", "vraisemblance")

# on identifie la ligne pour laquelle la vraisemblance est maximale 
i<-which.max(MLE.data$vraisemblance)
ft<-flextable(MLE.data)
# on met en couleur verte la ligne où la vraisemblance est maximale
# pour identifier l'estimation du paramètre pour laquelle la vraisemblance est maximale ft<-bg(ft, i = i, j = NULL, bg = "#0e925c", part = "body")

ft<-bg(x=ft, i = i, j = NULL, bg = "#0e925c", part = "body", source = j)
ft<-set_caption(ft, 
                "Vraisemblance estimée pour différentes 
                hypothèses de probabilité d'obtenir 6.")
ft



# Représenter la densité de probabilité 

dnorm(112, # la valeur d'intérêt 
      100, # la moyenne supposée 
      15) # l'écart-type supposé
ggplot(data = tibble(x = 45:145), aes(x)) +
  
  stat_function(fun = dnorm, n = 1000, args = list(mean = 100, sd =15), color ="black")+
  geom_segment(aes(x = 112, y = 0, xend = 112, yend = dnorm(112, 100, 15)), 
               color ="red"
  )





# Vraisemblance avec l'hypothèse d'un QI moyen à 100 et un écart-type à 15
densite<-dnorm(QI, 100,15)

densite

vraisemblance<-prod(densite)
vraisemblance


# Vraisemblance avec l'hypothèse d'un QI égal à la moyenne du QI 
# et un écart-type égal à la moyenne de l'écart-type
densite<-dnorm(QI,mean(QI),sd(QI) )

densite

vraisemblance<-prod(densite)
vraisemblance

# Comparaison avec la vraisemblance obtenue par la fonction logLik
lmx <- lm(QI ~ 1)
logML<-logLik(lmx) 
as.numeric(exp(logML))



# Paramètres de la distribution normale
mu <- 100
sigma <- 15

# Observations
QI

# Créer les données de la densité
x_vals <- seq(50, 150, length.out = 1000)
dens <- data.frame(
  x = x_vals,
  y = dnorm(x_vals, mean = mu, sd = sigma)
)

# Calculer la hauteur de la densité à chaque observation
obs_df <- data.frame(
  x = QI,
  y = dnorm(QI, mean = mu, sd = sigma)
)

# Tracer la courbe + les segments verticaux
ggplot(dens, aes(x = x, y = y)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_segment(
    data = obs_df,
    aes(x = x, xend = x, y = 0, yend = y),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(
    title = "Densité normale (μ = 100, σ = 15)",
    x = "Valeur",
    y = "Densité"
  ) +
  theme_minimal(base_size = 14)



# Vraisemblance avec l'hypothèse d'un QI égal à la moyenne du QI 
# et un écart-type égal à la moyenne de l'écart-type
densite<-dnorm(QI,mean(QI),sd(QI) )

densite

vraisemblance<-prod(densite)
vraisemblance

# Comparaison avec la vraisemblance obtenue par la fonction logLik
lmx <- lm(QI ~ 1)
logML<-logLik(lmx) 
as.numeric(exp(logML))


lmx <- lm(QI ~ 1)
logML<-logLik(lmx) 
as.numeric(exp(logML))



# Excercice 1 
Exo1<-c(45, 20, 15, 30, 29, 29, 32, 46, 35, 27, 28, 16, 34, 24, 56, 
        30, 40, 19, 13, 46)

m<-35 # ma moyenne
s<-10 # mon écart-type
densite<-dnorm(Exo1,m,s) 

vrais<-prod(densite)
vrais


# La même chose avec la vraie moyenne
m<-mean(Exo1) # moyenne exo1
s<-sd(Exo1) # écart-type exo1
densite<-dnorm(Exo1,m,s) 

vrais2<-prod(densite)
vrais2


# La probabilité a posteriori ---------------------------------------------


# Le facteur de bayes -----------------------------------------------------



FB<-vraisemblance122/vraisemblance100
FB




tBF.out<-ttestBF(QI, # Les données 
                 mu = 100, # la norme pour fixer l'hypothèse nulle
                 rscale = sqrt(2)) # La distribution a priori pour une distribution ultrawide (vu que la taille d'effet qu'on s'attend à avoir est grande)

tBF.out

# on commence par faire un t de Student fréquentiste
# on extrait la valeur t 
tstat <- t.test(QI , mu = 100)$statistic
tstat
# on récupère le nombre d'obervations (nécssaire notamment pour les ddl)
n <- length(QI)
rscale <- sqrt(2)

# fonction de densité marginale sous H1
# la fonction dt permet de calculer une densité de probabilité 
# pour une distribution t, comme dnorm le fait pour une distribution normale 
# On peut donc calculer la
# densité sous H0 : p(D | H0)
# ce sera le dénominateur 
denominator <- dt(tstat, df = n - 1)
denominator


# Il va falloir faire la même chose à présent pour H1
# il faut calculer la densité p(D | H1)
# nous allons devoir préciser le paramètre de non-centralité
# c'est-a-dire notre prior 
# nous allons devoir faire l'intégrale pour les différentes valeurs possibles 
# cette intégrale a approximativement un incrément de 1/100 entre chaque valeur entre -4 et 4
# le delta va être pondéré par la forme de la distribution Cauchy 
integrand <- function(delta) {
  dt(tstat, 
     df = n - 1, 
     ncp = delta * sqrt(n)) * dcauchy(delta, scale = rscale)
}

# IOn peut faire l'intégrale numérique de : p(D | H1)
numerator <- integrate(integrand, lower = -Inf, upper = Inf)$value



# On  peut obtenir le facteur de Bayes
bf_manual <- numerator / denominator
bf_manual


# Ajuster nos probabilités  



bf.exp <- bfttest(QI, mu = 100, alternative = "two.sided", prior.loc = 0, prior.r = sqrt(2))

plot(bf.exp)

# Ajout d'une observation aberrante avant de faire l'analyse séquentielle
QI<-c(QI, 40)
bf.exp2 <- bfttest(QI, mu = 100, alternative = "two.sided", prior.loc = 0, prior.r = sqrt(2))

plot(bf.exp2)

# Tester une hypothèse nulle 


# Simulation de la décision prise avec les facteurs de Bayes
# pour un échantillon de 20 personnes
# quand la différence vaut 0 ou 0.2 

# le fait de mettre une graine assure une reproductibilité des résultats 

set.seed(1234) 
# on va créer une fonction qui compare deux moyennes qui sont strictement égales lorsque la condition vaut 0 et
# dont les moyennes se différencient de 0,2 écart-type (d = 0.2) quand la condition vaut 1
# La fonction renvoie la probabilité du test t 

F2<-function(cond = 0){ 
  if(cond==0){
    # on fait une comparaison de deux populations dont les moyennes sont strictement égales 
    t.out<-ttestBF(rnorm(n = 20, # taille de l'échantillon 1
                         mean = 0, # moyenne du groupe 1
                         sd = 1), # écart-type de l'échantillon 1
                   rnorm(n = 20, # taille de l'échantillon 2
                         mean = 0,# moyenne du groupe 2
                         sd = 1) )# écart-type de l'échantillon 2
  }else{ 
    t.out<-ttestBF(rnorm(n = 20, # taille de l'échantillon 1
                         mean = 0, # moyenne du groupe 1
                         sd = 1), # écart-type de l'échantillon 1
                   rnorm(n = 20, # taille de l'échantillon 2
                         mean = 0.2,# moyenne du groupe 2
                         sd = 1) ) # écart-type de l'échantillon 2
  }
  
  return(extractBF(t.out)$bf) # renvoie le FB 
  
}

# on crée des données (200 000) : pour créer de manière aléatoire nos conditions. Elles sont au nombre de 200 000
# ici l'échantillonnage se fait de manière équiprobable avec approximativement autant de situations pour la
# condition différence qu'absence de différence 
df<-data.frame(condition = sample(0:1, 
                                  20000, #20000, 
                                  replace=T )) 

# on applique la fonction F pour chacune des lignes du jeu de données (variable condition)
df$BF<-apply(df, 1, F2)


df$sig1<-if_else(df$condition==1 & df$BF>3 | 
                   df$condition==0 & df$BF<1/3, "correct",
                 if_else(df$condition==1 & df$BF<1/3 |  
                           df$condition==0 & df$BF>3, "incorrect",
                         "manque de sensibilité"))


T1<-table(df$condition, df$sig1)
T1  



# Simulation du comportement du Bayes factor
# pour une différence de 0.1 sur le test de QI 

F1<-function(n){
  t.out<-ttestBF(rnorm(n, 100,15), 
                 rnorm(n, 100.1,15))
  return(extractBF(t.out)$bf)
}

df<-data.frame(n = rep(2^(1:20), 100))

df$bf<-apply(df, 1, F1)

df$Decision<-if_else(df$bf>3, "Preuves en faveur de l'alternative",
                     if_else(df$bf<1/3, "Preuves en faveur de l'hypothèse nulle",
                             "Manque de sensibilité"))

df2<-df %>% group_by(n, Decision) %>% summarize(Nombre= n())





ggplot(df2, aes(x=n, y=Nombre, colour =Decision))+geom_line()+
  ylab("Nombre de décision précise (sur 100)")



# Simulation d'une corrélation de 0.08 avec 526 observations


set.seed(46)
# create the variance covariance matrix
sigma<-rbind(c(1,0.08), c(0.08,1))
# create the mean vector
mu<-c(76.51, 51.45 ) 
# generate the multivariate normal distribution
df<-as.data.frame(mvrnorm(n=526, mu=mu, Sigma=sigma))

cor.test(df[,1], df[,2])

correlationBF(
  y= df[,1],
  x=df[,2],
  rscale = "ultrawide"
)


# Réalisation d'une analyse robuste 


## on commencer par établir la liste de prior que nous souhaitons tester
## ici c'est une séquence qui va de 0.05 à 2, avec un accent particulier 
## sur les priors prédéfinis de dans le package BayesFactor (0.5,0.707, 1,1.41)
rs <- c(seq(from = .05, to =2, by = 0.05), 0.5, 0.707, 1, 1.41)

## on veut pouvoir les calculer tous en même temps. 
## pour cela, on va utiliser la fonction sapply qui va 
## calculer le Bayes Factor pour tous les priors qu'on va lui donner (stockés dans rs)
bfs <- sapply(rs, 
              function(r) { # on crée un fonction spécifiquement pour notre propos
                # on utilise exactement la même fonction que celle qu'on utilise pour un seul BF
                # mais au lieu de donner une valeur au rscale, on lui donne l'argument r
                # qui seront fourni par tous les rs              
                bf <- ttestBF(x = QI, mu = 100, rscale = r)
                # on extrait les facteurs de Bayes
                extractBF(bf)$bf
              })

## on crée un data.frame pour faire le graphique 
robust<-data.frame(rs, bfs) 
## on réalise le graphique 

## on fait la ligne qui touche tous les points
ggplot(robust, aes(x=rs, y=bfs))+geom_line()+ 
  # on donne le titre à l'axe des x
  xlab("Choix de la distribution a priori")+
  # on donne le titre à l'axe des y
  ylab("Valeur du facteur de Bayes")+
  # on met en valeur les points des priors prédéfinis dans BayesFactor
  geom_point(data=robust[41:44,], colour="blue")+
  # On indique sur le graphique à quel prior chaque point correspond *
  # en faisant un petit décalage afin d'éviter une superposition avec le point
  annotate("text", label = robust$rs[41:44], 
           x = robust$rs[41:44],
           y = robust$bfs[41:44]+1, colour ="blue", check_overlap = TRUE)

## note les Bayes Factor sont sur une échelle qui permet de les représenter 
##en valeur 
##brute. Il peut être utile de faire une transformation logarithmique lorque 
## leur valeur est très élevée. 

bf.robust <- bfRobustness(bf.exp2)
plot(bf.robust)

bf.robust <- bfRobustness(bf.exp2, prior.loc=0)
plot(bf.robust)



# Comparaison avec l'approche fréquentiste --------------------------------



# Exercices ---------------------------------------------------------------


## t de Student
TOLD<-read_xlsx("./Exercices/Demo.xlsx", sheet="TOLD")
str(TOLD)
TOLD$groupe<-as.factor(TOLD$groupe)
str(TOLD)
psych::describeBy(TOLD ~groupe, data= TOLD,mat=T)
t.out<-ttestBF(# formule du type VD ~ VI
  formula =TOLD ~groupe, 
  # nom du jeu de données
  data=TOLD, 
  # est-ce que c'est apparié (TRUE) ou à groupes 
  # indépendants (FALSE)
  paired =F, 
  # l'argument mu permet d'indiquer une norme, notamment dans
  # les t avec un seul échantillon
  # cet argument n'est pas pris en compte si on a une formule
  # et que la valeur vaut 0
  mu = 0,
  # distribution a priori à .707
  rscale = .707,
  # faut il calculer la distribution a posteriori (F =non)
  posterior = F) 
t.out

bf.exp <- bfttest(TOLD ~groupe, , 
                  data= TOLD, 
                  alternative = "two.sided", 
                  # localisation de la taille d'effet
                  # pour répliquer ttestBF, il faut place loc à 0
                  prior.loc = 0, 
                  prior.r = sqrt(2))

bf.exp
plot(bf.exp)

bf.robust <- bfRobustness(bf.exp,prior.loc =0 )
plot(bf.robust)


## Corrélation 

plausib<-read_xlsx("./Exercices/Demo.xlsx", sheet="plausibilite")
str(plausib)
corr.out<-correlationBF(
  # la variable sur l'ordonnée
  y=plausib$Plaus,
  # la variable en abscisse
  x=plausib$collab,
  # la distribution a priori
  rscale = "medium",
  # les limites inférieures et supérieures 
  # d'un intervalle à l'intérieur duquel 
  # on pense que se trouve la corrélation 
  nullInterval = NULL,
  # Faut il calculer la distribution a posteriori
  posterior = F)
corr.out

# calcul de la distribution a posteriori
post<-posterior(corr.out, iterations = 1000)
plot(post)

summary(post)

ci_hdi <- ci(post, method = "HDI")
ci_hdi

#les 4 dernières valeurs sont les valeurs correspondant aux effets prédéfinis 
#"moyenne", "large" et "ultra large"
rs <- c(seq(from = .05, to =2, by = 0.05), 
        1/sqrt(27), 1/3, 1/sqrt(3), 1)

# on veut pouvoir les calculer tous en même temps. 
# pour cela, on va utiliser la fonction sapply qui va 
# calculer le Bayes Factor pour tous les priors qu'on va lui donner (stockés dans rs)
bfs <- sapply(rs, 
              function(r) { # on crée un fonction spécifiquement pour notre propos
                # on utilise exactement la même fonction que celle qu'on utilise pour un seul BF
                # mais au lieu de donner une valeur au rscale, on lui donne l'argument r
                # qui seront fourni par tous les rs              
                bf <- correlationBF(y=plausib$collab,
                                    x=plausib$Plaus,
                                    rscale = r,
                                    nullInterval = NULL,
                                    posterior = F)
                # on extrait les facteurs de Bayes
                extractBF(bf)$bf
              })

# on crée un data.frame pour faire le graphique 
robust<-data.frame(rs, bfs) 
# on réalise le graphique 
robust$bfs<-round(robust$bfs, 3)
robust$rs<-round(robust$rs, 3)
# on fait la ligne qui touche tous les points
ggplot(robust, aes(x=rs, y=bfs))+geom_line()+ 
  # on donne le titre à l'axe des x
  xlab("Choix de la distribution a priori")+
  # on donne le titre à l'axe des y
  ylab("Valeur du facteur de Bayes")+
  # on met en valeur les points des priors prédéfinis dans BayesFactor
  geom_point(data=robust[41:44,], colour="blue")+
  # On indique sur le graphique à quel prior chaque point correspond 
  # en faisant un petit décalage afin d'éviter une superposition avec le point
  annotate("text", label = robust$rs[41:44], 
           x = robust$rs[41:44]+0.2,
           y = robust$bfs[41:44]+0.2, colour ="blue", check_overlap = TRUE)


## Régression 

detresse<-read_xlsx("./Exercices/Demo.xlsx", sheet="detresse")
str(detresse)

library(olsrr)
model<-lm(detresse~coping+age, detresse)

ols_plot_added_variable(model)
lm.out<-lmBF(
  # on précise le modèle du type VD ~VI1+VI2
  # on peut mettre des interactions VD ~VI1+VI2+VI1:VI2
  formula = detresse~coping+age,
  # on précise le jeu de données 
  data = detresse,
  # on peut préciser la variable aléatoire
  # sous forme de caractère comme "ID"
  whichRandom = NULL,
  # on peut préciser les distributions pour les effets d'intérêts
  rscaleFixed = "medium",
  # et contrôler la distribution de la variable aléatoire
  # on peut toujours laisser la valeur par défaut
  rscaleRandom = "nuisance",
  # il s'agit du prior sur la pente
  # mais la différence par rapport à rscaleFixed n'est pas claire. 
  # il est possible que rscaleFixed s'applique à tout
  # et que rscalecont et rscaleRandom permet de dissocier les deux
  rscaleCont = "medium",
  # il est possible de préciser avec rscaleEffects des prior pour chaque effet
  # par exemple c(coping = 0.5, age =0.3)
  # on ne précise pas ici
  rscaleEffects = NULL,
  # la distribution a posteriori doit elle être calculée. 
  posterior = FALSE)


lm.out

lm.out1<-lmBF(
  formula = detresse~coping,
  data = detresse,
  whichRandom = NULL,
  rscaleFixed = "medium",
  rscaleRandom = "nuisance",
  rscaleCont = "medium",
  rscaleEffects = NULL,
  posterior = FALSE)

lm.out2<-lmBF(
  formula = detresse~coping+age,
  data = detresse,
  whichRandom = NULL,
  rscaleFixed = "medium",
  rscaleRandom = "nuisance",
  rscaleCont = "medium",
  rscaleEffects = NULL,
  posterior = FALSE)

lm.out2/lm.out1


## Analyse de variance 

library(reshape2)
AI2<-AI %>% dplyr::select(TYPE, BLOC1, BLOC12, BLOC13)

# pour utiliser la fonction melt, on peut utiliser ici
# uniquement l'argument id.vars qui indique la variable des identifiants.
# Dans certaines situations, ce ne sera pas suffisant, mais dans cet exemple,
# nous pouvons nous en contenter
AI.long<-melt(AI2,id.vars="TYPE")
names(AI.long)<-c("TYPE", "BLOC", "Temps")
AI.long$TYPE<-as.factor(AI.long$TYPE)

# on crée une séquence qui va de 1 à 60 précédé par "p" (p1, p2...)
# et on la répète trois fois vu que nous avons 3 blocs sélectionnés
# pour nos analyses. 
AI.long$ID<-rep(paste0("p",1:60), 3)
AI.long$ID<-as.factor(AI.long$ID)

aov.out<- generalTestBF(
  # on précise le modèle
  # remarquez la présence de l'identifiant dans le modèle
  # R en a besoin pour identifier quelles mesures correspondent 
  # à quel participant
  Temps ~ TYPE*BLOC + ID, 
  data = AI.long,
  # on explicite ici que ID est le facteur aléatoire
  whichRandom = "ID",
  # pour éviter de multiplier les modèle calculés
  # on va conserver la variable aléatoire dans tous
  # les modèles
  # cela évite d'avoir VD~ID, VD~VI1, VD~ID+VI1
  
  neverExclude="ID")
aov.out

aov.out[4]/aov.out[3]

ttest.tstat(1.2, n1 =20, n2 = 40, rscale = "medium")


# Exercice 2 


priolo<-read_xlsx("./Exercices/Nudge.xlsx")

priolo <-as.data.frame(priolo)
t.out<-ttestBF(formula =OC ~Campus, 
               data=priolo, paired =F, 
               rscale = .707) # prior à .707
t.out


bf.exp <- bfttest(OC ~Campus, 
                  data= priolo, 
                  alternative = "two.sided", 
                  prior.loc = 0, 
                  prior.r = sqrt(2))

plot(bf.exp)

bf.robust <- bfRobustness(bf.exp,prior.loc =0 )

plot(bf.robust)

psych::describe(OC ~Campus, data= priolo)


# Exercice 3

corr.out<-correlationBF(
  y=priolo$MOY_ACC,
  x=priolo$MOY_FORCE,
  rscale = "medium",
  nullInterval = NULL,
  posterior = F)

# calcul de l'intervalle de crédibilité 
post<-posterior(corr.out, iterations = 1000)
post2 = recompute(post, iterations = 10000)
plot(post2)

summary(post2)

plot(post2[,1:2])



## OU
corr.out.post<-correlationBF(
  y=priolo$MOY_ACC,
  x=priolo$MOY_FORCE,
  rscale = "medium",
  nullInterval = NULL,
  posterior = T, iterations =1000)


ci_hdi <- ci(corr.out.post, method = "HDI")
ci_hdi

ggplot(priolo, aes(x = MOY_FORCE, y =MOY_ACC) )+
  geom_point()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")


# analyse robuste
rs <- c(seq(from = .05, to =2, by = 0.05), 1/sqrt(27), 1/3, 1/sqrt(3), 1)

# on veut pouvoir les calculer tous en même temps. 
# pour cela, on va utiliser la fonction sapply qui va 
# calculer le Bayes Factor pour tous les priors qu'on va lui donner (stockés dans rs)
bfs <- sapply(rs, 
              function(r) { # on crée un fonction spécifiquement pour notre propos
                # on utilise exactement la même fonction que celle qu'on utilise pour un seul BF
                # mais au lieu de donner une valeur au rscale, on lui donne l'argument r
                # qui seront fourni par tous les rs              
                bf <- correlationBF(y=priolo$MOY_ACC,
                                    x=priolo$MOY_FORCE,
                                    rscale = r,
                                    nullInterval = NULL,
                                    posterior = F)
                # on extrait les facteurs de Bayes
                extractBF(bf)$bf
              })

# on crée un data.frame pour faire le graphique 
robust<-data.frame(rs, bfs) 
# on réalise le graphique 
robust$bfs<-round(robust$bfs, 3)
robust$rs<-round(robust$rs, 3)
# on fait la ligne qui touche tous les points
ggplot(robust, aes(x=rs, y=bfs))+geom_line()+ 
  # on donne le titre à l'axe des x
  xlab("Choix de la distribution a priori")+
  # on donne le titre à l'axe des y
  ylab("Valeur du facteur de Bayes")+
  # on met en valeur les points des priors prédéfinis dans BayesFactor
  geom_point(data=robust[41:44,], colour="blue")+
  # On indique sur le graphique à quel prior chaque point correspond *
  # en faisant un petit décalage afin d'éviter une superposition avec le point
  annotate("text", label = robust$rs[41:44], 
           x = robust$rs[41:44]+0.2,
           y = robust$bfs[41:44]+0.2, colour ="blue", check_overlap = TRUE)


# Exercice 4 


priolo2<-priolo %>% dplyr::select( Participants, Campus, UC, OC)
priolo.long<-melt(priolo2, id.vars = c("Participants", "Campus"), variable.name="Localisation", value.name="Quantite")

priolo.long$Participants<-as.factor(priolo.long$Participants)

priolo.long$Campus<-as.factor(priolo.long$Campus)

aov.out<- generalTestBF(Quantite ~ Campus*Localisation + Participants, data = priolo.long, whichRandom = "Participants",
                        neverExclude="Participants", progress=FALSE)
aov.out
aov.out[4]/aov.out[3]
psych::describeBy(Quantite ~ Campus*Localisation, data=priolo.long,mat=T)

# Exercice 5

drouillet<-read_xlsx("./Exercices/Drouillet2018.xlsx")

drouillet$participant<-as.factor(drouillet$participant)
lm.out1.1<-lmBF(indice1.meta.baseline ~  Mill_Hill + participant, 
                data = drouillet, whichRandom = "participant")
lm.out1.2<-lmBF(indice1.meta.baseline ~ indice_app_impl+ Mill_Hill + participant, 
                data = drouillet, whichRandom = "participant")
lm.out1.3<-lmBF(indice1.meta.baseline ~ indice_app_impl+ Mill_Hill +
                  indice_app_impl:Mill_Hill+ participant, 
                data = drouillet, whichRandom = "participant")

lm.out1.2/lm.out1.1
lm.out1.3/lm.out1.3


lm.out2.1<-lmBF(indice2.meta.baseline ~  Mill_Hill + participant, 
                data = drouillet, whichRandom = "participant")
lm.out2.2<-lmBF(indice2.meta.baseline ~ indice_app_impl+ Mill_Hill + participant, 
                data = drouillet, whichRandom = "participant")
lm.out2.3<-lmBF(indice2.meta.baseline ~ indice_app_impl+ Mill_Hill +
                  indice_app_impl:Mill_Hill+ participant, 
                data = drouillet, whichRandom = "participant")

lm.out2.2/lm.out2.1
lm.out2.3/lm.out2.3
