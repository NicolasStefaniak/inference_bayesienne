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

# Code chapitre 2 -------------------------------------------------



# Que signifie la valeur p ? ----------------------------------------------




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



set.seed(1234) # le fait de mettre une graine assure une reproductibilité des résultats 

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

df$sign<-if_else(df$p.value<.05, "oui", "non") # créer une colonne qui indique si c'est significatif ou non

T1<-table(df$condition, df$sign) # on fai une table 
T1

round(prop.table(T1, 1),4)*100 # on obtient les proportions par ligne


pwr.t.test(n = 20,# la taille de l'échantillon dans chacun des groupes
           d =0.2, # la taille d'effet 
           type="two.sample") # le type de test de Student 



df$p.rond<-round(df$p.value, 4)
df$p.rond[15454]

dim(df[which(df$condition==1 & df$p.rond==.030),])

df[which(df$condition==1 & df$p.rond==.030),]
df[which(df$condition==0 & df$p.rond==.030),]


df2<-df %>%filter(p.rond == 0.0001)
df2


table(df2$condition)

df3<- df[sample(nrow(df), 1000), ]
df3$condition<-as.factor(df3$condition)
df3$condition<-recode_factor(df3$condition, "0"="Pas de différence", "1"="Différence")
p<-ggplot(df3, aes(y =p.value, x =condition))+geom_point()+ geom_hline(yintercept=0.05, color ="red", size=1)

ggplotly(p)



#  Quand un effet négligeable est significatif ----------------------------



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



# Quand la règle d'arrêt vient tout changer  ------------------------------



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





# Contruire une distribution a priori -------------------------------------


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





# Distribution Cauchy v. Distribution normale

ggplot(data = tibble(x = -10:10), aes(x)) +
  stat_function(fun = dcauchy, n = 1000, args = list(0, 1)) +
  annotate(geom="text", x=-4, y=0.3, label="Distribution Cauchy",
           color="black")+
  stat_function(fun = dnorm, n = 1000, args = list(0), color ="blue") +
  annotate(geom="text", x=4, y=0.3, label="Distribution normale",
           color="blue")



# Explicatoin du prior dans BayesFactor

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


# Calcul de facteur de Bayes 




ttestBF(QI, mu = 100,rscale = sqrt(2))

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


# Analyses séquentielles 
bf.exp <- bfttest(QI, mu = 100, alternative = "two.sided", prior.loc = 0, prior.r = sqrt(2))

plot(bf.exp)

# Ajout d'une observation aberrante avant de faire l'analyse séquentielle
QI<-c(QI, 40)
bf.exp2 <- bfttest(QI, mu = 100, alternative = "two.sided", prior.loc = 0, prior.r = sqrt(2))

plot(bf.exp2)


# Simulation de la décision prise avec les facteurs de Bayes
# pour un échantillon de 20 personnes
# quand la différence vaut 0 ou 0.2 

set.seed(1234) # le fait de mettre une graine assure une reproductibilité des résultats 

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

# on crée des données (20000) : pour créer de manière aléatoire nos conditions. Elles sont au nombre de 200 000
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
                           df$condition==0 & df$BF>33, "incorrect",
                         "manque de sensibilité"))


table(df$condition, df$sig1)


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
# on commencer par établir la liste de prior que nous souhaitons tester
# ici c'est une séquence qui va de 0.05 à 2, avec un accent particulier 
# sur les prior prédéfinis de dans le package BayesFactor (0.5,0.707, 1,1.41)
rs <- c(seq(from = .05, to =2, by = 0.05), 0.5, 0.707, 1, 1.41)

# on veut pouvoir les calculer tous en même temps. 
# pour cela, on va utiliser la fonction sapply qui va 
# calculer le Bayes Factor pour tous les priors qu'on va lui donner (stockés dans rs)
bfs <- sapply(rs, 
              function(r) { # on crée un fonction spécifiquement pour notre propos
                # on utilise exactement la même fonction que celle qu'on utilise pour un seul BF
                # mais au lieu de donner une valeur au rscale, on lui donne l'argument r
                # qui seront fourni par tous les rs              
                bf <- ttestBF(x = QI, mu = 100, rscale = r)
                # on extrait les facteurs de Bayes
                extractBF(bf)$bf
              })

# on crée un data.frame pour faire le graphique 
robust<-data.frame(rs, bfs) 
# on réalise le graphique 

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
           x = robust$rs[41:44],
           y = robust$bfs[41:44]+1, colour ="blue", check_overlap = TRUE)

# note les Bayes Factor sont sur une échelle qui permet de les représenter 
#en valeur 
#brute. Il peut être utile de faire une transformation logarithmique lorque 
# leur valeur est très élevée. 




# Exercices ---------------------------------------------------------------


