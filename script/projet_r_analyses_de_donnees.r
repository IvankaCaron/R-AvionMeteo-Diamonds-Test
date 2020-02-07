########################################
###  Projet R - Analyse des données  ###
########################################

#        Ivanka / Davy / Pascal        #

###### Dernière MAJ >> 2020-01-16 ######

####### Présentation des données #######

# Chargement des modules
library(nycflights13)
library(dplyr)
library(data.table) #porqoui table?

# Dimensions de la base
dim(nycflights13::flights)

# Nombre de lignes avant nettoyage
nlavn = dim(nycflights13::flights)[1]

# Chargement des tables dans l'environnement
flights  <- nycflights13::flights
weather  <- nycflights13::weather
planes   <- nycflights13::planes
airports <- nycflights13::airports
airlines <- nycflights13::airlines


######   Premiers traitements   ######

# Noms et types des variables présentes
str(flights)

### Valeurs à étudier

# Distance du vol     >>> flights$distance
# Durée du vol        >>> flights$air_time
# Retard au départ    >>> flights$dep_delay
# Retard à l’arrivée  >>> flights$arr_delay

### Valeurs manquantes

# Aperçu rapide en utilisant la fonction 'is.na'
sapply(flights, function(x) sum(is.na(x)))

# Création du tableau des valeurs manquantes (pour visualisation)
missing_values <- flights %>% 
  filter(is.na(flights$distance) | is.na(flights$air_time) | 
           is.na(flights$dep_delay) | is.na(flights$arr_delay))

# Première estimation nous renvoyant 9430 lignes
dim(missing_values)


### Remplacement des valeurs manquantes par des valeurs pouvant
### être calculées à l'aide de valeurs d'autres colonnes

# Les valeurs manquantes que l'on peut retrouver
# dep_delay (en minutes) = dep_time - sched_dep_time (les 2 en HHMM ou HMM)
# arr_delay (en minutes) = arr_time - sched_arr_time (les 2 en HHMM ou HMM)

# Nombre de valeurs manquantes recalculables
missing_delays <- flights %>%
  filter((is.na(flights$dep_delay) | is.na(flights$arr_delay)) &
           !is.na(flights$dep_time) & !is.na(flights$sched_dep_time) &
           !is.na(flights$arr_time) & !is.na(flights$sched_arr_time))

# La commande suivante nous informe que l'on peut récupérer 717 lignes
dim(missing_delays)

# La commande suivante nous informe qu'il n'y a qu'une
# colonne recalculable >>> 'arr_delay'
sapply(missing_delays, function(x) sum(is.na(x)))

# Fonction qui crée une nouvelle colonne en minutes
# à partir d'une colonne existante en HHMM
konversion <- function(colonne) {
  nouvelle_colonne  =  paste(colonne,'_minutes',sep='')
  flights[nouvelle_colonne] <- floor(flights[colonne]/100) * 60 +
    (flights[colonne]/100 - floor(flights[colonne]/100))*100
  return (flights[nouvelle_colonne])
}

# Ajout des nouvelles colonnes en minutes pour le calcul des valeurs manquantes
columns_list <- list('arr_time','sched_arr_time')

for (x in columns_list) {
  flights <- cbind(flights,konversion(x))
}

# Suppression des lignes comportant des valeurs NA dans
# les colonnes que l'on vient de créer et dans la colonne 'air_time'
sapply(flights, function(x) sum(is.na(x)))
flights <- flights[!is.na(flights$arr_time_minutes), ]
flights <- flights[!is.na(flights$air_time), ]
sapply(flights, function(x) sum(is.na(x)))

# Remplacement des valeurs manquantes par les valeurs calculées
flights$arr_delay = flights$arr_time_minutes - flights$sched_arr_time_minutes

# Dernière vérification
sapply(flights, function(x) sum(is.na(x)))

# Dimension après nettoyage
dim(flights)

# Nombre de lignes après nettoyage
nlapn = dim(flights)[1]

# Pourcentage de lignes supprimées 
pls = (1 - (nlapn / nlavn)) * 100
cat('Pourcentage de lignes supprimées : ',round(pls,2),'%',' (',nlavn-nlapn,'lignes )')


### Statistiques (moyenne, écart-type, min, max)
### suivant l'aéroport de départ
summarize(group_by(flights, origin), distance_mean = mean(distance), 
          distance_sd = sd(distance), distance_min = min(distance),
          distance_max = max(distance))

summarize(group_by(flights, origin), air_time_mean = mean(air_time), 
          air_time_sd = sd(air_time), air_time_min = min(air_time),
          air_time_max = max(air_time))

summarize(group_by(flights, origin), dep_delay_mean = mean(dep_delay), 
          dep_delay_sd = sd(dep_delay), dep_delay_min = min(dep_delay),
          dep_delay_max = max(dep_delay))

summarize(group_by(flights, origin), arr_delay_mean = mean(arr_delay), 
          arr_delay_sd = sd(arr_delay), arr_delay_min = min(arr_delay),
          arr_delay_max = max(arr_delay))

# Simplifier le code / Résultats ci-dessus à interprèter <<<<< A FAIRE


# Rapprochement avec des données météo #

# Traitement des données manquantes de la base 'weather'
str(weather)
sapply(weather, function(x) sum(is.na(x)))


