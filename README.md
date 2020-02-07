
Projet R - Analyse des données
7 janvier 2020
Énoncé

Le but de ce projet est d’étudier le lien entre des conditions météorologiques et les retards au départ et à l’arrivée des avions en partance de 3 aéroports de New York.
Présentation des données

Sur R, certains packages contiennent des bases de données. C’est le cas du package nycflights13 qui contient les bases suivantes :
- flights : informations sur les vols ayant quitté New York City en 2013.
- weather : données météo par heure pour chaque aéroport.
- planes : informations techniques sur les avions.
- airports : noms complets des aéroports et locations.
- airlines : clef de passage entre le nom de la compagnie codée en 2 caractères et le nom complet.

    Chargez ces tables dans votre environnement.

Premiers traitements

    Parcourez la base flights en affichant les noms et les types des variables présentes. Vous pouvez aussi accéder à un dictionnaire des variables en tapant ?nycflights13::flights dans votre console.
    On s’intéresse plus spécifiquement à la distance du vol, sa durée, son retard au départ et à l’arrivée. En utilisant la fonction is.na, regardez si ces variables ont des valeurs manquantes. Si oui, créez un dataframe où vous les regroupez. À quoi sont dûs ces valeurs manquantes? Comment les traiter (les sortir de la table ou remplacer la valeur manquante par une autre valeur)?
    Sur ces variables, présentez des statistiques (moyenne, écart-type, min, max). Observez-vous des différences de ces statistiques selon l’aéroport d’où est parti l’avion?

Rapprochement avec des données météo

    De la même manière, parcourez la base weather et proposez un traitement des valeurs manquantes.
    Sortez des statistiques sur les variables qui vous semblent pouvoir impacter le retard des avions, sur toute la base puis selon l’aéroport.
    Quel traitement reste-t-il à faire sur la base de données flights pour pouvoir la rapprocher des données météo?
    Fusionnez la table flights ainsi transformée et la table weather en utilisant la fonction de merge de dplyr qui vous semble la plus appropriée entre inner_join, left_join, right_join et outer_join.
    Vérifiez que de nouvelles valeurs manquantes ne sont pas apparues dans cette nouvelle table, si oui traitez-les.

Analyse

    En vous appuyant sur la comparaison des statistiques déjà réalisées et sur au moins 4 représentations graphiques bien choisies, proposez une analyse de l’effet des conditions météorologiques sur les retards des avions. Pensez à définir une problématique en amont, que vous êtes libres de choisir : vous n’êtes pas obligés d’utiliser toutes les variables à disposition!

Modalités & rendu

Par groupes de 2 ou 3 maximum.

Le rendu sera le dossier du projet dans lequel on trouvera :
- le ou les programmes R que vous aurez réalisés, bien commentés et que je pourrai faire tourner
- la table fusionnée au format RDS sur laquelle vous faites les analyses données météo/retards
- les sorties au format image (je vous conseille d’utiliser ggsave pour sauvegarder vos plots au format image).
- un petit rapport au format word où vous présentez d’abord rapidement les stats de base sur les données weather et flights puis vos résultats sur les effets de la météo sur les retards (pas plus de 5 pages).

La date du rendu est fixée au 22 janvier.

