
TITRE DU PROJET : Analyse de Performance des Ventes E-commerce (2023-2024)

 Auteur : Davidson RIGAUD
 Date : Le 08 decembre 2025



 1. Description du Projet
Ce projet est une analyse approfondie des données de transactions, clients, vendeurs et produits sur deux années (2023-2024). L'objectif est d'évaluer la performance commerciale, d'identifier les moteurs de revenus (catégories, clients), d'analyser l'efficacité de la logistique, et de fournir des prévisions de CA pour le trimestre à venir.


 2. Structure du Dossier
Le projet est organisé selon une structure standard pour les analyses de données en R :

mon_projet_2/
├── data/
│   ├── raw/ (Contient les fichiers source : CSV et JSON)
│   └── processed/ (Contient le dataframe fusionné 'donnees_clean.csv')
├── scripts/ (Contient tous les scripts R d'exécution)
├── output/ (Dossier des livrables générés)
│   ├── figures/ (Contient tous les graphiques exportés en .png)
│   └── tables/ (Contient les tableaux d'analyse exportés en .csv)
├── rapport.pdf (Le rapport final d'analyse)
└── README.txt (Ce fichier)



 3. Prérequis et Dépendances

Ce projet nécessite l'installation de l'environnement R et de RStudio. Les packages R suivants sont obligatoires pour l'exécution complète des scripts :

- **dplyr** (Manipulation et agrégation des données)
- **tidyr** (Nettoyage et organisation)
- **ggplot2** (Visualisations graphiques)
- **scales** (Formatage des étiquettes d'axes)
- **jsonlite** (Importation du fichier produits.json)
- **forecast** et **tseries** (Modèles de Séries Temporelles pour les prédictions)

Pour installer tous les packages nécessaires, exécutez la commande suivante dans votre console R :

install.packages(c("dplyr", "tidyr", "ggplot2", "scales", "jsonlite", "forecast", "tseries"))



 4. Instructions d'Exécution

Pour exécuter l'intégralité du projet et générer tous les livrables dans le dossier 'output/', veuillez suivre ces deux étapes :

### Étape 4.1 : Définir le Répertoire de Travail

Vous devez d'abord vous positionner à la racine du dossier du projet (**'mon_projet_2'**).

**Commande R :**
setwd("C:/chemin/vers/votre/dossier/mon_projet_2") 

 Étape 4.2 : Lancer le Script Principal

Une fois le répertoire de travail défini, exécutez le script 'main.R'. Ce script gère l'ordre d'exécution de toutes les étapes (fonctions, nettoyage, analyses, visualisations).

**Commande R :**
source("scripts/main.R")


 5. Livrables Générés

L'exécution du script 'main.R' génère automatiquement tous les fichiers suivants dans le dossier 'output/' :

- **7 Fichiers PNG** (Graphiques) dans `output/figures/`
- **10 Fichiers CSV** (Tables d'analyse agrégées) dans `output/tables/`
- Le fichier fusionné `donnees_clean.csv` dans `data/processed/`
