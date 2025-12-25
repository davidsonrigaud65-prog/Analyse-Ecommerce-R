
# repertoire
setwd("C:\\Users\\DELL\\Desktop\\Formation Data-science_FRST_FDS\\Phase 1\\Introuduction R_Stat Descriptives\\mon_projet_2")

#Chargement des Librairies 
library(dplyr)      
library(jsonlite)   
library(ggplot2)    
library(jsonlite)
library(readr)
library(lubridate)
library(forecast)
library(tidyr)

# 1. Chargement le fichier de fonctions 
print("--- Démarrage : Chargement des fonctions utilitaires (00_fonctions.R) ---")
source("scripts/00_fonctions.R") 
print("Fonctions chargées. Prêt pour l'analyse.")

# 2. ÉTAPE PRÉPARATOIRE : IMPORTATION ET NETTOYAGE
# Ce script importe les données brutes, les fusionne (jointures), nettoie les NA,
# convertit les dates et crée le dataframe principal (d_c ou donnees_fusionnees).
# C'est l'étape la plus critique.
print("--- Étape 1 : Importation, Fusion et Nettoyage (01_import_nettoyage.R) ---")
source("scripts/01_importer_nettoyage.R") 
print("Nettoyage terminé. Dataframe 'd_c' ou 'donnees_fusionnees' créé/mis à jour.")

# 3. ANALYSES DÉTAILLÉES 
# Ces scripts effectuent les calculs et créent les tableaux de résultats.

print("--- Étape 2 : Analyse des Ventes  ---")
source("scripts/02_analyse_ventes.R")

print("--- Étape 3 : Analyse des Produits  ---")
source("scripts/03_analyse_produits.R")

print("--- Étape 4 : Analyse des Clients ---")
source("scripts/04_analyse_clients.R")

print("--- Étape 5 : Analyse vendeurs ---")
source("scripts/05_analyse_vendeurs.R") 

print("---Etape 6 : Analyse logistique---")
source("scripts/06_analyse_logistique.R")

print("---Etape 7 : Prediction---")
source("scripts/07_prediction.R")

print("--- Étape 8 : Visualisations ---")
source("scripts/08_visualisation.R") 


# -----------------------------------------------------------------------------
# 5. MESSAGE DE FIN
# -----------------------------------------------------------------------------
print("--------------------------------------------------")
print("✅ EXECUTION DU PROJET 2 TERMINEE AVEC SUCCES.")
print("Tous les résultats (figures PNG, tables CSV) sont disponibles dans le dossier 'output/'.")
print("--------------------------------------------------")
