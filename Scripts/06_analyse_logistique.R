#  Logistique 

# Delai de livraison
#______________________________________________________________________


# a) Délai moyen de livraison

delai_moyen_global <- d_c %>%
  summarise(
    Delai_Moyen_Jours = mean(delai_livraison_jours)
  )


# b) Distribution des délais (Table de Fréquence)

distribution_delais <- d_c %>%
  group_by(delai_livraison_jours) %>%
  summarise(
    Nb_Transactions = n(),
    .groups = 'drop'
  ) %>%
  arrange(delai_livraison_jours)


# c) Délais par région (du vendeur)

delais_par_region <- d_c %>%
  group_by(region) %>%
  summarise(
    Delai_Moyen_Jours = mean(delai_livraison_jours),
    .groups = 'drop'
  ) %>%
  arrange(desc(Delai_Moyen_Jours)) 


# d) Délais par catégorie de produit

delais_par_categorie <- d_c %>%
  group_by(categorie) %>%
  summarise(
    Delai_Moyen_Jours = mean(delai_livraison_jours),
    .groups = 'drop'
  ) %>%
  arrange(desc(Delai_Moyen_Jours))


# e) Identification des problèmes logistiques (Transactions lentes)

seuil_probleme <- 5

problemes_logistiques <- d_c %>%
  filter(delai_livraison_jours > seuil_probleme | statut_livraison == "En cours" | statut_livraison == "Annulé") %>%
  group_by(statut_livraison) %>%
  summarise(
    Nb_Cas = n(),
    Pourcentage_CA = (sum(montant_total) / sum(d_c$montant_total)) * 100,
    Delai_Moyen_Cas = mean(delai_livraison_jours),
    .groups = 'drop'
  ) %>%
  arrange(desc(Nb_Cas))

#____________________________________________________________________


# Qualite de service


# a) Taux livraison reussite

# Définir le statut "Réussie"
statuts_succes <- c("Livré", "Terminé")

taux_succes <- d_c %>%
  summarise(
    Nb_Transactions_Totales = n(),
    Nb_Reussies = sum(statut_livraison %in% statuts_succes),
    Taux_Succes = (Nb_Reussies / Nb_Transactions_Totales) * 100
  )


# b) Corrélation délai / annulations

# Création la variable binaire 
data_correlation <- d_c %>%
  mutate(
    Est_Annule = ifelse(statut_livraison %in% c("Annulé", "Échec"), 1, 0)
  )

# Calcul de la corrélation
correlation_delai_annulation <- cor(
  data_correlation$delai_livraison_jours,
  data_correlation$Est_Annule,
  use = "complete.obs" # Gérer les valeurs manquantes
)


# c) Recommandations d’optimisation (Tableau de Synthèse)

# Création d'un tableau de synthèse pour identifier les priorités 
synthese_optimisation <- d_c %>%
  group_by(region, categorie) %>%
  summarise(
    Delai_Moyen = mean(delai_livraison_jours),
    Nb_Annulations = sum(statut_livraison %in% c("Annulé", "Échec")),
    .groups = 'drop'
  ) %>%
  # Calculel d'un score de gravité (Délai x Annulation) pour la priorisation
  mutate(
    Score_Probleme = Delai_Moyen * Nb_Annulations
  ) %>%
  arrange(desc(Score_Probleme)) %>%
  slice_head(n = 5) 

#___________________________________________________________________________


# Enregistrement de stableaux csv dans le fichier Tables de Outputs

exporter_resultat_csv(delais_par_region, "delais_par_region.csv")


