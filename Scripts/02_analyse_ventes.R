# GESTION DES DATES 
d_c$date <- as.Date(d_c$date)
d_c$annee <- format(d_c$date, "%Y")
d_c$mois <- format(d_c$date, "%m")
d_c$trimestre <- quarters(d_c$date)
d_c$annee_mois <- format(d_c$date, "%Y-%m")




#  Vue d'ensemble des ventes


# a) CA total sur la periode 2023-2024
ca_total <- sum(d_c$montant_total)

# b)  CA par année (2023 vs 2024)
ca_annuel <- d_c %>%
  group_by(annee) %>%
  summarise(CA = sum(montant_total))

# c)  CA par mois selon l'annee 
ca_mensuel_23_24 <- d_c %>%
  group_by(annee_mois) %>%
  summarise(CA = sum(montant_total))

# CA Trimestriel par Année

ca_trimestriel_annee <- d_c %>%
  group_by(annee, trimestre) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop' 
  ) %>%
  arrange(annee, trimestre)

# e) Calcul du Taux de croissance annuel
ca_2023 <- ca_annuel$CA[ca_annuel$annee == "2023"]
ca_2024 <- ca_annuel$CA[ca_annuel$annee == "2024"]

taux_croissance_annuel <-( (ca_2024 - ca_2023) / ca_2023) * 100

#____________________________________________________________

# Volume des transactions

# a) Nombre total de transactions (volume)
nb_transac_total <- nrow(d_c)

# b) Nombre de transactions par mois
transactions_mensuelles <- d_c %>%
  group_by(annee_mois) %>%
  summarise(
    Nb_Transactions = n(), 
    .groups = 'drop'      
  ) %>%
  arrange(annee_mois)

# c) Panier moyen
panier_moyen_global <- mean(d_c$montant_total)

# d) Distribution des montants de transactions
stats_montant <- summary(d_c$montant_total)

#____________________________________________________________

# Statuts et moyens de paiement


# a) Répartition par statut  

repartition_statut <- d_c %>%
  group_by(statut_livraison) %>%
  summarise(
    Volume = n(),
    Proportion_Volume = (n() / nb_transac_total) * 100,
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(Volume))



# b) Taux d’annulation

taux_annulation <- repartition_statut$Proportion_Volume[repartition_statut$statut_livraison == "Annulé"]



# c) Répartition par moyen de paiement 

nb_transac_total <- nrow(d_c)

repartition_moyen_paiement <- d_c %>%
  group_by(methode_paiement) %>%
  summarise(
    Volume_Transac = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Calcul du pourcentage du volume total de transactions
    Part_Volume = (Volume_Transac / sum(Volume_Transac)) * 100
  ) %>%
  arrange(desc(Volume_Transac))


# d) CA par moyen de paiement

repartition_ca_paiement <- d_c %>%
  group_by(methode_paiement) %>%
  summarise(
    CA_Genere = sum(montant_total),
  ) %>%
  arrange(desc(CA_Genere))

#_____________________________________________________________


# Enregistreemnt des tableaux dans le fichier Tables

exporter_resultat_csv(ca_mensuel, "ca_mensuel_agg.csv")
exporter_resultat_csv(ca_annuel, "croissance_annuelle.csv")

