# Performance vendeurs

# Performance individuelle


# a) CA par vendeur

ca_par_vendeur <- d_c %>%
  group_by(nom_vendeur) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Total))


# b) Top 5 et Bottom 5 vendeurs (par CA)

#  En Utilisant le   tableau calculé en (a), on a :
top_5_vendeurs <- ca_par_vendeur %>%
  slice_head(n = 5)

bottom_5_vendeurs <- ca_par_vendeur %>%
  slice_tail(n = 5)


# c) Nombre de transactions par vendeur

volume_par_vendeur <- d_c %>%
  group_by(nom_vendeur) %>%
  summarise(
    Nb_Transactions = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(Nb_Transactions))


# d) Panier moyen par vendeur

panier_moyen_vendeur <- d_c %>%
  group_by(nom_vendeur) %>%
  summarise(
    Panier_Moyen = mean(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(Panier_Moyen))

#__________________________________________________________________


#  Performance par Région


# a) CA par région (du vendeur)

ca_par_region_vendeur <- d_c %>%
  group_by(region) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Total))


# b) Comparaison inter-régions (Atteinte Objectif)

comparaison_region <- d_c %>%
  group_by(region) %>%
  summarise(
    CA_Total = sum(montant_total),
    Objectif_Moyen_Mensuel = mean(objectif_mensuel),
    # Pour avoir l'objectif cumulé, on multiplie l'objectif mensuel * par le nombre de mois dans la période (24 mois = 2 ans)
    # Si la colonne 'objectif_mensuel' est le même pour tous les vendeurs d'une région:
    Objectif_Total = sum(distinct(., vendeur_id, objectif_mensuel)$objectif_mensuel) * 24, 
    .groups = 'drop'
  ) %>%
  # Calcul du Taux de Réalisation de l'Objectif
  mutate(
    Taux_Atteinte = (CA_Total / Objectif_Total) * 100
  ) %>%
  arrange(desc(Taux_Atteinte))


# c) Impact de l’ancienneté sur les performances

impact_anciennete <- d_c %>%

  group_by(vendeur_id, nom_vendeur, anciennete_mois) %>%
  summarise(
    CA_Total = sum(montant_total),
    Nb_Transactions = n(),
    Panier_Moyen = mean(montant_total),
    .groups = 'drop'
  ) %>%
  # Création des tranches d'ancienneté (pour mieux visualiser)
  mutate(
    Tranche_Anciennete = case_when(
      anciennete_mois < 12 ~ "Moins d'un an",
      anciennete_mois >= 12 & anciennete_mois < 36 ~ "1 à 3 ans",
      TRUE ~ "3 ans et plus"
    )
  ) %>%
  # Regrouper par tranche d'ancienneté pour l'analyse
  group_by(Tranche_Anciennete) %>%
  summarise(
    CA_Moyen_Vendeur = mean(CA_Total, na.rm = TRUE),
    Panier_Moyen_Global = mean(Panier_Moyen, na.rm = TRUE),
    Nb_Vendeurs = n_distinct(vendeur_id),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Moyen_Vendeur))

#___________________________________________________________________







