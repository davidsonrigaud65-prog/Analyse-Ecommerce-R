
# a) Top 10 produits par Chiffre d'Affaires (CA)

top_10_Produits_ca <- d_c %>%
  group_by(nom_produit) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Total)) %>%
  slice_head(n = 10) 


# b) Top 10 produits par Volume (Quantité)

top10_Produits_volume <- d_c %>%
  group_by(nom_produit) %>%
  summarise(
    Quantite_Vendue = sum(quantite),
    .groups = 'drop'
  ) %>%
  arrange(desc(Quantite_Vendue)) %>%
  slice_head(n = 10)


# c) 10 produits les moins performants 

Produits10_moinsPerformants <- d_c %>%
  group_by(nom_produit) %>%
  summarise(
    CA_Total = sum(montant_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(CA_Total) %>% # Tri par ordre croissant (du plus faible au plus fort)
  slice_head(n = 10)


# d) Prix moyen par produit

prix_moyen_produit <- d_c %>%
  group_by(nom_produit) %>%
  summarise(
    Prix_Moyen = mean(prix_unitaire, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Prix_Moyen))


# e)  Impact des remises sur les ventes

impact_remise <- d_c %>%
  # Calculer du montant de la remise (Remise = Montant Brut - Montant Total)
  mutate(
    Montant_Brut = quantite * prix_unitaire,
    Montant_Remise = Montant_Brut - montant_total
  ) %>%
  # Creation de la variable de segmentation (Avec ou Sans Remise)
  mutate(
    Avec_Remise = ifelse(Montant_Remise > 0.01, "Oui", "Non") # Utilise 0.01 pour éviter les erreurs d'arrondi
  ) %>%
  #  Calcul des indicateurs pour chaque groupe
  group_by(Avec_Remise) %>%
  summarise(
    Volume_Transac = n(),
    CA_Total = sum(montant_total),
    Panier_Moyen = mean(montant_total),
    .groups = 'drop'
  ) %>%
  mutate(
    Part_Volume = (Volume_Transac / sum(Volume_Transac)) * 100
  )
#_____________________________________________________________




#  Catégories de Produits



# a) CA par catégorie principale

ca_par_categorie <- d_c %>%
  group_by(categorie) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Total))


# b) CA par sous-catégorie
# La variable sous_categorie est abscente dans la base, on ne peut pas effectuer des calcul avec


# c) Catégorie la plus rentable (par CA)

# En Utilisant le tableau calculé en (a) on a:
categorie_plus_rentable <- ca_par_categorie %>%
  slice_head(n = 1)


# d) Évolution par catégorie (2023 vs 2024)

evolution_categorie_annee <- d_c %>%
  group_by(categorie, annee) %>%
  summarise(
    CA = sum(montant_total),
    .groups = 'drop'
  ) %>%
  # Pivoter pour mettre 2023 et 2024 en colonnes
  pivot_wider(names_from = annee, values_from = CA, values_fill = 0) %>%
  # Calcul du Taux de Croissance
  mutate(
    Croissance = ((`2024` - `2023`) / `2023`) * 100,
    # Gérer les cas de division par zéro (si une catégorie n'existait pas en 2023)
    Croissance = ifelse(`2023` == 0, NA, Croissance)
  ) %>%
  arrange(desc(Croissance))


# e) Part de marché de chaque catégorie

#  Calcule du CA total global 
ca_total_global <- sum(d_c$montant_total)

#  Utilisation du tableau de CA par catégorie (calculé  a) pour ajouter le pourcentage
part_de_marche_categorie <- d_c %>%
  group_by(categorie) %>%
  summarise(
    CA_Total = sum(montant_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Calcul de la Part du CA (Part de marché)
    Part_CA = (CA_Total / ca_total_global) * 100
  ) %>%
  arrange(desc(Part_CA))
#____________________________________________________________


# Enregistrement de tableaux dans le fichier Tables de Outputs

exporter_resultat_csv(ca_par_categorie, "ca_par_categorie.csv")
exporter_resultat_csv(top_10_Produits_ca, "top_produits.csv")

