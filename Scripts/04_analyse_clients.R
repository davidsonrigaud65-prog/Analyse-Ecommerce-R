#  Analyse Clients


# Segmentation

# CA par segment clients(premium/Standard/Occasionnel)

# Détermination des seuils 
seuils <- quantile(d_c$montant_depense_total, probs = c(0.25, 0.75), na.rm = TRUE)
Q1 <- seuils[1] 
Q3 <- seuils[2] 

# Création d'un dataframe client pour la segmentation 
df_clients_segmentation <- d_c %>%
  distinct(client_id, montant_depense_total) %>%
  mutate(
    # Segmentation basée sur le montant total dépensé
    segment_client = case_when(
      montant_depense_total >= Q3 ~ "Premium",       
      montant_depense_total < Q1 ~ "Occasionnel",   
      TRUE ~ "Standard"                             
    )
  ) %>%
  select(client_id, segment_client)

#  Joindre le segment aux données de transaction (d_c)
d_c_segment <- d_c %>%
  left_join(df_clients_segmentation, by = "client_id")

# Ca par segment client
ca_par_segment <- d_c_segment %>%
  group_by(segment_client) %>%
  summarise(
    CA_Total_Segment = sum(montant_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Total_Segment))


# b) Nombre de clients par segment

nb_clients_par_segment <- df_clients_segmentation %>%
  group_by(segment_client) %>%
  summarise(
    Nb_Clients = n(),
    .groups = 'drop'
  )


# c) CA moyen par client et par segment (CODE CORRECT)

ca_moyen_par_client_segment <- df_clients_segmentation %>% 
  group_by(segment_client) %>%
  summarise(

    CA_Moyen_Par_Client = mean(d_c$montant_depense_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Moyen_Par_Client))


# d) Fréquence d’achat par segment

frequence_segment <- d_c_segment %>%
  group_by(segment_client) %>%
  summarise(
    Nb_Transactions = n(),                  
    Nb_Clients = n_distinct(client_id),     
    .groups = 'drop'
  ) %>%
  mutate(
    # Fréquence = Total Transactions / Total Clients
    Frequence_Achat_Moyenne = Nb_Transactions / Nb_Clients
  ) %>%
  arrange(desc(Frequence_Achat_Moyenne))

#____________________________________________________________


# Demographie et geographie


# a) CA par région

ca_par_region <- d_c %>%
  group_by(region) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Total))


# b) CA par tranche d’âge

# Création des tranches d'âge 
tranches_age <- d_c %>%
  mutate(
    tranche_age = case_when(
      age < 30 ~ "Moins de 30 ans",
      age >= 30 & age < 45 ~ "30-44 ans",
      age >= 45 & age < 60 ~ "45-59 ans",
      TRUE ~ "60 ans et plus"
    )
  ) %>%
  group_by(tranche_age) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  mutate(tranche_age = factor(tranche_age, levels = c("Moins de 30 ans", "30-44 ans", "45-59 ans", "60 ans et plus"))) %>%
  arrange(tranche_age)


# c) Top 10 villes par CA

top_10_villes_ca <- d_c %>%
  group_by(ville) %>%
  summarise(
    CA_Total = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(desc(CA_Total)) %>%
  slice_head(n = 10)


# d) Profil du client type 

profil_client <- d_c %>%
  distinct(client_id, .keep_all = TRUE) %>% 
  summarise(
    Age_Moyen = mean(age),
    Genre_Majoritaire = names(sort(table(genre), decreasing = TRUE)[1]), # Genre le plus fréquent
    Region_Majoritaire = names(sort(table(region), decreasing = TRUE)[1]), # Région la plus fréquente
    Panier_Moyen_Client = mean(montant_depense_total, na.rm = TRUE),
    .groups = 'drop'
  )


# e) Clients les plus actifs 

top_20_actifs <- d_c %>%
  group_by(client_id, prenom, nom) %>%
  summarise(
    Nb_Transactions = n(),
    CA_Total_Client = sum(montant_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Nb_Transactions), desc(CA_Total_Client)) %>%
  slice_head(n = 20)

#__________________________________________________________________


# Enregistrement des tableaux csv dan sle fichier Tables de Outputs
exporter_resultat_csv(ca_par_segment, "ca_par_segment.csv")
exporter_resultat_csv(profil_client, "profil_demographique.csv")

