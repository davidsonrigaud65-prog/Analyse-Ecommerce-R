
# 1. Évolution Temporelle : CA Mensuel sur 2 Ans

#  le dataframe 'ca_mensuel' et la conversion de 'annee_mois' en Date pour ggplot
ca_mensuel$date_mensuelle <- as.Date(paste0(ca_mensuel$annee_mois, "-01"))

plot_ca_mensuel <- ggplot(ca_mensuel, aes(x = date_mensuelle, y = CA)) +
  geom_line(color = "blue", linewidth = 1) + 
  geom_point(color = "orange", size = 2) + 
 geom_smooth(method = "loess", se = FALSE, color = "grey", linetype = "dashed") +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000, suffix = "K")) + 
  labs(
    title = " Graphique # 1 : Évolution du Chiffre d'Affaires Mensuel (2023-2024)",
    subtitle = "Forte saisonnalité observée en fin d'année",
    x = "Date",
    y = "Chiffre d'Affaires (K€)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


print(plot_ca_mensuel)

#____________________________________________________________________


# 2. Barplot : CA par Catégorie de Produits


# Calcul de 'part_de_marche_categorie'
part_de_marche_categorie <- d_c %>%
  group_by(categorie) %>%
  summarise(CA_Total = sum(montant_total, na.rm = TRUE)) %>%
  mutate(Part_CA = (CA_Total / sum(CA_Total)) * 100) %>%
  arrange(desc(Part_CA))

plot_ca_categorie <- ggplot(part_de_marche_categorie, aes(x = reorder(categorie, CA_Total), y = CA_Total, fill = categorie)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Part_CA, 1), "%")), hjust = -0.1, size = 3) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000000, suffix = "M€")) + 
  coord_flip() + 
  labs(
    title = " Graphique # 2 : Contribution du CA par Catégorie Principale",
    y = "Chiffre d'Affaires (M€)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(plot_ca_categorie)

#__________________________________________________________________________________



# 3. Carte géographique ou Barplot : CA par Région

ca_par_region_vendeur <- d_c %>%
  group_by(region) %>%
  summarise(CA_Total = sum(montant_total, na.rm = TRUE)) %>%
  arrange(desc(CA_Total))

plot_ca_region <- ggplot(ca_par_region_vendeur, aes(x = reorder(region, CA_Total), y = CA_Total, fill = region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::label_number(scale = 1/1000000, suffix = "M€")(CA_Total)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(scale = 1/1000000)) +
  labs(
    title = " Graphique # 3 : Performance du CA par Région de Vente",
    x = "Région",
    y = "Chiffre d'Affaires (M€)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

print(plot_ca_region)

#____________________________________________________________________________

# 4. Scatter plot : Délai vs CA

plot_delai_vs_ca <- ggplot(d_c, aes(x = montant_total, y = delai_livraison_jours)) +
  geom_point(alpha = 0.3, color = "orange") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  scale_x_continuous(labels = scales::dollar) + 
  labs(
    title = "Gaphique # 4 : Relation entre le Montant de la Transaction et le Délai de Livraison",
    x = "Montant Total de la Transaction (€)",
    y = "Délai de Livraison (Jours)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot_delai_vs_ca)
#___________________________________________________________________________



# 5. Heatmap : Ventes par Mois et Catégorie

# Création des variables pour le Heatmap
heatmap_data <- d_c %>%
  mutate(mois = format(date, "%m"),
         annee = format(date, "%Y")) %>%
  group_by(categorie, mois) %>%
  summarise(CA_Mois_Cat = sum(montant_total), .groups = 'drop')

# Définir l'ordre des mois pour la lisibilité
ordre_mois <- format(seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-01"), by = "month"), "%m")

plot_heatmap <- ggplot(heatmap_data, aes(x = factor(mois, levels = ordre_mois), y = categorie, fill = CA_Mois_Cat)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red", labels = scales::label_number(scale = 1/1000, suffix = "K")) +
  labs(
    title = " Graphique # 5 : Heatmap de la Saisonnalité du CA par Catégorie",
    x= "Mois",
    y = "Catégorie de Produit",
    fill = "CA (K€)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(plot_heatmap)
#_________________________________________________________________________


# 6. Graphique Créatif : Courbe de Lorenz 

#  dataframe de segmentation client 
df_clients_lorenz <- d_c %>%
  distinct(client_id, montant_depense_total) %>%
  arrange(montant_depense_total) %>%
  mutate(
    rank = row_number(),
    Pourcentage_Clients = rank / n(),
    Pourcentage_Depenses = cumsum(montant_depense_total) / sum(montant_depense_total)
  )

lorenz_line <- data.frame(x = c(0, 1), y = c(0, 1))

plot_lorenz <- ggplot(df_clients_lorenz, aes(x = Pourcentage_Clients, y = Pourcentage_Depenses)) +
  geom_line(color = "orange", linewidth = 1.2) + 
  geom_line(data = lorenz_line, aes(x = x, y = y), linetype = "dashed", color = "blue") + 
  labs(
    title = " Graphique # 6 : Courbe de Lorenz : Inégalité des Dépenses Clients",
    x = "Pourcentage Cumulé des Clients",
    y = "Pourcentage Cumulé du Chiffre d'Affaires (CA)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  coord_fixed() 

print(plot_lorenz)
#_____________________________________________________________


# 7. Histogramme : Distribution de l'Âge des Clients 

plot_histogramme_age <- d_c %>%
  distinct(client_id, age) %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = " Graphique # 7 : Distribution de l'Âge des Clients",
    x = "Âge (par tranches de 5 ans)",
    y = "Nombre de Clients"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot_histogramme_age)
#______________________________________________________________________________


# ENREGISTREMENT DES FIGURES DANS output/figures/

# Pour s'assurer que le dossier 'output/figures' existe.
if (!dir.exists("Outputs/Figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}

# 1. Évolution CA Mensuel sur 2 Ans 
ggsave(
  filename = "Outputs/Figures/01_evolution_ca.png", 
  plot = plot_ca_mensuel, 
  width = 10, 
  height = 6, 
  units = "in",
  dpi = 300 
)

# 2. Barplot : CA par Catégorie de Produits 
ggsave(
  filename = "Outputs/Figures/02_ca_categories.png", 
  plot = plot_ca_categorie, 
  width = 8, 
  height = 6, 
  units = "in",
  dpi = 300
)

# 3. Barplot : CA par Région 
ggsave(
  filename = "Outputs/Figures/03_ca_par_region.png", 
  plot = plot_ca_region, 
  width = 8, 
  height = 6, 
  units = "in",
  dpi = 300
)

# 4. Scatter Plot : Délai vs CA 
ggsave(
  filename = "Outputs/Figures/04_delai_vs_ca.png", 
  plot = plot_delai_vs_ca, 
  width = 9, 
  height = 7, 
  units = "in",
  dpi = 300
)

# 5. Heatmap : Ventes par Mois et Catégorie 
ggsave(
  filename = "Outputs/Figures/05_heatmap_ventes.png", 
  plot = plot_heatmap, 
  width = 12, 
  height = 7, 
  units = "in",
  dpi = 300
)

# 6.  Courbe de Lorenz 
ggsave(
  filename = "Outputs/Figures/06_courbe_lorenz.png", 
  plot = plot_lorenz, 
  width = 7, 
  height = 7, 
  units = "in",
  dpi = 300
)

# 7. Graphique Supplémentaire : Distribution de l'Âge des Clients 
ggsave(
  filename = "Outputs/Figures/07_histogramme_age.png", 
  plot = plot_histogramme_age, 
  width = 9, 
  height = 6, 
  units = "in",
  dpi = 300
)

print("Tous les 7 graphiques ont été exportés avec succès vers output/figures/")



















