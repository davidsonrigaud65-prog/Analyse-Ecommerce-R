# FONCTIONS UTILITAIRES POUR LE PROJET D'ANALYSE 


# 1. Fonction pour calculer le CA par une variable donnée 

# Calcule le Chiffre d'Affaires (CA) total par un groupe donné.
#' @param df Le dataframe de transactions (d_c).
#' @param groupe La colonne par laquelle agréger (en tant que chaîne de caractères, ex: "categorie").
#' @return Un dat'aframe agrégé avec la colonne CA_Total.
#' 
calculer_ca_par_groupe <- function(df, groupe) {
  df %>%
    group_by(across(all_of(groupe))) %>% # Utiliser across(all_of()) pour gérer la variable de chaîne
    summarise(
      CA_Total = sum(montant_total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(CA_Total))
}


## 2. Fonction pour créer un Barplot de CA 

#' Crée un Barplot horizontal du CA agrégé par une variable.
#' @param df_agg Le dataframe agrégé par la fonction calculer_ca_par_groupe.
#' @param titre Le titre du graphique.
#' @param sous_titre Le sous-titre du graphique.
#' @param x_var Le nom de la colonne du groupe (en tant que chaîne de caractères, ex: "categorie").
#' @return Un objet ggplot.
creer_barplot_ca <- function(df_agg, titre, sous_titre, x_var) {
  
  # Assurer que la colonne x_var est traitée comme un facteur ordonné
  df_agg <- df_agg %>%
    mutate(!!sym(x_var) := factor(!!sym(x_var))) 
  
  ggplot(df_agg, aes(x = reorder(!!sym(x_var), CA_Total), y = CA_Total, fill = !!sym(x_var))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::label_number(scale = 1/1000000, suffix = "M€")(CA_Total)), 
              hjust = -0.1, size = 3) +
    scale_y_continuous(labels = scales::label_number(scale = 1/1000000)) +
    coord_flip() + 
    labs(
      title = titre,
      subtitle = sous_titre,
      x = stringr::str_to_title(gsub("_", " ", x_var)), 
      y = "Chiffre d'Affaires (M€)"
    ) +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
}


## 3. Fonction pour exporter un tableau en CSV

#' Exporte un dataframe vers un fichier CSV dans le dossier output/tables.
#' @param df Le dataframe à exporter.
#' @param nom_fichier Le nom du fichier de sortie (ex: "ca_par_segment.csv").
exporter_resultat_csv <- function(df, nom_fichier) {
  chemin_complet <- file.path("Outputs", "Tables", nom_fichier)
  # Créer le dossier s'il n'existe pas
  if (!dir.exists(file.path("Outputs", "Tables"))) {
    dir.create(file.path("Outputs", "Tables"), recursive = TRUE)
  }
  write.csv(df, chemin_complet, row.names = FALSE)
  print(paste("Fichier exporté avec succès :", chemin_complet))
}