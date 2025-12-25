
# Prediction

# Agrégation du CA par Mois 

ca_mensuel <- d_c %>%
  group_by(annee_mois) %>%
  summarise(
    CA = sum(montant_total),
    .groups = 'drop'
  ) %>%
  arrange(annee_mois)
#_______________________________________________________________________


# a) Prédiction CA pour le trimestre suivant 

# Déterminons l'année/mois de début
start_date <- as.numeric(c(substr(min(ca_mensuel$annee_mois), 1, 4), substr(min(ca_mensuel$annee_mois), 6, 7)))

ca_ts <- ts(ca_mensuel$CA, start = start_date, frequency = 12)

#  Construction du Modèle ARIMA Automatique
modele_arima <- auto.arima(ca_ts)

#  Prédiction pour les 3 prochains mois 
prediction_trimestre_suivant <- forecast(modele_arima, h = 3)



# b) Identification des tendances saisonnières

decomposition_saisonniere <- decompose(ca_ts, type = "multiplicative")


# c) Prévision par catégorie 

# Nous devons choisir la catégorie avec le CA le plus élevé 

top_categorie <- "Smartphones"

# 1. Préparation de la série temporelle pour la Top Catégorie
ca_cat_mensuel <- d_c %>%
  filter(categorie == top_categorie) %>%
  group_by(annee_mois) %>%
  summarise(CA = sum(montant_total, na.rm = TRUE), .groups = 'drop') %>%
  arrange(annee_mois)

ca_cat_ts <- ts(ca_cat_mensuel$CA, start = start_date, frequency = 12)

# 2. Application du modèle 
modele_holt <- ets(ca_cat_ts)
prediction_cat_trimestre <- forecast(modele_holt, h = 3)


# d) Recommandations stratégiques basées sur les tendances
# ces lignes de codes , en ,ettant evidance le facteur > ou < 1 , nous permetra de recommender les mois les plus strategiques pour la production facteur > 1
# Les mois les plus forts et les plus faibles (pour la saisonnalité)
saisonnalite_moyenne <- data.frame(
  Mois = month.abb,
  Facteur_Saison = as.numeric(decomposition_saisonniere$seasonal[1:12])
) %>%
  arrange(desc(Facteur_Saison))

print("d) Mois les Plus Saisonniers (Facteur > 1 = Fort) :")
print(saisonnalite_moyenne)

# Information 2 : La tendance générale du CA
print(paste("Tendance Linéaire de la Série (Pente Positive = Croissance) :", round(as.numeric(coef(lm(ca_ts ~ time(ca_ts)))[2]), 2)))


#_______________________________________________________________

# Enregistrement de tableau csv dans le fichier Tables de Outputs
exporter_resultat_csv(prediction_trimestre_suivant, "prediction_trimestre.csv")
