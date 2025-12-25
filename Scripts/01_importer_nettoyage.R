# Importation des donnees
 
 transactions <- read.csv("C:\\Users\\DELL\\Desktop\\Formation Data-science_FRST_FDS\\Phase 1\\Introuduction R_Stat Descriptives\\mon_projet_2\\Data\\raw\\transactions.csv")
 clients <- read.csv("C:\\Users\\DELL\\Desktop\\Formation Data-science_FRST_FDS\\Phase 1\\Introuduction R_Stat Descriptives\\mon_projet_2\\Data\\raw\\clients.csv")
 vendeurs <- read.csv("C:\\Users\\DELL\\Desktop\\Formation Data-science_FRST_FDS\\Phase 1\\Introuduction R_Stat Descriptives\\mon_projet_2\\Data\\raw\\vendeurs.csv")
 produits <- fromJSON("C:\\Users\\DELL\\Desktop\\Formation Data-science_FRST_FDS\\Phase 1\\Introuduction R_Stat Descriptives\\mon_projet_2\\Data\\raw\\produits.json")
 
 # vue des donnees
 View(transactions)
 View(clients)
 View(vendeurs)
 
 
 # Fusion des tables
 
 donnees_completes <- transactions %>%
   left_join(clients, by = "client_id") %>% 
   left_join(vendeurs, by = "vendeur_id") %>% 
   left_join(produits, by = "produit_id")

d_c<-donnees_completes

View(d_c)

#_________________________________________________________________

# PROCESSUS DE NETTOYAGES DES DONNEES



# Structure de la table fusionee
str(d_c)

# resumes statistiques
summary(d_c)

# nombre de valeurs manquantes par colonnes

colSums(is.na(d_c))


# mediane de delai_livraison
mediane_delai <- median(d_c$delai_livraison_jours, na.rm = TRUE)

# Imputation des valeurs manquantes de delai_livraison par la mediane
d_c <- d_c %>%
  mutate(delai_livraison_jours = 
           ifelse(is.na(delai_livraison_jours), mediane_delai, delai_livraison_jours))


colSums(d_c$delai_livraison_jours)

# Convertir les variables date et date_inscription en format date

# 1. date de la transaction
d_c$date <- as.Date(d_c$date) 

# 2. date_inscription des clients
d_c$date_inscription <- as.Date(d_c$date_inscription)

#____________________________________________________________

# Sauvegarde de ma base nettoyee

Donees_clean<-d_c

# POur le repertoire de travail
setwd("C:/Users/DELL/Desktop/Formation Data-science_FRST_FDS/Phase 1/Introuduction R_Stat Descriptives/mon_projet_2")

#  Sauvegarde de la base nettoyée
write.csv(d_c, "Data/processed/Donees_clean.csv", row.names = FALSE)




