# deploy_production.R
# Objectif : Exécuter le script SQL de déploiement sur la base de production.

# --- Chargement des librairies ---
library(DBI)
library(RPostgres)
library(readr)
library(dotenv)
library(here)

# --- Configuration ---
load_dot_env(file = here::here(".Renviron")) 

# --- Connexion et exécution ---
cat("Connexion à la base de données de production...\n")
con_prod <- NULL

tryCatch({
  
  con_prod <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS")
  )
  
  # 1. Lire le contenu du fichier SQL
  sql_content <- readr::read_file(here::here("utils", "deploy.sql"))
  
  # 2. Séparer le contenu en commandes individuelles
  #    On sépare par ';' suivi d'un éventuel retour à la ligne.
  commands <- strsplit(sql_content, ";\\s*\\n")[[1]]
  
  # 3. Démarrer la transaction
  dbBegin(con_prod)
  
  cat("Exécution des commandes de déploiement dans une transaction...\n")
  
  # 4. Exécuter chaque commande individuellement
  for (cmd in commands) {
    # Ignorer les lignes vides ou les commentaires
    if (nchar(trimws(cmd)) > 0 && !startsWith(trimws(cmd), "--")) {
      cat(paste(" -> Exécution:", substr(cmd, 1, 70), "...\n"))
      dbExecute(con_prod, cmd)
    }
  }
  
  # 5. Si tout s'est bien passé, valider la transaction
  dbCommit(con_prod)
  
  cat("\n✅ Déploiement terminé et transaction validée avec succès !\n")
  
}, error = function(e) {
  cat("\nERREUR : Le déploiement a échoué. Annulation de la transaction (ROLLBACK).\n")
  print(e$message)
  
  # 6. En cas d'erreur, annuler la transaction
  if (!is.null(con_prod) && dbIsValid(con_prod)) {
    dbRollback(con_prod)
  }
  
}, finally = {
  if (!is.null(con_prod) && dbIsValid(con_prod)) {
    cat("Fermeture de la connexion.\n")
    dbDisconnect(con_prod)
  }
})